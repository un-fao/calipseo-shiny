#artfish_fishing_unit_server
artfish_fishing_unit_server <- function(id, pool, reloader){
  
  moduleServer(id, function(input, output, session){   
  
  ns<-session$ns
  
  bg_summary<-reactiveVal(NULL)
  bg_rank<-reactiveVal(NULL)
  sp_summary<-reactiveVal(NULL)
  sp_rank<-reactiveVal(NULL)
  
  indicator_color<-reactiveVal(NULL)
  indicator_icon<-reactiveVal(NULL)
  indicator_unit<-reactiveVal(NULL)
  indicator_label<-reactiveVal(NULL)
  

  ref_species<-accessRefSpecies(pool)
  ref_fishing_units<-accessRefFishingUnits(pool)
  
  files<-getStatPeriods(config=appConfig, "artfish_estimates",target = "release")
  
  output$no_release<-renderUI({
    div(
      if(nrow(files)>0){
        NULL
      }else{
        p(i18n("NO_RELEASE"))
      }
    )
  })
  
  req(nrow(files)>0)
  
  estimate<-do.call(rbind,lapply(files$file, readr::read_csv))
  
  output$year_selector<-renderUI({
    
    years<-sort(unique(estimate$year),decreasing = T)
    
    selectizeInput(ns("year"),paste0(i18n("SELECT_INPUT_TITLE_YEAR")," :"),choices=years,multiple = F,selected=years[1])
  })  
  
  output$fishing_unit_selector<-renderUI({
    
    req(input$year)
    
    selection<-subset(estimate,year==input$year)
    bg_sp<-unique(selection$fishing_unit)
    
    ref_bg_sp<-subset(ref_fishing_units,ID %in% bg_sp)
    
    bg<-setNames(c(0,ref_bg_sp$ID),c(i18n("ALL_FISHING_UNITS_LABEL"),ref_bg_sp$NAME))
    
    selectizeInput(ns("bg"),paste0("SELECT_INPUT_TITLE_FISHING_UNIT"," :"),choices=bg,multiple = F,selected=bg[1])
  })
  
  output$unit_selector<-renderUI({
    
    req(input$indicator)
    
    selection<-switch(input$indicator,
                      "effort"=setNames("base",i18n("DAYS_LABEL")),
                      "catch"=setNames(c("base","K"),c(i18n("KILOGRAM_LABEL"),i18n("TONNE_LABEL"))),
                      "value"=setNames(c("base","K","M"),c(i18n("DOLLAR_LABEL"),i18n("THOUSAND_DOLLAR_LABEL"),i18n("MILLION_DOLLAR_LABEL"))),
                      "cpue"=setNames("base",i18n("CPUE_KILOGRAM_DAY_LABEL"))
                      
    )
    
    selectizeInput(ns("unit"),paste0(i18n("SELECT_INPUT_TITLE_UNIT")," :"),choices=selection,multiple = F,selected=selection[1])
  })
  
  output$selectors<-renderUI({
    tagList(
      fluidRow(
        column(6,
               uiOutput(ns("year_selector")),
               uiOutput(ns("fishing_unit_selector"))
        ),
        column(6,
               selectizeInput(ns("indicator"),paste0(i18n("SELECT_INPUT_TITLE_INDICATOR")," :"),choices=setNames(c("effort","catch","value","cpue"),
                                                                                                                   c(i18n("EFFORT_LABEL"),i18n("CATCH_LABEL"),i18n("VALUE_LABEL"),i18n("CPUE_LABEL"))),
                                                                                                                   multiple = F),
               uiOutput(ns("unit_selector"))
        )
      )
    )
  })
  
  #Fishing Units Plots and Tables
  
  observe({
    req(input$year)
    req(input$bg)
    req(input$indicator)
    req(input$unit)
    
    value<-switch(input$indicator,
                  "effort"="effort_nominal",
                  "catch"="catch_nominal_landed",
                  "value" = "trade_value",
                  "cpue" = "catch_cpue"
    )
    
    table<-artfish_year_summary(
      data = estimate, target_year = input$year,
      variable = "fishing_unit", value = value,
      value_fun = if(input$indicator == "effort") unique else sum
    )
    
    summary<-table$summary%>%
      merge(ref_fishing_units%>%select(ID,NAME)%>%mutate(ID=as.character(ID))%>%rename('fishing_unit'='ID'))%>%
      relocate(NAME)%>%
      mutate(target=ifelse(fishing_unit == input$bg,"target","other"))%>%
      select(-fishing_unit)%>%
      mutate(NAME=ifelse(is.na(NAME),"Total",NAME))%>%
      rename(`Fishing Unit`=NAME)
    
    rank<-table$rank%>%
      merge(ref_fishing_units%>%select(ID,NAME)%>%mutate(ID=as.character(ID))%>%rename('fishing_unit'='ID'))%>%
      relocate(NAME)%>%
      mutate(target=ifelse(fishing_unit == input$bg,"target","other"))%>%
      select(-fishing_unit)%>%
      mutate(NAME=ifelse(is.na(NAME),"Total",NAME))%>%
      rename(`Fishing Unit`=NAME,
             Rank = rank,
             Percentage = percent,
             `Cumulative percentage` = cum_percent)
    
    if(input$unit=="K"){
      summary<-summary%>%
        mutate(across(where(is.double), ~.x/1000))
      rank<-rank%>%
        mutate(Total = Total/1000)
    }
    
    if(input$unit=="M"){
      summary<-summary%>%
        mutate(across(where(is.double), ~.x/1000000))
      rank<-rank%>%
        mutate(Total = Total/1000000)
    }
    
    bg_summary(summary)
    bg_rank(rank)
    
  })
  
  observe({
    req(input$indicator)
    req(input$unit)
    
    switch(input$indicator,
           "effort"={
             indicator_color("purple")
             indicator_icon("fas fa-clock")
             indicator_unit(i18n("DAYS_LABEL"))
             indicator_label(i18n("INDICATOR_LABEL_TOTAL_EFFORT"))
             
           },
           "catch"={
             indicator_color("orange")
             indicator_icon("fas fa-fish")
             if(input$unit=="base"){indicator_unit("kg")}else{indicator_unit("t")}
             indicator_label(i18n("INDICATOR_LABEL_TOTAL_CATCH"))
           },
           "value" = {
             indicator_color("blue")
             indicator_icon("fas fa-dollar-sign")
             if(input$unit=="base"){indicator_unit("$")}else if(input$unit=="K"){indicator_unit("$K")}else{indicator_unit("$M")}
             indicator_label(i18n("INDICATOR_LABEL_TOTAL_VALUE"))
           },
           "cpue" = {
             indicator_color("red")
             indicator_icon("fas fa-ship")
             indicator_unit("kg/day")
             indicator_label(i18n("INDICATOR_LABEL_TOTAL_CPUE"))
           })
  })
  
  output$value<-renderUI({
    req(input$indicator)
    req(input$unit)
    if(!is.null(bg_summary())){
      if(input$bg!="0"){
        value<-subset(bg_summary(),target=="target")$Total
      }else{
        value<- bg_summary() %>%
          summarise(across(where(is.numeric), sum)) %>%
          mutate(`Fishing Unit` = "Total",
                 target="other")%>%
          pull(Total)
      }
      valueBox(value=tags$p(sprintf("%s (%s)",indicator_label(),indicator_unit()),style="font-size: 40%"), subtitle=round(value,2), icon = tags$i(class = indicator_icon(), style="font-size: 30px"), width = 12,color = indicator_color() )
    }
  })
  
  output$accuracy<-renderPlotly({
    req(input$year)
    req(input$bg)
    
    accuracy<-subset(estimate,year==input$year)
    if(input$bg!="0"){
      accuracy<-subset(accuracy,fishing_unit==input$bg)
    }
    accuracy<-mean(accuracy$overall_accuracy*100,na.rm=T)
    
    plot_ly(
      domain = list(x = c(0.20, 0.80), y = c(0, 0.90)),
      value = accuracy,
      title = list(text = paste0(i18n("GAUGE_TITLE_ACCURACY")," (%)")),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis =list(range = list(NULL,100)),
        bar = list(color = "grey"),
        steps = list(
          list(range = c(0, 90), color = "#ffc163"),
          list(range = c(90, 100), color = "#cbe261"))
      )) %>%
      layout(height=200,
             margin = list(l=20,r=30,0,0),
             plot_bgcolor  = "rgba(0, 0, 0, 0)",
             paper_bgcolor = "rgba(0, 0, 0, 0)")
    
  })
  
  output$bg_sum_table<-DT::renderDT(server = FALSE, {
    
    if(!is.null(bg_summary())){
    df <- bg_summary()
    df_total <- df %>%
      summarise(across(where(is.numeric), ~sum(.,na.rm=T))) %>%
      mutate(`Fishing Unit` = "Total")
    
    df <- bind_rows(df, df_total)
    
    
      DT::datatable(
        class = 'cell-border stripe',
        df,
        extensions = c("Buttons"),
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 'Bt',
          scrollX=TRUE,
          pageLength=nrow(df),
          columnDefs = list(list(visible=FALSE, targets=c(14))),
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  sprintf("%s_summary",input$year), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  sprintf("%s_summary",input$year), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',filename = sprintf("%s_summary",input$year),
                 title = sprintf("%s_summary",input$year), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          )
        )
      )%>%
        formatStyle(columns = c("Fishing Unit"), `text-align` = 'right')%>%
        formatStyle(columns = c("Total"), fontWeight = 'bold', `text-align` = 'left')%>%
        formatRound(2:14,digits=1)%>%
        formatStyle("target",target = 'row',backgroundColor = styleEqual(c("target"), c("orange")))
    }
    
  })
  
  output$bg_rank_table<-DT::renderDT(server = FALSE, {
    
    if(!is.null(bg_rank())){
      DT::datatable(
        class = 'cell-border stripe',
        bg_rank(),
        extensions = c("Buttons"),
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 'Bt',
          scrollX=TRUE,
          pageLength=nrow(bg_rank()),
          columnDefs = list(list(visible=FALSE, targets=c(5))),
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  sprintf("%s_rank",input$year), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  sprintf("%s_rank",input$year), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',filename = sprintf("%s_rank",input$year),
                 title = sprintf("%s_rank",input$year), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          )
        )
      )%>%
        formatStyle(columns = c("Fishing Unit"), `text-align` = 'right')%>%
        formatRound(3,digits=1)%>%
        formatPercentage(4:5,digits = 1)%>%
        formatStyle(
          'Percentage',
          background = styleColorBar(c(0,1), "#029bef",-90),
          backgroundSize = '90% 80%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'left'
        )%>%
        formatStyle("target",target = 'row',backgroundColor = styleEqual(c("target"), c("orange")))
    }
    
  })
  
  output$bg_rank_plot<-renderPlotly({
    
    if(!is.null(bg_rank())){
      rank<-bg_rank()
      
      value<-switch(input$indicator,
                    "effort"="effort_nominal",
                    "catch"="catch_nominal_landed",
                    "value" = "trade_value",
                    "cpue" = "catch_cpue"
      )
      
      if(input$bg!="0"){
        rank%>%
          plot_ly(y = ~Rank, x = ~Total, type = "bar",  orientation = "h",color=~factor(target),colors = c("grey","orange"),text=~`Fishing Unit`,hovertemplate =~ sprintf("</br>%s[Rank %s]</br>%s:%s</br>Percentage:%s%%",`Fishing Unit`,Rank,input$indicator,round(Total,1),round(Percentage*100,1)),
                  textposition = "auto",textfont = list(color = "black")) %>%
          layout(showlegend = FALSE,
                 uniformtext=list(minsize=8, mode='show'),
                 yaxis = list(title=NULL,autorange = "reversed",tickmode = "array", tickvals = unique(rank$Rank), ticktext = unique(rank$Rank)),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,title = FALSE),
                 plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)")
      }else{
        rank%>%
          plot_ly(y = ~Rank, x = ~Total, type = "bar",  orientation = "h",hoverinfo='none',marker = list(color = ~Rank, colorscale = list(c(0, 1), c("#029bef", "#84fcfc")), showscale = FALSE),text = ~`Fishing Unit`,hovertemplate =~ sprintf("</br>%s[Rank %s]</br>%s:%s</br>Percentage:%s%%",`Fishing Unit`,Rank,input$indicator,round(Total,1),round(Percentage*100,1)),
                  textposition = "auto",textfont = list(color = "black")) %>%
          layout(showlegend = FALSE,
                 uniformtext=list(minsize=8, mode='show'),
                 yaxis = list(title=NULL,autorange = "reversed",tickmode = "array", tickvals = unique(rank$Rank), ticktext = unique(rank$Rank)),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,title = FALSE),
                 plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)")
        
      }
    }
    
  })
  
  output$bg_sum_plot<-renderPlotly({
    
    if(!is.null(bg_summary())){
      data_plot<-bg_summary()
      
      data_plot<-data_plot%>%
        select(-Total)
      
      if(input$bg!="0"){
        data_plot<-data_plot%>%
          filter(target == "target")
      }else if(input$bg_level=="global"){
        print(data_plot)
        
        data_plot <- data_plot %>%
          summarise(across(where(is.numeric), ~sum(.,na.rm=T))) %>%
          mutate(`Fishing Unit` = "Total",
                 target="other")%>%
          ungroup()
        
      }else{
        data_plot<-data_plot%>%
          filter(`Fishing Unit` != "Total")
      }
      
      data_plot<-data_plot%>%
        select(-target)%>%
        pivot_longer(!`Fishing Unit`, names_to = "month", values_to = "value")
      
      if(input$bg_level=="global"){
        p<-data_plot%>%plot_ly(x = ~month,y =~ value,color= ~`Fishing Unit`, colors=indicator_color(),type = 'scatter', mode = 'lines+markers',fill = 'tozeroy',line = list(shape = "spline"),text = ~sprintf("%s[%s]: %s",`Fishing Unit`,month,round(value,1)),name=input$indicator)
      }else{
        p<-data_plot%>%plot_ly(x = ~month,y =~ value,color= ~`Fishing Unit`, type = 'scatter', mode = 'lines+markers',line = list(shape = "spline"),text = ~sprintf("%s[%s]: %s",`Fishing Unit`,month,round(value,1))) 
      }
      p%>%layout(
        hovermode ='closest',
        xaxis = list(
          fixedrange = TRUE,
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          zeroline = F,
          tickmode = "array", tickvals = unique(data_plot$month), ticktext = unique(data_plot$month)
        ),
        yaxis = list(
          tickfont = list(size = 10),
          title = sprintf("%s (%s)",indicator_label(),indicator_unit()),
          zeroline = F
        )
      )
    }
    
  })
  
  
  
  #Species Plots and Tables
  
  observe({
    req(input$year)
    req(input$bg)
    req(input$indicator)
    req(input$unit)
    
    value<-switch(input$indicator,
                  "effort"="effort_nominal",
                  "catch"="catch_nominal_landed",
                  "value" = "trade_value",
                  "cpue" = "catch_cpue"
    )
    
    data<-estimate
    
    levels=NULL
    if(input$bg!="0"){
      levels<-input$bg
      data<-subset(estimate,fishing_unit == input$bg)
    }
    
    
    
    table<-artfish_year_summary(
      data = data, target_year = input$year,
      variable = "species", value = value, 
      value_fun = sum
    )
    
    summary<-table$summary%>%
      merge(ref_species%>%select(ID,NAME)%>%mutate(ID=as.character(ID))%>%rename('species'='ID'))%>%
      relocate(NAME)%>%
      select(-species)%>%
      mutate(NAME=ifelse(is.na(NAME),"Total",NAME))%>%
      rename(`Species`=NAME)
    
    rank<-table$rank%>%
      merge(ref_species%>%select(ID,NAME)%>%mutate(ID=as.character(ID))%>%rename('species'='ID'))%>%
      relocate(NAME)%>%
      select(-species)%>%
      mutate(NAME=ifelse(is.na(NAME),"Total",NAME))%>%
      rename(`Species`=NAME,
             Rank = rank,
             Percentage = percent,
             `Cumulative percentage` = cum_percent)
    
    if(input$unit=="K"){
      summary<-summary%>%
        mutate(across(where(is.double), ~.x/1000))
      rank<-rank%>%
        mutate(Total = Total/1000)
    }
    
    if(input$unit=="M"){
      summary<-summary%>%
        mutate(across(where(is.double), ~.x/1000000))
      rank<-rank%>%
        mutate(Total = Total/1000000)
    }
    
    sp_summary(summary)
    sp_rank(rank)
    
  })
  
  output$sp_sum_table<-DT::renderDT(server = FALSE, {
    
    if(!is.null(sp_summary())){
      DT::datatable(
        class = 'cell-border stripe',
        sp_summary(),
        extensions = c("Buttons"),
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 'Bt',
          scrollX=TRUE,
          pageLength=nrow(sp_summary()),
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  sprintf("%s_summary",input$year), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  sprintf("%s_summary",input$year), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',filename = sprintf("%s_summary",input$year),
                 title = sprintf("%s_summary",input$year), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          )
        )
      )%>%
        formatStyle(columns = c("Species"), `text-align` = 'right')%>%
        formatStyle(columns = c("Total"), fontWeight = 'bold', `text-align` = 'left')%>%
        formatRound(2:14,digits=1)
    }
    
  })
  
  output$sp_rank_table<-DT::renderDT(server = FALSE, {
    
    if(!is.null(sp_rank())){
      DT::datatable(
        class = 'cell-border stripe',
        sp_rank(),
        extensions = c("Buttons"),
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = 'Bt',
          scrollX=TRUE,
          pageLength=nrow(sp_rank()),
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  sprintf("%s_rank",input$year), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  sprintf("%s_rank",input$year), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',filename = sprintf("%s_rank",input$year),
                 title = sprintf("%s_rank",input$year), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          )
        )
      )%>%
        formatStyle(columns = c("Species"), `text-align` = 'right')%>%
        formatRound(3,digits=1)%>%
        formatPercentage(4:5,digits = 1)%>%
        formatStyle(
          'Percentage',
          background = styleColorBar(c(0,1), "#029bef",-90),
          backgroundSize = '90% 80%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'left'
        )
    }
    
  })
  
  output$sp_rank_plot<-renderPlotly({
    
    if(!is.null(sp_rank())){
      rank<-sp_rank()
      
      value<-switch(input$indicator,
                    "effort"="effort_nominal",
                    "catch"="catch_nominal_landed",
                    "value" = "trade_value",
                    "cpue" = "catch_cpue"
      )
      
      rank%>%
        filter(Rank<=10)%>%
        plot_ly(y = ~Rank, x = ~Total, type = "bar",  orientation = "h",hoverinfo='none',marker = list(color = ~Rank, colorscale = list(c(0, 1),  c("#029bef", "#84fcfc")), showscale = FALSE),text = ~Species,hovertemplate =~ sprintf("</br>%s[Rank %s]</br>%s:%s</br>Percentage:%s%%",Species,Rank,input$indicator,round(Total,1),round(Percentage*100,1)),
                textposition = "auto",textfont = list(color = "black")) %>%
        layout(showlegend = FALSE,
               uniformtext=list(minsize=8, mode='show'),
               yaxis = list(title=NULL,autorange = "reversed",tickmode = "array", tickvals = unique(rank$Rank), ticktext = unique(rank$Rank)),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,title = FALSE),
               plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)")
      
    }
    
  })
  
  output$sp_number_selector<-renderUI({
    if(input$sp_level=="global"){NULL}else{
      max_nb<-nrow(sp_rank())
      numericInput(ns("sp_number"), paste0(i18n("NUMERIC_INPUT_TITLE_NUMBER_OF_SPECIES")," :"), value = if(max_nb<=10){max_nb}else{10}, min = 0, max = max_nb)
    }
  })
  
  output$sp_sum_plot<-renderPlotly({
    
    if(!is.null(sp_summary())){
      data_plot<-sp_summary()
      
      data_plot<-data_plot%>%
        select(-Total)
      
      if(input$sp_level=="global"){
        data_plot<-data_plot%>%
          filter(Species == "Total")
      }else{
        req(input$sp_number)
        rank_index<-sp_rank()%>%
          filter(Rank%in%seq(1,input$sp_number))%>%
          pull(Species)
        
        data_plot<-data_plot%>%
          filter(Species%in%rank_index)
        
      }
      
      data_plot<-data_plot%>%
        pivot_longer(!Species, names_to = "month", values_to = "value")
      
      if(input$sp_level=="global"){
        p<-data_plot%>%plot_ly(x = ~month,y =~ value, color= ~Species,colors=indicator_color(),type = 'scatter', mode = 'lines',fill = 'tozeroy',line = list(shape = "spline"),text = ~sprintf("%s[%s]: %s",`Species`,month,round(value,1)),name=input$indicator)
      }else{
        p<-data_plot%>%plot_ly(x = ~month,y =~ value,color= ~Species, type = 'scatter', mode = 'lines',line = list(shape = "spline"),text = ~sprintf("%s[%s]: %s",`Species`,month,round(value,1))) 
      }
      p%>%layout(
        hovermode ='closest',
        xaxis = list(
          fixedrange = TRUE,
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          zeroline = F,
          tickmode = "array", tickvals = unique(data_plot$month), ticktext = unique(data_plot$month)
        ),
        yaxis = list(
          tickfont = list(size = 10),
          title = sprintf("%s (%s)",indicator_label(),indicator_unit()),
          zeroline = F
        )
      )
    }
    
  })
  
  output$bg_results<-renderUI({
    tagList(
      fluidRow(
        div(
          class = "col-md-6",
          box(
            title=i18n("BOX_TITLE_MONTHLY_REPARTITION_BY_FISHING_UNITS"),
            width = 12,
            tabsetPanel(type="pills",
                        tabPanel(i18n("PLOT_LABEL"),
                                 box(
                                   title='',
                                   width = 12,
                                   sidebar = shinydashboardPlus::boxSidebar(
                                     id=ns("bg_sum_box"),
                                     width = 25,
                                     style = 'font-size:14px;',
                                     selectizeInput(ns("bg_level"),"Level :",choices=setNames(c("global","detail"),c(i18n("LEVEL_LABEL_GLOBAL"),i18n("LEVEL_LABEL_DETAIL"))),multiple = F)
                                   ),
                                   plotlyOutput(ns("bg_sum_plot"))%>%withSpinner(type = 4))
                        ),
                        tabPanel(i18n("TABLE_LABEL"),DTOutput(ns("bg_sum_table"))%>%withSpinner(type = 4))
            )
          )
        ),
        div(
          class = "col-md-6",
          box(
            title=i18n("BOX_TITLE_TOP_RANKING_FISHING_UNITS"),
            width = 12,
            tabsetPanel(type="pills",
                        tabPanel(i18n("PLOT_LABEL"),plotlyOutput(ns("bg_rank_plot"))%>%withSpinner(type = 4)),
                        tabPanel(i18n("TABLE_LABEL"),DTOutput(ns("bg_rank_table"))%>%withSpinner(type = 4))
            )
          )
        )
      )
    )
  })
  
  output$sp_results<-renderUI({
    req(input$indicator)
    if(input$indicator!="effort"){
      tagList(
        fluidRow(
          div(
            class = "col-md-6",
            box(
              title=i18n("BOX_TITLE_MONTHLY_REPARTITION_BY_SPECIES"),
              width = 12,
              tabsetPanel(type="pills",
                          tabPanel(i18n("PLOT_LABEL"),
                                   box(
                                     title='',
                                     width = 12,
                                     sidebar = shinydashboardPlus::boxSidebar(
                                       id=ns("sp_sum_box"),
                                       width = 25,
                                       style = 'font-size:14px;',
                                       selectizeInput(ns("sp_level"),"Level :",choices=setNames(c("global","detail"),c(i18n("LEVEL_LABEL_GLOBAL"),i18n("LEVEL_LABEL_DETAIL"))),multiple = F),
                                       uiOutput(ns("sp_number_selector"))
                                     ),
                                     plotlyOutput(ns("sp_sum_plot"))%>%withSpinner(type = 4))
                          ),
                          tabPanel(i18n("TABLE_LABEL"),DTOutput(ns("sp_sum_table"))%>%withSpinner(type = 4))
              )
            )
          ),
          div(
            class = "col-md-6",
            box(
              title=i18n("BOX_TITLE_TOP_RANKING_SPECIES"),
              width = 12,
              tabsetPanel(type="pills",
                          tabPanel(i18n("PLOT_LABEL"),plotlyOutput(ns("sp_rank_plot"))%>%withSpinner(type = 4)),
                          tabPanel(i18n("TABLE_LABEL"),DTOutput(ns("sp_rank_table"))%>%withSpinner(type = 4))
              )
            )
          )
        )
      )
    }
  })
  })  
  
}