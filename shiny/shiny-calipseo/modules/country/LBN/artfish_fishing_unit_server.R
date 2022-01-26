#artfish_fishing_unit_server
artfish_fishing_unit_server <- function(input, output, session, pool){
  
  ns<-session$ns
  
  bg_summary<-reactiveVal(NULL)
  bg_rank<-reactiveVal(NULL)
  sp_summary<-reactiveVal(NULL)
  sp_rank<-reactiveVal(NULL)
  
  output$urlPage<-renderUI({
    session$userData$page("artfish-fishing-unit")
    updatePageUrl("artfish-fishing-unit", session)
  })
  
  ref_species<-accessRefSpecies(pool)
  ref_fishing_units<-accessRefFishingUnits(pool)
  
  estimate<-artfish_estimates(data_effort=accessEffortData(pool),data_landing=accessLandingData(pool))
  
  output$year_selector<-renderUI({
    
    years<-sort(unique(estimate$EST_YEAR),decreasing = T)
    
    selectizeInput(ns("year"),"Year :",choices=years,multiple = F,selected=years[1])
  })  

  output$fishing_unit_selector<-renderUI({
      
    req(input$year)
        
    selection<-subset(estimate,EST_YEAR==input$year)
        bg_sp<-unique(selection$EST_BGC)
        
        ref_bg_sp<-subset(ref_fishing_units,ID %in% bg_sp)
          
        bg<-setNames(c(0,ref_bg_sp$ID),c("All fishing units",ref_bg_sp$NAME))
        
        selectizeInput(ns("bg"),"Fishing unit :",choices=bg,multiple = F,selected=bg[1])
  })
  
  output$unit_selector<-renderUI({
    
    req(input$indicator)
    
    selection<-switch(input$indicator,
                      "effort"=c("Days"="base"),
                      "catch"=c("Kilogram (kg)"="base","tonne (t)"="K"),
                      "value"=c("Dollar ($)"="base","thousand of dollars ($K)"="K"),
                      "cpue"=c("Kilogram by day (kg/day)"="base")
                      
                      )
    
    selectizeInput(ns("unit"),"Unit :",choices=selection,multiple = F,selected=selection[1])
  })
  
  output$selectors<-renderUI({
    tagList(
      fluidRow(
        column(6,
               uiOutput(ns("year_selector")),
               uiOutput(ns("fishing_unit_selector"))
        ),
        column(6,
               selectizeInput(ns("indicator"),"Indicator :",choices=c("Effort"="effort","Catch"="catch","Value"="value","CPUE"="cpue"),multiple = F),
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
                  "effort"="EST_EFF_EFFORT",
                  "catch"="EST_LND_CATCH",
                  "value" = "EST_LND_VALUE",
                  "cpue" = "EST_LND_CPUE"
    )
    
    table<-artfish_year_summary(data=estimate,year=input$year,variable="EST_BGC",value=value)

    summary<-table$summary%>%
      left_join(ref_fishing_units%>%select(ID,NAME)%>%mutate(ID=as.character(ID)),by=c('EST_BGC'='ID'))%>%
      relocate(NAME)%>%
      mutate(target=ifelse(EST_BGC == input$bg,"target","other"))%>%
      select(-EST_BGC)%>%
      mutate(NAME=ifelse(is.na(NAME),"Total",NAME))%>%
      rename(`Fishing Unit`=NAME)
    
    rank<-table$rank%>%
      left_join(ref_fishing_units%>%select(ID,NAME)%>%mutate(ID=as.character(ID)),by=c('EST_BGC'='ID'))%>%
      relocate(NAME)%>%
      mutate(target=ifelse(EST_BGC == input$bg,"target","other"))%>%
      select(-EST_BGC)%>%
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
    
    bg_summary(summary)
    bg_rank(rank)
    
  })
  
  output$value<-renderUI({
    if(!is.null(bg_summary())){
      if(input$bg!="0"){
        value<-subset(bg_summary(),target=="target")$Total
      }else{
        value<-subset(bg_summary(),`Fishing Unit`=="Total")$Total
      }
    valueBox(value=tags$p(input$indicator,style="font-size: 40%"), subtitle=round(value,2), icon = tags$i(class = "fas fa-dollar-sign", style="font-size: 30px"), width = 12,color = "blue" )
    }
  })
  
  output$accuracy<-renderUI({
    req(input$year)
    req(input$bg)
    
  accuracy<-subset(estimate,EST_YEAR==input$year)
  if(input$bg!="0"){
  accuracy<-subset(accuracy,EST_BGC==input$bg)
  }
  accuracy<-mean(accuracy$EST_ACCUR,na.rm=T)

  valueBox(value=tags$p("Average Accuracy",style="font-size: 40%"), subtitle=paste(format(round(accuracy*100,1),nsmall=1),"%"), icon = tags$i(class ="fas fa-percent", style="font-size: 30px"), width = 12,color = ifelse(accuracy>=0.9,"green","orange"))

  })
  
  output$accuracy2<-renderPlotly({
    req(input$year)
    req(input$bg)
    
    accuracy<-subset(estimate,EST_YEAR==input$year)
    if(input$bg!="0"){
      accuracy<-subset(accuracy,EST_BGC==input$bg)
    }
    accuracy<-mean(accuracy$EST_ACCUR*100,na.rm=T)
    
    plot_ly(
    domain = list(x = c(0.20, 0.80), y = c(0, 0.90)),
    value = accuracy,
    title = list(text = "Accuracy (%)"),
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
    DT::datatable(
      class = 'cell-border stripe',
      bg_summary(),
      extensions = c("Buttons"),
      escape = FALSE,
      rownames = FALSE,
      options = list(
        dom = 'Bt',
        scrollX=TRUE,
        pageLength=nrow(bg_summary()),
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
                  "effort"="EST_EFF_EFFORT",
                  "catch"="EST_LND_CATCH",
                  "value" = "EST_LND_VALUE",
                  "cpue" = "EST_LND_CPUE"
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
          data_plot<-data_plot%>%
            filter(`Fishing Unit` == "Total")
        }else{
          data_plot<-data_plot%>%
            filter(`Fishing Unit` != "Total")
        }
          
        data_plot<-data_plot%>%
          select(-target)%>%
          pivot_longer(!`Fishing Unit`, names_to = "month", values_to = "value")
          
        if(input$bg_level=="global"){
          p<-data_plot%>%plot_ly(x = ~month,y =~ value, type = 'scatter', mode = 'lines',fill = 'tozeroy',line = list(shape = "spline"),text = ~sprintf("%s[%s]: %s",`Fishing Unit`,month,round(value,1)),name=input$indicator)
        }else{
          p<-data_plot%>%plot_ly(x = ~month,y =~ value,color= ~`Fishing Unit`, type = 'scatter', mode = 'lines',line = list(shape = "spline"),text = ~sprintf("%s[%s]: %s",`Fishing Unit`,month,round(value,1))) 
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
            title = input$indicator,
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
                  "effort"="EST_EFF_EFFORT",
                  "catch"="EST_LND_CATCH",
                  "value" = "EST_LND_VALUE",
                  "cpue" = "EST_LND_CPUE"
    )
    
    data<-estimate
    
    levels=NULL
    if(input$bg!="0"){
      levels<-input$bg
      data<-subset(estimate,EST_BGC == input$bg)
    }
    
    
    
    table<-artfish_year_summary(data=data,year=input$year,variable="EST_SPC",value=value)
    
    summary<-table$summary%>%
      left_join(ref_species%>%select(ID,NAME)%>%mutate(ID=as.character(ID)),by=c('EST_SPC'='ID'))%>%
      relocate(NAME)%>%
      select(-EST_SPC)%>%
      mutate(NAME=ifelse(is.na(NAME),"Total",NAME))%>%
      rename(`Species`=NAME)
    
    rank<-table$rank%>%
      left_join(ref_species%>%select(ID,NAME)%>%mutate(ID=as.character(ID)),by=c('EST_SPC'='ID'))%>%
      relocate(NAME)%>%
      select(-EST_SPC)%>%
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
                    "effort"="EST_EFF_EFFORT",
                    "catch"="EST_LND_CATCH",
                    "value" = "EST_LND_VALUE",
                    "cpue" = "EST_LND_CPUE"
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
      numericInput(ns("sp_number"), "Number of species:", value = if(max_nb<=10){max_nb}else{10}, min = 0, max = max_nb)
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
        p<-data_plot%>%plot_ly(x = ~month,y =~ value, type = 'scatter', mode = 'lines',fill = 'tozeroy',line = list(shape = "spline"),text = ~sprintf("%s[%s]: %s",`Species`,month,round(value,1)),name=input$indicator)
      }else{
        p<-data_plot%>%plot_ly(x = ~month,y =~ value,color= ~`Species`, type = 'scatter', mode = 'lines',line = list(shape = "spline"),text = ~sprintf("%s[%s]: %s",`Species`,month,round(value,1))) 
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
          title = input$indicator,
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
          title="Monthly Repartition By Fishing Units",
          width = 12,
          tabsetPanel(type="pills",
                      tabPanel("Plot",
                               box(
                                 title='',
                                 width = 12,
                                 sidebar = shinydashboardPlus::boxSidebar(
                                   id=ns("bg_sum_box"),
                                   width = 25,
                                   style = 'font-size:14px;',
                                   selectizeInput(ns("bg_level"),"Level :",choices=c("Global"="global","Detail"="detail"),multiple = F)
                                 ),
                                 plotlyOutput(ns("bg_sum_plot"))%>%withSpinner(type = 4))
                      ),
                      tabPanel('Table',DTOutput(ns("bg_sum_table"))%>%withSpinner(type = 4))
          )
        )
      ),
      div(
        class = "col-md-6",
        box(
          title="Top Ranking Fishing Units",
          width = 12,
          tabsetPanel(type="pills",
                      tabPanel("Plot",plotlyOutput(ns("bg_rank_plot"))%>%withSpinner(type = 4)),
                      tabPanel('Table',DTOutput(ns("bg_rank_table"))%>%withSpinner(type = 4))
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
            title="Monthly Repartition By Species",
            width = 12,
            tabsetPanel(type="pills",
                        tabPanel("Plot",
                                 box(
                                   title='',
                                   width = 12,
                                   sidebar = shinydashboardPlus::boxSidebar(
                                     id=ns("sp_sum_box"),
                                     width = 25,
                                     style = 'font-size:14px;',
                                     selectizeInput(ns("sp_level"),"Level :",choices=c("Global"="global","Detail"="detail"),selected="detail",multiple = F),
                                     uiOutput(ns("sp_number_selector"))
                                   ),
                                   plotlyOutput(ns("sp_sum_plot"))%>%withSpinner(type = 4))
                        ),
                        tabPanel('Table',DTOutput(ns("sp_sum_table"))%>%withSpinner(type = 4))
            )
          )
        ),
        div(
          class = "col-md-6",
          box(
            title="Top Ranking Species",
            width = 12,
            tabsetPanel(type="pills",
                        tabPanel("Plot",plotlyOutput(ns("sp_rank_plot"))%>%withSpinner(type = 4)),
                        tabPanel('Table',DTOutput(ns("sp_rank_table"))%>%withSpinner(type = 4))
            )
          )
        )
      )
    )
    }
  })
}