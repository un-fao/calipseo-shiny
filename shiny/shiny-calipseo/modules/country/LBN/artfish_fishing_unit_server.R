#artfish_fishing_unit_server
artfish_fishing_unit_server <- function(id, pool){

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
  
  estimate<-artfish_estimates(data_effort=accessEffortData(pool),data_landing=accessLandingData(pool))
  
  
  
  indicator_output <- if(input$indicator==i18n("EFFORT_LABEL")){
    "effort"
  }else if(input$indicator==i18n("CATCH_LABEL")){
    "catch"
  }else if(input$indicator==i18n("VALUE_LABEL")){
    "value"
  }else if(input$indicator==i18n("CPUE_LABEL")){
    "cpue"
  }
  
  
  bg_level_output <- if(input$bg_level==i18n("LEVEL_LABLE_GLOBAL")){
    "global"
  }else if(input$bg_level==i18n("LEVEL_LABLE_DETAIL")){
    "detail"
  }
  
 
  sp_level_output <- if(input$sp_level==i18n("LEVEL_LABLE_GLOBAL")){
    "global"
  }else if(input$sp_level==i18n("LEVEL_LABLE_DETAIL")){
    "detail"
  }
  
  
  unit_output_1 <- if(input$unit_1==i18n("KILOGRAM_LABEL")){
    "base"
  }else if(input$unit_1==i18n("TONNE_LABEL")){
    "K"
  }
  
  unit_output_2 <- if(input$unit_2==i18n("DOLLAR_LABEL")){
    "base"
  }else if(input$unit_2==i18n("THOUSAND_DOLLAR_LABEL")){
    "K"
  }else if(input$unit_2==i18n("MILLION_DOLLAR_LABEL")){
    "M"
  }
  
  
  
  output$year_selector<-renderUI({
    
    years<-sort(unique(estimate$EST_YEAR),decreasing = T)
    
    selectizeInput(ns("year"),paste0(i18n("SELECT_INPUT_TITLE_YEAR")," :"),choices=years,multiple = F,selected=years[1])
  })  

  output$fishing_unit_selector<-renderUI({
      
    req(input$year)
        
    selection<-subset(estimate,EST_YEAR==input$year)
        bg_sp<-unique(selection$EST_BGC)
        
        ref_bg_sp<-subset(ref_fishing_units,ID %in% bg_sp)
          
        bg<-setNames(c(0,ref_bg_sp$ID),c(i18n("ALL_FISHING_UNITS_LABEL"),ref_bg_sp$NAME))
        
        selectizeInput(ns("bg"),paste0(i18n( "SELECT_INPUT_TITLE_FISHING_UNIT")," :"),choices=bg,multiple = F,selected=bg[1])
  })
  
 
  #Fishing Units Plots and Tables
  
  observe({
    req(input$year)
    req(input$bg)
    req(input$indicator)
    req(input$unit)
    
    value<-switch(indicator_output,
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
    
    switch(indicator_output,
           "effort"={
             indicator_color("purple")
             indicator_icon("fas fa-clock")
             indicator_unit(i18n("UNIT_LABEL_DAYS"))
             indicator_label(i18n("INDICATOR_LABEL_TOTAL_EFFORT"))
             
             },
           "catch"={
             indicator_color("orange")
             indicator_icon("fas fa-fish")
             if(unit_output_1=="base"){indicator_unit(i18n("UNIT_LABEL_KILOGRAM"))}else{indicator_unit(i18n("UNIT_LABEL_TONNE"))}
             indicator_label(i18n("INDICATOR_LABEL_TOTAL_CATCH"))
             },
           "value" = {
             indicator_color("blue")
             indicator_icon("fas fa-dollar-sign")
             if(unit_output_2=="base"){indicator_unit(i18n("UNIT_LABEL_DOLLAR"))}else if(unit_output_2==i18n("UNIT_LABEL_THOUSAND")){indicator_unit(i18n("UNIT_LABEL_THOUSAND_DOLLAR"))}else{indicator_unit(i18n("UNIT_LABEL_MILLION_DOLLAR"))}
             indicator_label(i18n("INDICATOR_LABEL_TOTAL_VALUE"))
           },
           "cpue" = {
             indicator_color("red")
             indicator_icon("fas fa-ship")
             indicator_unit(i18n("UNIT_LABEL_KILOGRAM_DAY"))
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
        value<-subset(bg_summary(),`Fishing Unit`=="Total")$Total
      }
    valueBox(value=tags$p(sprintf("%s (%s)",indicator_label(),indicator_unit()),style="font-size: 40%"), subtitle=round(value,2), icon = tags$i(class = indicator_icon(), style="font-size: 30px"), width = 12,color = indicator_color() )
    }
  })
  
  output$accuracy<-renderPlotly({
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
    title = list(text = paste0(i18n( "GUAGE_TITLE_ACCURACY")," (%)")),
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
      df$`Fishing Unit`[nrow(df)] <- i18n("FISHING_UNIT_TABLE_ROWNAME_9")
      names(df)[names(df)=='Fishing Unit'] <- i18n("FISHING_UNIT_TABLE_COLNAME_1")
      names(df)[names(df)=='Total'] <- i18n("FISHING_UNIT_TABLE_ROWNAME_9")
      
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
          list(extend = 'csv', filename =  paste0(input$year,"_",i18n("TABLE_TITLE_SUMMARY")), title = NULL, header = TRUE),
          list(extend = 'excel', filename =  paste0(input$year,"_",i18n("TABLE_TITLE_SUMMARY")), title = NULL, header = TRUE),
          list(extend = "pdf", pageSize = 'A4',filename = paste0(input$year,"_",i18n("TABLE_TITLE_SUMMARY")),
               title = paste0(input$year,"_",i18n("TABLE_TITLE_SUMMARY")), header = TRUE)
        ),
        exportOptions = list(
          modifiers = list(page = "all",selected=TRUE)
        ),
        language = list(url = i18n("TABLE_LANGUAGE"))
      )
    )%>%
      formatStyle(columns = c(i18n("FISHING_UNIT_TABLE_COLNAME_1")), `text-align` = 'right')%>%
      formatStyle(columns = c(i18n("FISHING_UNIT_TABLE_ROWNAME_9")), fontWeight = 'bold', `text-align` = 'left')%>%
      formatRound(2:14,digits=1)%>%
      formatStyle("target",target = 'row',backgroundColor = styleEqual(c("target"), c("orange")))
    }

})

  output$bg_rank_table<-DT::renderDT(server = FALSE, {
    
    df <- bg_rank()
    names(df) <- c(i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_1"),i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_2"),
                   i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_3"), i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_4"),
                   i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_5"), i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_6"))
    
    
    if(!is.null(bg_rank())){
      DT::datatable(
        class = 'cell-border stripe',
        df,
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
            list(extend = 'csv', filename =  paste0(input$year,"_",i18n("TABLE_TITLE_RANK")), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  paste0(input$year,"_",i18n("TABLE_TITLE_RANK")), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',filename = paste0(input$year,"_",i18n("TABLE_TITLE_RANK")),
                 title = paste0(input$year,"_",i18n("TABLE_TITLE_RANK")), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          ),
          language = list(url = i18n("TABLE_LANGUAGE"))
        )
      )%>%
        formatStyle(columns = c(i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_1")), `text-align` = 'right')%>%
        formatRound(3,digits=1)%>%
        formatPercentage(4:5,digits = 1)%>%
        formatStyle(
          i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_4"),
          background = styleColorBar(c(0,1), "#029bef",-90),
          backgroundSize = '90% 80%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'left'
        )%>%
        formatStyle(i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_6"),target = 'row',backgroundColor = styleEqual(c(i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_6")), c("orange")))
    }
    
  })
  
  output$bg_rank_plot<-renderPlotly({
    
    if(!is.null(bg_rank())){
    rank<-bg_rank()
    
    value<-switch(indicator_output,
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
        }else if(bg_level_output=="global"){
          data_plot<-data_plot%>%
            filter(`Fishing Unit` == "Total")
        }else{
          data_plot<-data_plot%>%
            filter(`Fishing Unit` != "Total")
        }
          
        data_plot<-data_plot%>%
          select(-target)%>%
          pivot_longer(!`Fishing Unit`, names_to = "month", values_to = "value")
          
        if(bg_level_output=="global"){
          p<-data_plot%>%plot_ly(x = ~month,y =~ value,color= ~`Fishing Unit`, colors=indicator_color(),type = 'scatter', mode = 'lines',fill = 'tozeroy',line = list(shape = "spline"),text = ~sprintf("%s[%s]: %s",`Fishing Unit`,month,round(value,1)),name=input$indicator)
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
    
    value<-switch(indicator_output,
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
    
    df <- sp_summary()
    df$Species[nrow(df)] <- i18n("MONTHLY_REPARTITION_BY_SPECIES_TABLE_COLNAME_14")
    names(df)[names(df)=='Species'] <- i18n("MONTHLY_REPARTITION_BY_SPECIES_TABLE_COLNAME_1")
    names(df)[names(df)=='Total'] <- i18n("MONTHLY_REPARTITION_BY_SPECIES_TABLE_COLNAME_14")
    
    if(!is.null(sp_summary())){
      
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
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  paste0(input$year,"_",i18n("TABLE_TITLE_SUMMARY")), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  paste0(input$year,"_",i18n("TABLE_TITLE_SUMMARY")), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',filename = paste0(input$year,"_",i18n("TABLE_TITLE_SUMMARY")),
                 title = paste0(input$year,"_",i18n("TABLE_TITLE_SUMMARY")), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          ),
          language = list(url = i18n("TABLE_LANGUAGE"))
        )
      )%>%
        formatStyle(columns = c(i18n("MONTHLY_REPARTITION_BY_SPECIES_TABLE_COLNAME_1")), `text-align` = 'right')%>%
        formatStyle(columns = c(i18n("MONTHLY_REPARTITION_BY_SPECIES_TABLE_COLNAME_14")), fontWeight = 'bold', `text-align` = 'left')%>%
        formatRound(2:14,digits=1)
    }
    
  })
  
  output$sp_rank_table<-DT::renderDT(server = FALSE, {
    
    if(!is.null(sp_rank())){
      df <- sp_rank()
      names(df) <- c(i18n("MONTHLY_REPARTITION_BY_SPECIES_TABLE_COLNAME_1"),i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_2"),
                     i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_3"), i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_4"),
                     i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_5"))
      
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
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  paste0(input$year,"_",i18n("TABLE_TITLE_RANK")), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  paste0(input$year,"_",i18n("TABLE_TITLE_RANK")), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',filename = paste0(input$year,"_",i18n("TABLE_TITLE_RANK")),
                 title = paste0(input$year,"_",i18n("TABLE_TITLE_RANK")), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          ),
          language = list(url = i18n("TABLE_LANGUAGE"))
        )
      )%>%
        formatStyle(columns = c(i18n("MONTHLY_REPARTITION_BY_SPECIES_TABLE_COLNAME_1")), `text-align` = 'right')%>%
        formatRound(3,digits=1)%>%
        formatPercentage(4:5,digits = 1)%>%
        formatStyle(
          i18n("TOP_RANKING_FISHING_UNIT_TABLE_COLNAME_4"),
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
      
      value<-switch(indicator_output,
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
    if(sp_level_output=="global"){NULL}else{
      max_nb<-nrow(sp_rank())
      numericInput(ns("sp_number"), paste0(i18n("NUMERIC_INPUT_TITLE_NUMBER_OF_SPECIES"),":"), value = if(max_nb<=10){max_nb}else{10}, min = 0, max = max_nb)
    }
  })
  
  output$sp_sum_plot<-renderPlotly({
    
    if(!is.null(sp_summary())){
      data_plot<-sp_summary()
      
      data_plot<-data_plot%>%
        select(-Total)
      
      if(sp_level_output=="global"){
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
      
      if(sp_level_output=="global"){
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
  
 })  
  
}