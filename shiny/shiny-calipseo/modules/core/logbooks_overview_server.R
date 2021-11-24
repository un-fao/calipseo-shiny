#logbooks_overview_server
logbooks_overview_server <- function(input, output, session, pool){
  
  ns<-session$ns
  
  output$urlPage<-renderUI({
    session$userData$page("logbooks-overview")
    updatePageUrl("logbooks-overview", session)
  })
  
  currentyear <- as.integer(format(Sys.Date(),"%Y"))
  lastyear <- currentyear-1
  
  infos_fetched <- reactiveVal(FALSE)
  infos <- reactiveValues(
    currentyear = currentyear,
    lastyear = lastyear,
    total_lastyear = NULL,
    total_currentyear = NULL,
    stats_by_type_lastyear = NULL,
    stats_by_type_currentyear = NULL,
    ratio_reporting_lastyear = NULL,
    ratio_reporting_currentyear = NULL,
    data_logbooks = NULL
  )
  
  #functions
  #mean quantities by vessel type
  compute_stats_by_vessel_type = function(data){
    if(nrow(data)==0){
      return(data.frame(
        vesstype = character(0),
        sum = character(0),
        mean = character(0),
        sd = character(0),
        q1 = character(0),
        median = character(0),
        q3 = character(0),
        stringsAsFactors = FALSE
      ))
    }
    logbooks_sum_by_trip = aggregate(
      data$quantity,
      by = list(
        landing_id = data$landing_id,
        vesstype = data$vesstype
      ),
      sum
    )
    colnames(logbooks_sum_by_trip)[3] <- "quantity"
    logbooks_stats_by_type = do.call(data.frame, aggregate(
      logbooks_sum_by_trip$quantity, 
      by = list(
        vesstype = logbooks_sum_by_trip$vesstype
      ),
      FUN = function(x){
        list(
          sum = round(sum(x, na.rm = TRUE),2),
          mean = round(mean(x, na.rm = TRUE),2),
          sd = round(sd(x, na.rm = TRUE),2),
          q1 = round(quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE),2),
          median = round(median(x, na.rm = TRUE),2),
          q3 = round(quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE),2)
        )
      }))
    colnames(logbooks_stats_by_type)[2:7] <- c("sum", "mean", "sd", "q1", "median", "q3") 
    return(logbooks_stats_by_type)
  }
  
  observe({
    
    #logbooks data
    logbooks_lastyear <- accessLogBooks(pool, lastyear)
    logbooks_currentyear <- accessLogBooks(pool, currentyear)
    #vessels counting by stat type
    vessel_count = accessVesselsCountByStatType(pool)
    vessel_count = vessel_count[!is.na(vessel_count$ID) & vessel_count$ID == 2,]$COUNT
    #infos counting
    infos$total_lastyear <- sum(logbooks_lastyear$quantity) #assumes all units = KG
    infos$total_currentyear <- sum(logbooks_currentyear$quantity) #assumes all units = KG
    #stats by type
    infos$stats_by_type_lastyear <- compute_stats_by_vessel_type(logbooks_lastyear)
    infos$stats_by_type_currentyear <- compute_stats_by_vessel_type(logbooks_currentyear)
    #ratios reporting
    infos$ratio_reporting_lastyear <- paste0(if(length(unique(logbooks_lastyear$regnum))/vessel_count*100<0.01){"<0.01"}else{round(length(unique(logbooks_lastyear$regnum))/vessel_count*100, 2)},"%")
    infos$ratio_reporting_currentyear <- paste0(if(length(unique(logbooks_currentyear$regnum))/vessel_count*100<0.01){"<0.01"}else{round(length(unique(logbooks_currentyear$regnum))/vessel_count*100, 2)},"%")
  })
  
  observe({
    
    if(all(!sapply(reactiveValuesToList(infos), is.null))) infos_fetched(TRUE)
    
    #info
    output$logbooks_overview_info <- renderText({
      #session$userData$page("logbooks_overview")
      text <- "<h2>Overview of industrial fishing activities<small>Based on logbooks monitoring</small></h2>"
    })
    
    #counters
    output$nb_infos <- renderUI({
      tagList(
        fluidRow(
          infoBox(sprintf("Total quantity (%s)", lastyear), paste(infos$total_lastyear, "kg"), icon = icon("fish"), fill = TRUE, width = 6),
          infoBox(sprintf("Total quantity (%s)", currentyear), paste(infos$total_currentyear, "kg"), icon = icon("fish"), fill = TRUE, width = 6)
        ),
        fluidRow(
          infoBox(sprintf("Logbook reporting percentage (%s)", lastyear), infos$ratio_reporting_lastyear, icon = icon("percent"), fill = TRUE, width = 6),
          infoBox(sprintf("Logbook reporting percentage (%s)", currentyear), infos$ratio_reporting_currentyear, icon = icon("percent"), fill = TRUE, width = 6)
        )
      )
    })
    
    #stats by type
    output$stats_by_type_lastyear_table <- renderDataTable(
      infos$stats_by_type_lastyear,
      server = FALSE,
      escape = FALSE,
      rownames = FALSE,
      extensions = c("Buttons"), 
      options = list(
        autoWidth = TRUE,
        dom = 'Bfrtip',
        deferRender = TRUE,
        scroll = FALSE,
        buttons = list(
          list(extend = 'copy'),
          list(extend = 'csv', filename =  sprintf("stats_by_vesseltype_%s", lastyear), title = NULL, header = TRUE),
          list(extend = 'excel', filename =  sprintf("stats_by_vesseltype_%s", lastyear), title = NULL, header = TRUE),
          list(extend = "pdf", filename = sprintf("stats_by_vesseltype_%s", lastyear), 
               title = sprintf("Statistics by vessel type - %s", lastyear), header = TRUE)
        ),
        exportOptions = list(
          modifiers = list(page = "all", selected = TRUE)
        )
      )
    )
    output$stats_by_type_currentyear_table <- renderDataTable(
      infos$stats_by_type_currentyear,
      server = FALSE,
      escape = FALSE,
      rownames = FALSE,
      extensions = c("Buttons"), 
      options = list(
        autoWidth = TRUE,
        dom = 'Bfrtip',
        deferRender = TRUE,
        scroll = FALSE,
        buttons = list(
          list(extend = 'copy'),
          list(extend = 'csv', filename =  sprintf("stats_by_vesseltype_%s", currentyear), title = NULL, header = TRUE),
          list(extend = 'excel', filename =  sprintf("stats_by_vesseltype_%s", currentyear), title = NULL, header = TRUE),
          list(extend = "pdf", filename = sprintf("stats_by_vesseltype_%s", currentyear), 
               title = sprintf("Statistics by vessel type - %s", currentyear), header = TRUE)
        ),
        exportOptions = list(
          modifiers = list(page = "all", selected = TRUE)
        )
      )
    )
    
  })
  
#Vessel type plot
  
  observeEvent(input$vt_stat,{
    output$vt_additional<-renderUI({
      if(input$vt_stat=="mean"){
        checkboxInput(ns("vt_withsd"),"standard deviation", value = FALSE)
      }else if(input$vt_stat=="median"){
        checkboxInput(ns("vt_withquartile"),"quartiles", value = FALSE)
      }else{
        NULL
      }
    })
  })
  
  observeEvent(c(input$vt_stat,input$vt_granu),{
    
    output$vt_plot<-renderPlotly({
      
      data_logbooks <- accessLogBooksMultiyear(pool)
      
      p<-data_logbooks%>%
        mutate(date = as.character(format(as.Date(date),format = input$vt_granu)))%>%
        group_by(date,vesseltype,trip_id)%>%
        summarise(sum_by_trip = sum(quantity))%>%
        group_by(date,vesseltype)%>%
        summarise(sum = sum(sum_by_trip, na.rm = TRUE),
                  mean = mean(sum_by_trip, na.rm = TRUE),
                  min = min(sum_by_trip, na.rm = TRUE),
                  max = max(sum_by_trip, na.rm = TRUE),
                  sd = sd(sum_by_trip, na.rm = TRUE),
                  q1 = quantile(sum_by_trip, probs = 0.25, na.rm = TRUE, names = FALSE),
                  median = median(sum_by_trip, na.rm = TRUE),
                  q3 = quantile(sum_by_trip, probs = 0.75, na.rm = TRUE, names = FALSE)
        )%>%
        mutate(vesseltype=as.factor(vesseltype))%>%
        mutate(sd = ifelse(is.na(sd), 0, sd))%>%
        ungroup()%>%
        plot_ly(
          x = ~date
        )
      
      if(isTRUE(input$vt_withquartile)&input$vt_stat=="median"){
        p<-p%>%add_boxplot(x = ~date,color= ~vesseltype,type = "box", q1=~ q1, median=~ median,q3=~ q3, mean=~ mean,lowerfence=~ min,upperfence=~ max)
      }else{
        p<-p%>%    
          add_lines(y =~ get(input$vt_stat),color= ~vesseltype,line = list(simplyfy = F),text = ~sprintf("%s[%s]: %s kg",vesseltype,date,round(get(input$vt_stat),2)))
      }
      
      if(isTRUE(input$vt_withsd)){
        p<-p%>%add_ribbons(color= ~vesseltype,
                           ymin = ~ get(input$vt_stat)-sd,
                           ymax = ~ get(input$vt_stat)+sd,
                           showlegend=F,
                           opacity = 0.3,
                           line = list(dash="dash"))
      }
      
      p%>%layout(
        showlegend=T,
        hovermode ='closest',
        xaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          title = "Time",
          zeroline = F
        ),
        yaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          title = "Quantity (kg)",
          zeroline = F
        ))
    })
  })  
  
#Species plot

  output$sp_nb_selector<-renderUI({
    data_logbooks <- accessLogBooksMultiyear(pool)
    nb_sp<-length(unique(data_logbooks))
    numericInput(ns("sp_number"), "Display x most catched species:", value = if(nb_sp<=5){nb_sp}else{5}, min = 0, max = nb_sp)
  })

  observeEvent(input$sp_stat,{
    output$sp_additional<-renderUI({
      if(input$sp_stat=="mean"){
        checkboxInput(ns("sp_withsd"),"standard deviation", value = FALSE)
      }else if(input$sp_stat=="median"){
        checkboxInput(ns("sp_withquartile"),"quartiles", value = FALSE)
      }else{
        NULL
      }
    })
  })

  observeEvent(c(input$sp_number,input$sp_stat,input$sp_granu,input$sp_rank_method),{
  
  req(input$sp_number)
  output$sp_plot<-renderPlotly({
    
    data_logbooks <- accessLogBooksMultiyear(pool)
    
    if(input$sp_rank_method=="sum"){
    rank_sp <- data_logbooks %>%
               group_by(species_desc) %>% 
               summarise(total = sum(quantity))%>%
               mutate(rank = rank(-total)) %>%
               filter(rank <=as.numeric(input$sp_number)) %>%
               arrange(rank)%>%
               pull(species_desc)
    }
    
    if(input$sp_rank_method=="year_avg"){
    rank_sp <- data_logbooks %>%
      group_by(year,species_desc) %>% 
      summarise(total = sum(quantity))%>%
      group_by(species_desc)%>%
      summarise(avg = mean(total))%>%
      mutate(rank = rank(-avg)) %>%
      filter(rank <=as.numeric(input$sp_number)) %>%
      arrange(rank)%>%
      pull(species_desc)
    }
    
    if(input$sp_rank_method=="last_year"){
    rank_sp <- data_logbooks %>%
      filter(year==max(year))%>%
      group_by(species_desc) %>% 
      summarise(total = sum(quantity))%>%
      mutate(rank = rank(-total)) %>%
      filter(rank <=as.numeric(input$sp_number)) %>%
      arrange(rank)%>%
      pull(species_desc)
    }
    
    p<-data_logbooks%>%
      filter(species_desc%in%rank_sp)%>%
      mutate(date = as.character(format(as.Date(date),format = input$sp_granu)))%>%
      group_by(date,species_desc,species_sci,species_asfis,trip_id)%>%
      summarise(sum_by_trip = sum(quantity))%>%
      group_by(date,species_desc,species_sci,species_asfis)%>%
      summarise(sum = sum(sum_by_trip, na.rm = TRUE),
                mean = mean(sum_by_trip, na.rm = TRUE),
                min = min(sum_by_trip, na.rm = TRUE),
                max = max(sum_by_trip, na.rm = TRUE),
                sd = sd(sum_by_trip, na.rm = TRUE),
                q1 = quantile(sum_by_trip, probs = 0.25, na.rm = TRUE, names = FALSE),
                median = median(sum_by_trip, na.rm = TRUE),
                q3 = quantile(sum_by_trip, probs = 0.75, na.rm = TRUE, names = FALSE)
      )%>%
      mutate(species_desc =factor(species_desc,levels=rank_sp))%>%
      mutate(sd = ifelse(is.na(sd), 0, sd))%>%
      ungroup()%>%
      plot_ly(
        x = ~date
      )
    
    if(isTRUE(input$sp_withquartile)&input$sp_stat=="median"){
      p<-p%>%add_boxplot(x = ~date,color= ~species_desc ,type = "box", q1=~ q1, median=~ median,q3=~ q3, mean=~ mean,lowerfence=~ min,upperfence=~max)
    }else{
      p<-p%>%    
        add_lines(y =~ get(input$sp_stat),color= ~species_desc ,line = list(simplyfy = F),text = ~sprintf("%s-<em>%s</em>(<b>%s</b>)[%s]: %s kg",species_desc,species_sci,species_asfis,date,round(get(input$sp_stat),2)))
    }
    
    if(isTRUE(input$sp_withsd)){
      p<-p%>%add_ribbons(color= ~species_desc ,
                         ymin = ~ get(input$sp_stat)-sd,
                         ymax = ~ get(input$sp_stat)+sd,
                         showlegend=F,
                         opacity = 0.3,
                         line = list(dash="dash"))
    }
    
    p%>%layout(
      showlegend=T,
      hovermode ='closest',
      xaxis = list(
        titlefont = list(size = 10), 
        tickfont = list(size = 10),
        title = "Time",
        zeroline = F
      ),
      yaxis = list(
        titlefont = list(size = 10), 
        tickfont = list(size = 10),
        title = "Quantity (kg)",
        zeroline = F
      ))
    })
  })
  
#Fish group plot
  
  fish_group<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_GROUPS.csv")
  fish_group<-subset(fish_group,select=c('3A_Code','ISSCAAP_Group_En'))
  names(fish_group)<-c('species_asfis','ISSCAAP_Group_En')
  
  observeEvent(input$fg_stat,{
    output$fg_additional<-renderUI({
      if(input$fg_stat=="mean"){
        checkboxInput(ns("fg_withsd"),"standard deviation", value = FALSE)
      }else if(input$fg_stat=="median"){
        checkboxInput(ns("fg_withquartile"),"quartiles", value = FALSE)
      }else{
        NULL
      }
    })
  })
  
  observeEvent(c(input$fg_stat,input$fg_granu),{
    
    output$fg_plot<-renderPlotly({
      
      data_logbooks <- accessLogBooksMultiyear(pool)
      
      p<-data_logbooks%>%
        mutate(date = as.character(format(as.Date(date),format = input$fg_granu)))%>%
        left_join(fish_group)%>%
        group_by(date,ISSCAAP_Group_En,trip_id)%>%
        summarise(sum_by_trip = sum(quantity))%>%
        group_by(date,ISSCAAP_Group_En)%>%
        summarise(sum = sum(sum_by_trip, na.rm = TRUE),
                  mean = mean(sum_by_trip, na.rm = TRUE),
                  min = min(sum_by_trip, na.rm = TRUE),
                  max = max(sum_by_trip, na.rm = TRUE),
                  sd = sd(sum_by_trip, na.rm = TRUE),
                  q1 = quantile(sum_by_trip, probs = 0.25, na.rm = TRUE, names = FALSE),
                  median = median(sum_by_trip, na.rm = TRUE),
                  q3 = quantile(sum_by_trip, probs = 0.75, na.rm = TRUE, names = FALSE)
        )%>%
        mutate(ISSCAAP_Group_En=as.factor(ISSCAAP_Group_En))%>%
        mutate(sd = ifelse(is.na(sd), 0, sd))%>%
        ungroup()%>%
        plot_ly(
          x = ~date
        )
      
      if(isTRUE(input$fg_withquartile)&input$fg_stat=="median"){
        p<-p%>%add_boxplot(x = ~date,color= ~ISSCAAP_Group_En,type = "box", q1=~ q1, median=~ median,q3=~ q3, mean=~ mean,lowerfence=~ min,upperfence=~ max)
      }else{
        p<-p%>%    
          add_lines(y =~ get(input$fg_stat),color= ~ISSCAAP_Group_En,line = list(simplyfy = F),text = ~sprintf("%s[%s]: %s kg",ISSCAAP_Group_En,date,round(get(input$fg_stat),2)))
      }
      
      if(isTRUE(input$fg_withsd)){
        p<-p%>%add_ribbons(color= ~ISSCAAP_Group_En,
                           ymin = ~ get(input$fg_stat)-sd,
                           ymax = ~ get(input$fg_stat)+sd,
                           showlegend=F,
                           opacity = 0.3,
                           line = list(dash="dash"))
      }
      
      p%>%layout(
        showlegend=T,
        hovermode ='closest',
        xaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          title = "Time",
          zeroline = F
        ),
        yaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          title = "Quantity (kg)",
          zeroline = F
        ))
    })
  })

  
}