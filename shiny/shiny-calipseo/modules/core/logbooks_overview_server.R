#logbooks_overview_server
logbooks_overview_server <- function(id, pool){
 
 moduleServer(id, function(input, output, session) {  
   
  ns<-session$ns
  
  currentyear <- as.integer(format(Sys.Date(),"%Y"))
  lastyear <- currentyear-1
  
  infos_fetched <- reactiveVal(FALSE)
  infos <- reactiveValues(
    currentyear = currentyear,
    lastyear = lastyear,
    nb_active_vessel_lastyear = NULL,
    nb_active_vessel_currentyear = NULL,
    total_lastyear = NULL,
    total_currentyear = NULL,
    stats_by_type_lastyear = NULL,
    stats_by_type_currentyear = NULL,
    ratio_reporting_lastyear = NULL,
    ratio_reporting_currentyear = NULL,
    data_logbooks = NULL
  )
  
  # #functions
  # #mean quantities by vessel type
  # compute_stats_by_vessel_type = function(data){
  #   if(nrow(data)==0){
  #     return(data.frame(
  #       vesstype = character(0),
  #       sum = character(0),
  #       mean = character(0),
  #       sd = character(0),
  #       q1 = character(0),
  #       median = character(0),
  #       q3 = character(0),
  #       stringsAsFactors = FALSE
  #     ))
  #   }
  #   logbooks_sum_by_trip = aggregate(
  #     data$quantity,
  #     by = list(
  #       landing_id = data$landing_id,
  #       vesstype = data$vesstype
  #     ),
  #     sum
  #   )
  #   colnames(logbooks_sum_by_trip)[3] <- "quantity"
  #   logbooks_stats_by_type = do.call(data.frame, aggregate(
  #     logbooks_sum_by_trip$quantity, 
  #     by = list(
  #       vesstype = logbooks_sum_by_trip$vesstype
  #     ),
  #     FUN = function(x){
  #       x<-x/1000
  #       list(
  #         sum = round(sum(x, na.rm = TRUE),2),
  #         mean = round(mean(x, na.rm = TRUE),2),
  #         sd = round(sd(x, na.rm = TRUE),2),
  #         q1 = round(quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE),2),
  #         median = round(median(x, na.rm = TRUE),2),
  #         q3 = round(quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE),2)
  #       )
  #     }))
  #   colnames(logbooks_stats_by_type)[2:7] <- c("sum", "mean", "sd", "q1", "median", "q3") 
  #   return(logbooks_stats_by_type)
  # }
  
  observe({
    
    #logbooks data
    logbooks_lastyear <- accessLogBooks(pool, lastyear)
    logbooks_currentyear <- accessLogBooks(pool, currentyear)
    infos$nb_active_vessel_lastyear<-length(unique(logbooks_lastyear$regnum))
    infos$nb_active_vessel_currentyear<-length(unique(logbooks_currentyear$regnum))
    # #vessels counting by stat type
    # vessel_count = accessVesselsCountByStatType(pool)
    # vessel_count = vessel_count[!is.na(vessel_count$ID) & vessel_count$ID == 2,]$COUNT
    #infos counting
    infos$total_lastyear <- sum(logbooks_lastyear$quantity) #assumes all units = KG
    infos$total_currentyear <- sum(logbooks_currentyear$quantity) #assumes all units = KG
  #   #stats by type
  #   infos$stats_by_type_lastyear <- compute_stats_by_vessel_type(logbooks_lastyear)
  #   infos$stats_by_type_currentyear <- compute_stats_by_vessel_type(logbooks_currentyear)
  #   #ratios reporting
  #   infos$ratio_reporting_lastyear <- paste0(if(length(unique(logbooks_lastyear$regnum))/vessel_count*100<0.01){"<0.01"}else{round(length(unique(logbooks_lastyear$regnum))/vessel_count*100, 2)},"%")
  #   infos$ratio_reporting_currentyear <- paste0(if(length(unique(logbooks_currentyear$regnum))/vessel_count*100<0.01){"<0.01"}else{round(length(unique(logbooks_currentyear$regnum))/vessel_count*100, 2)},"%")
   })
  
  observe({
    
    if(all(!sapply(reactiveValuesToList(infos), is.null))) infos_fetched(TRUE)
    
    #info
    output$logbooks_overview_info <- renderText({
      #session$userData$page("logbooks_overview")
      text <- paste0("<h2>", i18n("LOGBOOKS_OVERVIEW_TITLE")," <small>", i18n("LOGBOOKS_OVERVIEW_SUBTITLE"),"</small></h2>")
      text
    })
    
    #counters
    output$nb_infos <- renderUI({
      tagList(
        tags$head(tags$style(HTML('.info-box {min-height: 55px;} .info-box-icon {height: 55px; line-height: 55px;} .info-box-content {padding-top: 2px; padding-bottom: 2px;}'))),
        fluidRow(
          div(
            class = "col-md-6",
            box(
              title = HTML(sprintf("<b>%s</b>",as.integer(format(Sys.Date(), "%Y"))-1)),
              width = 12,
              CalipseoInfoBox(i18n("INFOBOX_OVERVIEW_TOTAL_QUANTITY"), style_title = "font-size:60%;",style_value = "font-size:90%;",paste(round(ifelse(PREF_UNIT_WEIGHT=="Kilogram",infos$total_lastyear/1000,infos$total_lastyear),2), ifelse(PREF_UNIT_WEIGHT=="Kilogram",i18n("TOTAL_QUANTITY_UNITS_TONS"),tolower(PREF_UNIT_WEIGHT))), icon = icon("fish"), width = 6),
             #CalipseoInfoBox(i18n("INFOBOX_OVERVIEW_LOGBOOK_REPORTING_PERCENTAGE"), style_title = "font-size: 58%;",style_value = "font-size:90%;", infos$ratio_reporting_lastyear, icon = icon("percent"), width = 6)
              CalipseoInfoBox(i18n("INFOBOX_OVERVIEW_TOTAL_PARTICIPATING_VESSELS"), style_title = "font-size: 58%;",style_value = "font-size:90%;", infos$nb_active_vessel_lastyear, icon = icon("ship"), width = 6)
            )
          ),
          div(
            class = "col-md-6",
            box(
              title=HTML(sprintf("<b>%s</b>",format(Sys.Date(), "%Y"))),
              width = 12,
              CalipseoInfoBox(i18n("INFOBOX_OVERVIEW_TOTAL_QUANTITY"), style_title = "font-size:60%;",style_value = "font-size:90%;", paste(round(ifelse(PREF_UNIT_WEIGHT=="Kilogram",infos$total_currentyear/1000,infos$total_currentyear),2), ifelse(PREF_UNIT_WEIGHT=="Kilogram",i18n("TOTAL_QUANTITY_UNITS_TONS"),tolower(PREF_UNIT_WEIGHT))), icon = icon("fish"), width = 6),
             #CalipseoInfoBox(i18n("INFOBOX_OVERVIEW_LOGBOOK_REPORTING_PERCENTAGE"), style_title = "font-size:58%;",style_value = "font-size:90%;", infos$ratio_reporting_currentyear, icon = icon("percent"), width = 6)
              CalipseoInfoBox(i18n("INFOBOX_OVERVIEW_TOTAL_PARTICIPATING_VESSELS"), style_title = "font-size:58%;",style_value = "font-size:90%;", infos$nb_active_vessel_currentyear, icon = icon("ship"), width = 6)
            )
          )
        )
      )
    })
    
    # #stats by type
    # output$stats_by_type_lastyear_table <- renderDataTable(
    #   infos$stats_by_type_lastyear,
    #   server = FALSE,
    #   escape = FALSE,
    #   rownames = FALSE,
    #   extensions = c("Buttons"), 
    #   options = list(
    #     autoWidth = TRUE,
    #     dom = 'Bfrtip',
    #     deferRender = TRUE,
    #     scroll = FALSE,
    #     buttons = list(
    #       list(extend = 'copy'),
    #       list(extend = 'csv', filename =  sprintf("stats_by_vesseltype_%s", lastyear), title = NULL, header = TRUE),
    #       list(extend = 'excel', filename =  sprintf("stats_by_vesseltype_%s", lastyear), title = NULL, header = TRUE),
    #       list(extend = "pdf", filename = sprintf("stats_by_vesseltype_%s", lastyear), 
    #            title = sprintf("Statistics by vessel type - %s", lastyear), header = TRUE)
    #     ),
    #     exportOptions = list(
    #       modifiers = list(page = "all", selected = TRUE)
    #     )
    #   )
    # )
    # output$stats_by_type_currentyear_table <- renderDataTable(
    #   infos$stats_by_type_currentyear,
    #   server = FALSE,
    #   escape = FALSE,
    #   rownames = FALSE,
    #   extensions = c("Buttons"), 
    #   options = list(
    #     autoWidth = TRUE,
    #     dom = 'Bfrtip',
    #     deferRender = TRUE,
    #     scroll = FALSE,
    #     buttons = list(
    #       list(extend = 'copy'),
    #       list(extend = 'csv', filename =  sprintf("stats_by_vesseltype_%s", currentyear), title = NULL, header = TRUE),
    #       list(extend = 'excel', filename =  sprintf("stats_by_vesseltype_%s", currentyear), title = NULL, header = TRUE),
    #       list(extend = "pdf", filename = sprintf("stats_by_vesseltype_%s", currentyear), 
    #            title = sprintf("Statistics by vessel type - %s", currentyear), header = TRUE)
    #     ),
    #     exportOptions = list(
    #       modifiers = list(page = "all", selected = TRUE)
    #     )
    #   )
    # )
    
  })
  
#Plots

  data_logbooks <- accessLogBooksMultiyear(pool)  
  data_logbooks$quantity<-data_logbooks$quantity/1000
  
  fish_group<-getRemoteReferenceDataset("asfis_enrished")
  fish_group<-subset(fish_group,select=c('3A_Code','ISSCAAP_Group_En'))
  names(fish_group)<-c('species_asfis','ISSCAAP_Group_En')
  
  line_chart_server("gq", label=i18n("GLOBAL_QUANTITY_LABEL"),
                    df=data_logbooks%>%
                      mutate(label="Total")
                      , colDate = "date",colTarget="label",ylab=sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT),colValue="quantity", rank=FALSE,mode='plot+table')
  
  gv_data_formated<-reactiveVal(NULL)
  gv_data_ready<-reactiveVal(FALSE)
  
  gv_format_date <- if(input$gv_granu==i18n("YEARLY")){
    "%Y"
  }else if(input$gv_granu==i18n("MONTHLY")){
    "%Y-%m"
  }else if(input$gv_granu==i18n("WEEKLY")){
    "%Y-%U"
  }
  
  gv_data_formating<-eventReactive(input$gv_granu,{
    df<-data_logbooks%>%
      mutate(date = as.character(format(as.Date(date),format = gv_format_date)))%>%
      group_by(date)%>%
      summarise(nb_vessel=length(unique(regnum)))%>%
      ungroup()
    
    gv_data_formated(df)
    gv_data_ready(TRUE)
  }
  )
  
  output$gv_plot<-renderPlotly({
    gv_data_formating()
    
    if(isTRUE(gv_data_ready())){
    
      p<-gv_data_formated()%>%plot_ly(x = ~date)
      p<-p%>%    
          add_trace(type="scatter",mode="lines+markers",y =~nb_vessel,line = list(simplyfy = F),text = ~sprintf(paste("%s: %s",i18n("GLOBAL_VESSEL_PLOT_UNIT")),date,round(nb_vessel)))
      
        p%>%layout(
          showlegend=F,
          hovermode ='closest',
          xaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            title = i18n("GLOBAL_VESSEL_PLOT_XLAB"),
            zeroline = F
          ),
          yaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            title = i18n("GLOBAL_VESSEL_PLOT_YLAB"),
            zeroline = F
          )
        )
    }
  })
  
  output$gv_table<-DT::renderDT(server = FALSE, {
    
    gv_data_formating()
    
    if(isTRUE(gv_data_ready())){
      
      granu<-switch(gv_format_date,"%Y"=i18n("GRANU_LABEL_YEAR"),
                    "%Y-%m"=i18n("GRANU_LABEL_MONTH"),
                    "%Y-%U"=i18n("GRANU_LABEL_WEEK"))
      
    dt<-gv_data_formated()%>%
      rename(!!granu:=date,
             !!i18n("GLOBAL_VESSEL_LABEL"):=nb_vessel)
      
      DT::datatable(
        dt,
        extensions = c("Buttons"),
        escape = FALSE,
        filter = list(position = 'top',clear =FALSE),
        options = list(
          dom = 'Bfrtip',
          scrollX=TRUE,
          pageLength=5,
          orientation ='landscape',
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  sprintf(i18n("STATISTIC_DATA_EXPORT_FILENAME"),i18n("GLOBAL_VESSEL_LABEL"),granu), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  sprintf(i18n("STATISTIC_DATA_EXPORT_FILENAME"),i18n("GLOBAL_VESSEL_LABEL"),granu), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',orientation = 'landscape',filename = sprintf(i18n("STATISTIC_DATA_EXPORT_FILENAME"),i18n("GLOBAL_VESSEL_LABEL"),granu), 
                 title = sprintf(i18n("STATISTIC_PDF_TITLE"), i18n("GLOBAL_VESSEL_LABEL"),granu), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          ),
          language = list(url = i18n("STATISTIC_TABLE_LANGUAGE"))
        )
      )
    }
  })
  
  output$gv_result<-renderUI({
             tabsetPanel(
               tabPanel(i18n("TABPANEL_PLOT"),plotlyOutput(ns("gv_plot"))%>%withSpinner(type = 4)),
               tabPanel(i18n("TABPANEL_STATISTIC"),DTOutput(ns("gv_table"))%>%withSpinner(type = 4))
             )
  })
  
  line_chart_server("vt", label=i18n("VESSEL_TYPE_LABEL"),df=data_logbooks, colDate = "date",colTarget="vesseltype",ylab=sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT),colValue="quantity", rank=FALSE,mode='plot+table')
  line_chart_server("gt", label=i18n("GEAR_TYPE_LABEL"),df=data_logbooks, colDate = "date",colTarget="fishing_gear",ylab=sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT),colValue="quantity", rank=FALSE,mode='plot+table')
  line_chart_server("sp", label=i18n("SPECIES_LABEL"),df=data_logbooks%>%
                        mutate(text=sprintf("%s-<em>%s</em> (<b>%s</b>)",species_desc,species_sci,species_asfis)),colDate = "date",colTarget="species_desc",ylab=sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT),colValue="quantity",colText="text", rank=TRUE,nbToShow=5,rankLabel=i18n("RANK_LABEL"),mode='plot+table')
  line_chart_server("fg", label=i18n("FISHING_GEAR_LABEL"),df=data_logbooks%>%left_join(fish_group),colDate = "date", colTarget="ISSCAAP_Group_En",ylab=sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT),colValue="quantity", rank=FALSE,mode='plot+table')
  line_chart_server("ls", label=i18n("LANDING_SITES_LABEL"),df=data_logbooks%>%left_join(fish_group),colDate = "date", colTarget="landing_site",ylab=sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT),colValue="quantity", rank=FALSE,mode='plot+table')
  line_chart_server("fz", label=i18n("FISHING_ZONE_LABEL"),df=data_logbooks%>%left_join(fish_group),colDate = "date", colTarget="fishing_zone",ylab=sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT),colValue="quantity", rank=FALSE,mode='plot+table')

 })
  
}