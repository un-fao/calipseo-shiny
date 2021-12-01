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
        x<-x/1000
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
          infoBox(sprintf("Total quantity (%s)", lastyear), paste(round(infos$total_lastyear/1000,2), "tons"), icon = icon("fish"), fill = TRUE, width = 6),
          infoBox(sprintf("Total quantity (%s)", currentyear), paste(round(infos$total_currentyear/1000,2), "tons"), icon = icon("fish"), fill = TRUE, width = 6)
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
  
#Plots

  data_logbooks <- accessLogBooksMultiyear(pool)  
  
  fish_group<-getRemoteReferenceDataset("asfis_enrished")
  fish_group<-subset(fish_group,select=c('3A_Code','ISSCAAP_Group_En'))
  names(fish_group)<-c('species_asfis','ISSCAAP_Group_En')
  
  line_chart_server("vt", data_logbooks, colDate = "date",colTarget="vesseltype",colValue="quantity", rank=FALSE)
  line_chart_server("gt", data_logbooks, colDate = "date",colTarget="fishing_gear",colValue="quantity", rank=FALSE)
  line_chart_server("sp", data_logbooks%>%
                        mutate(text=sprintf("%s-<em>%s</em>(<b>%s</b>)",species_desc,species_sci,species_asfis)),colDate = "date",colTarget="species_desc",colValue="quantity",colText="text", rank=TRUE,nbToShow=5,rankLabel="Display x most caught species:")
  line_chart_server("fg", data_logbooks%>%left_join(fish_group),colDate = "date", colTarget="ISSCAAP_Group_En",colValue="quantity", rank=FALSE)
  line_chart_server("ls", data_logbooks%>%left_join(fish_group),colDate = "date", colTarget="landing_site",colValue="quantity", rank=FALSE)
  line_chart_server("fz", data_logbooks%>%left_join(fish_group),colDate = "date", colTarget="fishing_zone",colValue="quantity", rank=FALSE)

}