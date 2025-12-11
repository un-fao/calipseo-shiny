#observer_list_server
observer_list_server <- function(id,parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    reports_info <- accessObserverReportsSummary(pool)
    
    print(head(reports_info))
    
    reports_info$embarkation_start <- as.POSIXct(as.character(reports_info$embarkation_start))
    attr(reports_info$embarkation_start, "tzone") <- appConfig$country_profile$timezone
    reports_info$embarkation_end <- as.POSIXct(as.character(reports_info$embarkation_end))
    attr(reports_info$embarkation_end, "tzone") <- appConfig$country_profile$timezone
    reports_info$observation_start <- as.POSIXct(as.character(reports_info$observation_start))
    attr(reports_info$observation_start, "tzone") <- appConfig$country_profile$timezone
    reports_info$observation_end <- as.POSIXct(as.character(reports_info$observation_end))
    attr(reports_info$observation_end, "tzone") <- appConfig$country_profile$timezone
    reports_info$year<-year(reports_info$embarkation_end)
    
    current_year<-Sys.time()
    current_year<- as.POSIXct(as.character(current_year))
    attr(current_year, "tzone") <- appConfig$country_profile$timezone
    current_year<-year(current_year)
    
    print(head(reports_info))
    
      req(reports_info)
      summary_report<-reports_info%>%
        select(-c(observation_start,observation_end,trip_start,trip_end,year))%>%
        mutate(detail="Detail")
    
    output$reports_summary <- renderDT(
      summary_report,
      container = initDTContainer(summary_report),
      server = FALSE,
      escape = FALSE,
      rownames = FALSE,
      extensions = c("Buttons"),
      filter = list(position = 'top', clear = FALSE),
      
      options = list(
        autoWidth = FALSE,
        dom = 'Bfrtip',
        deferRender = TRUE,
        scroll = FALSE,
        buttons = list(
          list(extend = 'copy'),
          list(extend = 'csv', filename = i18n("SUMMARY") , title = NULL, header = TRUE),
          list(extend = 'excel', filename =  i18n("SUMMARY"), title = NULL, header = TRUE),
          list(extend = "pdf", title = i18n("SUMMARY"), header = TRUE, orientation = "landscape")
        ),
        exportOptions = list(
          modifiers = list(page = "all", selected = TRUE)
        ),
        language = list(url = i18n("TABLE_LANGUAGE")),
        pageLength = 10,
        initComplete = js
      )
    )
    
  })
}
