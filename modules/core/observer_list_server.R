#observer_list_server
observer_list_server <- function(id,parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    reports_info <- accessObserverReportsSummary(pool)
    req(reports_info)
    
    reports_info$embarkation_start <- as.POSIXct(as.character(reports_info$embarkation_start))
    attr(reports_info$embarkation_start, "tzone") <- appConfig$country_profile$timezone
    reports_info$embarkation_end <- as.POSIXct(as.character(reports_info$embarkation_end))
    attr(reports_info$embarkation_end, "tzone") <- appConfig$country_profile$timezone
      
    icon_yes_no <- function(value) {
      if (is.na(value) || value == 0) {
        paste0(
          as.character(icon("times", style = "color:red;")),
          " ",
          i18n("OBSERVER_LIST_NO")
        )
      } else {
        paste0(
          as.character(icon("check", style = "color:green;")),
          " ",
          i18n("OBSERVER_LIST_YES")
        )
      }
    }
    
    icon_missing_chr <- function(value) {
      if (is.na(value) || trimws(value) == "") {
        paste0(
        as.character(icon("triangle-exclamation", style = "color:darkorange;")),
        " ",
        i18n("OBSERVER_LIST_UNKNOWN")
        )
      } else {
        value
      }
    }
    
    icon_missing_num <- function(value, dec = 0) {
      if (is.na(value)) {
        paste0(
          as.character(icon("triangle-exclamation", style = "color:darkorange;")),
          " ",
          i18n("OBSERVER_LIST_UNKNOWN")
        )
      } else {
        as.character(round(as.numeric(value), dec))
      }
    }
      
    summary_report_dt <- reports_info %>%
      select(
        report_id, vessel_name, observer_name,
        embarkation_start, embarkation_port,
        embarkation_end, disembarkation_port,
        days_at_sea, nb_fishing_operations,
        logbook_linked, biological_present,
        total_catch, total_catch_unit,
        total_discard, total_discard_unit
      ) %>%
      mutate(
        vessel_name = purrr::map_chr(vessel_name, icon_missing_chr),
        observer_name = purrr::map_chr(observer_name, icon_missing_chr),
        
        logbook_linked = purrr::map_chr(logbook_linked, icon_yes_no),
        biological_present = purrr::map_chr(biological_present, icon_yes_no),
        
        total_catch = purrr::map2_chr(
          total_catch, total_catch_unit,
          ~ if (is.na(.x)) {
            as.character(icon_missing_num(.x, 0))
          } else {
            paste0(round(.x, 0), " ", .y)
          }
        ),
        
        total_discard = purrr::map2_chr(
          total_discard, total_discard_unit,
          ~ if (is.na(.x)) {
            as.character(icon_missing_num(.x, 0))
          } else {
            paste0(round(.x, 0), " ", .y)
          }
        )
      )%>%
      select(-c(total_catch_unit,total_discard_unit))
    
    
      
      # Dynamic access to report detail
      # TO FIX
      # report_selection <- reactiveVal(NULL)
      # summary_report$Details <- sprintf(
      #   '<a href="#" onclick="Shiny.setInputValue(\'%s\', %d, {priority: \'event\'});">%s</a>',
      #   ns("access_observer_report"), summary_report$report_id, i18n("OBSERVER_LIST_TABLE_RECORD_ACCESS")
      # )
      # observeEvent(input$access_observer_report, {
      #   DEBUG("Click to access details for vessel '%s'", input$access_observer_report)
      #   report_selection(input$access_observer_report)
      # })
      # observeEvent(report_selection(),{
      #   if(is.null(report_selection())){
      #     parent.session$userData$record_selection = NULL
      #     isolate({ bs4Dash::updateTabItems(parent.session, "calipseo-tabs", "observer_list") })
      #   }else{
      #     parent.session$userData$record_selection = report_selection()
      #     isolate({ bs4Dash::updateTabItems(parent.session, "calipseo-tabs", "observer_report") })
      #   }
      # })
      
    output$reports_summary <- renderDT(
      summary_report_dt,
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
          list(extend = 'csv', filename = i18n("SUMMARY"), title = NULL, header = TRUE),
          list(extend = 'excel', filename = i18n("SUMMARY"), title = NULL, header = TRUE),
          list(extend = "pdf", title = i18n("SUMMARY"), header = TRUE, orientation = "landscape")
        ),
        exportOptions = list(
          modifiers = list(page = "all", selected = TRUE)
        ),
        language = list(url = i18n("TABLE_LANGUAGE")),
        pageLength = 10,
        initComplete = js
      ),
      colnames = c(
        i18n("OBSERVER_LIST_REPORT_ID"),
        i18n("OBSERVER_LIST_VESSEL_NAME"),
        i18n("OBSERVER_LIST_OBSERVER_NAME"),
        i18n("OBSERVER_LIST_EMBARKATION_START"),
        i18n("OBSERVER_LIST_EMBARKATION_PORT"),
        i18n("OBSERVER_LIST_EMBARKATION_END"),
        i18n("OBSERVER_LIST_DISEMBARKATION_PORT"),
        i18n("OBSERVER_LIST_DAYS_AT_SEA"),
        i18n("OBSERVER_LIST_NB_FISHING_OPERATIONS"),
        i18n("OBSERVER_LIST_LOGBOOK_LINKED"),
        i18n("OBSERVER_LIST_BIOLOGICAL_PRESENT"),
        i18n("OBSERVER_LIST_TOTAL_CATCH"),
        i18n("OBSERVER_LIST_TOTAL_DISCARD")
      )
    )
    
    
  })
}
