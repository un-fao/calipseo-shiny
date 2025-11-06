#observor_overview_server
observor_overview_server <- function(id, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    reports_info <- accessObserverReportsSummary(pool)
    
    
    
    output$indicators <- renderUI({
      req(reports_info)
      fluidRow(
          shiny::tagList(
            CalipseoInfoBox(i18n("INFOBOX_TITLE_REPORTS"), length(unique(reports_info$ID)), icon = icon("file")),
            CalipseoInfoBox(i18n("INFOBOX_TITLE_VESSELS"), length(unique(reports_info$VESSEL_NAME)), icon = icon("ship")),
            CalipseoInfoBox(i18n("INFOBOX_TITLE_OBSERVERS"), length(unique(reports_info$OBSERVER_FULL_NAME)), icon = icon("user")),
            CalipseoInfoBox(i18n("INFOBOX_TITLE_LANDINGS"), "-", icon = icon("location-dot")),
            CalipseoInfoBox(i18n("INFOBOX_TITLE_YEARS"), "-", icon = icon("history")),
            CalipseoInfoBox(i18n("INFOBOX_TITLE_NODATE"), nrow(subset(reports_info,is.na(OBSERVATION_PERIOD_START)|is.na(OBSERVATION_PERIOD_END))), icon = icon("calendar-xmark"))
        )
      )
    })
    
    
      req(reports_info)
      summary_report<-reports_info%>%
        select(ID,VESSEL_NAME,OBSERVER_FULL_NAME,OBSERVATION_PERIOD_START,OBSERVATION_PERIOD_END)%>%
        mutate(TOTAL_WEIGHT_LANDED= "")
      print(summary_report)
    
    
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
    
    output$timeplot <- renderUI({
      req(reports_info)
      time_table<-subset(reports_info,!is.na(OBSERVATION_PERIOD_START)&!is.na(OBSERVATION_PERIOD_END))
      
      if(nrow(time_table)>0){
        time_table<-time_table%>%
          mutate(year=year(OBSERVATION_PERIOD_END))%>%
          group_by(year)%>%
          summarise(n=length(ID))%>%
          ungroup()
        
        #TEST barplot when data available
      }else{
      div(paste0("(",i18n("PLOT_NODATE_DISCLAMER"),")"))
      }
    })    
  })
}
