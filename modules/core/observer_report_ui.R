#observer_report_ui
observer_report_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "observer_report",
          #fluidRow(column(width = 3, htmlOutput(ns("report_header")))),
          tags$h2(i18n("OBSERVER_REPORT_TITLE")),
          uiOutput(ns("report_selector")),
          uiOutput(ns("results"))
  )
  
}