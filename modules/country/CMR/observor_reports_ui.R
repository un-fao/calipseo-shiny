#observor_reports_ui
observor_reports_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "observor_reports",
          tags$h2(i18n("TITLE")),
          fluidRow(
            uiOutput(ns("report_selector")),
          ),
          uiOutput(ns("vessel_box_wrapper"))
  )
  
}