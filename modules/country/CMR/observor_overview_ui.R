#observor_overview_ui
observor_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "observor_overview",
          tags$h2(i18n("OBSERVER_OVERVIEW_TITLE")),
          fluidRow(
            uiOutput(ns("indicators"))
          ),
          fluidRow(
            DT::dataTableOutput(ns("reports_summary"))
          ),
          fluidRow(
            uiOutput(ns("timeplot"))
          )
  )
  
}