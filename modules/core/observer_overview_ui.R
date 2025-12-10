#observer_overview_ui
observer_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "observer_overview",
          tags$h2(i18n("OBSERVER_OVERVIEW_TITLE")),
            uiOutput(ns("first_line_indicators")),
            uiOutput(ns("second_line_indicators")),
            uiOutput(ns("third_line_indicators")),
          fluidRow(
            DT::dataTableOutput(ns("reports_summary"))
          ),
          fluidRow(
         #   uiOutput(ns("timeplot"))
          )
  )
  
}