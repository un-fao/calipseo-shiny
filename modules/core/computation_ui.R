#computation_ui
computation_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(
    tabName = "computation",
    fluidRow(
      div(
        width = 6, style = "margin:12px;",
        htmlOutput(ns("computation_info"))
      ),
      box(width = 6, title = i18n("COMPUTATION_RUN_LABEL"),
          uiOutput(ns("computation_by"))
      ),
    ),
    fluidRow(
      box(width = 12, title = i18n("COMPUTATION_RESULTS_LABEL"), DT::dataTableOutput(ns("computation_results")) )
    )
  )
}