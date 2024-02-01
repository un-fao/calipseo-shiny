#logbooks_export_ui
logbooks_export_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "logbooks_export",
          tags$h2(i18n("LOGBOOKS_EXPORT_TITLE")),
          fluidRow(
            div(
              class = "col-md-2",
              uiOutput(ns("year_wrapper"))
            ),
            div(
              class = "col-md-2",
              uiOutput(ns("aggregate_wrapper"))
            ),
            div(
              class = "col-md-2",
              uiOutput(ns("task_wrapper"))
            ),
            div(
              class = "col-md-2",
              uiOutput(ns("format_wrapper"))
            )
          ),
          fluidRow(
            div(
              class = "col-md-2",
              uiOutput(ns("btn_wrapper"))
            )
          )
  )
  
}