#observer_list_ui
observer_list_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "observer_list",
          tags$h2(i18n("OBSERVER_LIST_TITLE")),
          fluidRow(
            DT::dataTableOutput(ns("reports_summary"))
          )
  )
  
}