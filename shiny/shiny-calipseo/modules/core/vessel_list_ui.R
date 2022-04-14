#vessel_list_ui
vessel_list_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_list",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("vessel_list_info"))
            )
          ),
          fluidRow(
            box(width = 12, withSpinner(DT::dataTableOutput(ns("vessel_list"))))
          )
  )
  
}
