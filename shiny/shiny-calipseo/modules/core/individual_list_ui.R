#individual_list_ui
individual_list_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "individual_list",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("individual_list_info"))
            )
          ),
          fluidRow(
            box(width = 12, DT::dataTableOutput(ns("individual_list")))
          )
  )
  
}

