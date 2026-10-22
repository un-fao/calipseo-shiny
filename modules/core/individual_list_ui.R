#individual_list_ui
individual_list_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "individual_list",
    uiOutput(ns("main"))
  )
  
}

