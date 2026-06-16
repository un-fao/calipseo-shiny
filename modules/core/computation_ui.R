#computation_ui
computation_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "computation",
    uiOutput(ns("main"))
  )
}