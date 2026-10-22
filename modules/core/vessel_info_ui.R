vessel_info_ui <- function(id){
  
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "vessel_info",
    uiOutput(ns("main"))
  )
}