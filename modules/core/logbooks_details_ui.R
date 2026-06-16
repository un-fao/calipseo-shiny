#logbooks_details_ui
logbooks_details_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "logbooks_details",
    uiOutput(ns("main"))
  )
  
}