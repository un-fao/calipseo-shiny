#logbooks_overview_ui
logbooks_overview_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "logbooks_overview",
    uiOutput(ns("main"))      
  )
  
}