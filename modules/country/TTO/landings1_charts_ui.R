#landings1_charts_ui
landings1_charts_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "landings1_charts",
    uiOutput(ns("main"))
  )
  
}