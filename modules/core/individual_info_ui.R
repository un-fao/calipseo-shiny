individual_info_ui <- function(id){
  
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "individual_info",
    uiOutput(ns("main"))
  )
}