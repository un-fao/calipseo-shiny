#artfish_sws_report_ui
artfish_sws_report_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_sws_report",
    artfishr::artfish_shiny_report_ui(ns("artfish_sws_report"))
  )
  
}