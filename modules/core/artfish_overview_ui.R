#artfish_overview_ui
artfish_overview_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_overview",
    artfishr::artfish_shiny_overview_ui(ns("artfish_overview"))
  )
}
         