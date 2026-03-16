#artfish_fishing_unit_explorer_ui
artfish_fishing_unit_explorer_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_fishing_unit_explorer",
    tagList(
        artfishr::artfish_shiny_fishing_unit_ui(ns("artfish_fishing_unit_explorer"))
    )
  )
}