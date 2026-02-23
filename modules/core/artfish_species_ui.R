#artfish_species_ui
artfish_species_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_species",
    artfishr::artfish_shiny_species_ui(ns("artfish_species"))       
  )
  
}