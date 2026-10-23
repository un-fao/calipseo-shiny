#landings1_species_maps_ui
landings1_species_maps_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "landings1_species_maps",
    uiOutput(ns("main"))
  )
  
}