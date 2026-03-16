#artfish_species_explorer_ui
artfish_species_explorer_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_species_explorer",
    tagList(
      waiter::useWaiter(),
      waiter::useHostess(),
    artfishr::artfish_shiny_species_ui(ns("artfish_species_explorer"))
    )
  )
  
}