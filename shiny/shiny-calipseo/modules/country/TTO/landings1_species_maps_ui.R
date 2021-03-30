#landings1_species_maps_ui
landings1_species_maps_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(
    tabName = "landings1_species_maps",
    fluidRow(
      box(
        width = 6,
        htmlOutput(ns("landings1_species_maps_info"))
      ),
      box(width = 6,
          selectizeInput(ns("year_map_species"), label = "Year", 
                         choice = accessAvailableYears(pool), selected = NULL, 
                         options = list(
                           placeholder = 'Select a year',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
      )
    ),
    fluidRow(
      box(width = 6, height = 645, title = "LAN", status = "primary", solidHeader = TRUE, leafletOutput(ns("map_species_LAN"), height = 600)),
      box(width = 6, height = 645, title = "VAL", status = "primary", solidHeader = TRUE, leafletOutput(ns("map_species_VAL"), height = 600))
    )
  )
  
}