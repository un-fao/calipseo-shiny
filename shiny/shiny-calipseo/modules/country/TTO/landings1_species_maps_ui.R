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
<<<<<<< HEAD
        uiOutput(ns("year_map_species_selector"))
=======
          selectizeInput(ns("year_map_species"), label = i18n("LANDINGS1_SPECIES_MAPS_YEAR_LABEL"), 
                         choice = getReleasePeriods(config = appConfig, id = "artisanal_fisheries_landings1")$year, selected = NULL, 
                         options = list(
                           placeholder = i18n("LANDINGS1_SPECIES_MAPS_YEAR_PLACEHOLDER_LABEL"),
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
>>>>>>> 8de4505303af0d514cbfd2240b26e8f07797d595
      )
    ),
    fluidRow(
      box(width = 6, height = 645, title = "LAN", status = "primary", solidHeader = TRUE, leafletOutput(ns("map_species_LAN"), height = 600)),
      box(width = 6, height = 645, title = "VAL", status = "primary", solidHeader = TRUE, leafletOutput(ns("map_species_VAL"), height = 600))
    )
  )
  
}