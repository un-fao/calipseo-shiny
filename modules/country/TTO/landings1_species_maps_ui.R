#landings1_species_maps_ui
landings1_species_maps_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "landings1_species_maps",
    fluidRow(
      bs4Dash::box(
        width = 6,
        htmlOutput(ns("landings1_species_maps_info")),
        collapsible = FALSE
      ),
      bs4Dash::box(width = 6,
          div(class = "col-md-6",
            uiOutput(ns("mode_selector"))
          ),
          div(class = "col-md-6",
            uiOutput(ns("year_map_species_selector"))
          ),
          collapsible = FALSE
      )
    ),
    fluidRow(
      bs4Dash::box(width = 6, height = 645, title = "LAN", status = "primary", solidHeader = TRUE, leafletOutput(ns("map_species_LAN"), height = 600), collapsible = FALSE),
      bs4Dash::box(width = 6, height = 645, title = "VAL", status = "primary", solidHeader = TRUE, leafletOutput(ns("map_species_VAL"), height = 600), collapsible = FALSE)
    )
  )
  
}