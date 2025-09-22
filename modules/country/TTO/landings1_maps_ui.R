#landings1_maps_ui
landings1_maps_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "landings1_maps",
    fluidRow(
      bs4Dash::box(
        width = 6,
        htmlOutput(ns("landings1_maps_info")),
        collapsible = FALSE
        
      ),
      bs4Dash::box(width = 6,
          div(class = "col-md-6",
              uiOutput(ns("mode_selector"))
          ),
          div(class = "col-md-6",
              uiOutput(ns("year_map_total_selector"))
          ),
          collapsible = FALSE   
      )
    ),
    fluidRow(
      bs4Dash::box(width = 4, height = 369, title = "LAN", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_LAN"), height = 325), collapsible = FALSE),
      bs4Dash::box(width = 4, height = 369, title = "VAL", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_VAL"), height = 325), collapsible = FALSE),
      bs4Dash::box(width = 4, height = 369, title = "TRP", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_TRP"), height = 325), collapsible = FALSE)
    ),
    fluidRow(
      bs4Dash::box(width = 4, height = 369, title = "L/T", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_LT"), height = 325), collapsible = FALSE),
      bs4Dash::box(width = 4, height = 369, title = "V/T", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_VT"), height = 325), collapsible = FALSE),
      bs4Dash::box(width = 4, height = 369, title = "P/K", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_PK"), height = 325), collapsible = FALSE)
    )
  )
  
}