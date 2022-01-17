#landings1_maps_ui
landings1_maps_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(
    tabName = "landings1_maps",
    fluidRow(
      box(
        width = 6,
        htmlOutput(ns("landings1_maps_info"))
      ),
      box(width = 6,
          selectizeInput(ns("year_map_total"), label = i18n("LANDINGS1_MAP_YEAR_LABEL"), 
                         choice = accessAvailableYears(pool), selected = NULL, 
                         options = list(
                           placeholder = i18n("LANDINGS1_MAP_YEAR_PLACEHOLDER_LABEL"),
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
      )
    ),
    fluidRow(
      box(width = 4, height = 369, title = "LAN", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_LAN"), height = 325)),
      box(width = 4, height = 369, title = "VAL", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_VAL"), height = 325)),
      box(width = 4, height = 369, title = "TRP", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_TRP"), height = 325))
    ),
    fluidRow(
      box(width = 4, height = 369, title = "L/T", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_LT"), height = 325)),
      box(width = 4, height = 369, title = "V/T", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_VT"), height = 325)),
      box(width = 4, height = 369, title = "P/K", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_PK"), height = 325))
    )
  )
  
}