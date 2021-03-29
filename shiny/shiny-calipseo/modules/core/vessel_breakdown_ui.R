#vessel_breakdown_ui
vessel_breakdown_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_breakdown",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("vessel_breakdown_info"))
            )
          ),
          fluidRow(
            box(width = 6, height = 540, title = sprintf("Breakdown of vessels in %s per vessel type", appConfig$country_profile$country), status = "primary", solidHeader= TRUE, plotlyOutput(ns("rep_vessels"))),
            box(width = 6, height = 540, title = sprintf("Breakdown of vessels in %s per home port", appConfig$country_profile$country), status = "primary", solidHeader= TRUE, leafletOutput(ns("map_vessels"), height = 490))
          )
  )
  
}