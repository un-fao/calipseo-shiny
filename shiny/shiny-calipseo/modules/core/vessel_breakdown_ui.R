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
            
            box(width = 6, height = 480, title = sprintf("Breakdown of vessels in %s per vessel type", appConfig$country_profile$country), status = "primary", solidHeader= TRUE, plotlyOutput(ns("rep_vessels"))),
            
            box(width = 6, height = 480, title = sprintf("Data on Breakdown of vessels in %s per vessel type", appConfig$country_profile$country), status = "primary", solidHeader= TRUE, DT::dataTableOutput(ns("rep_vessels_data")))
          ),
          fluidRow(
            
            box(width = 6, height = 610, title = sprintf("Breakdown of vessels in %s per home port", appConfig$country_profile$country), status = "primary", solidHeader= TRUE,
            
              
              tabsetPanel(
                id = "breakdown_vessel_tab", type = "pills",
                tabPanel(title = "Map", leafletOutput(ns("map_vessels"), height = 450)),
                tabPanel(title = "Data", tags$div(DTOutput(ns("map_vessel_data"))), style = "margin:6px;")
              )
            
            
            ),
            
            box(width = 6, height = 610, title = sprintf("Breakdown of vessels in %s per home port", appConfig$country_profile$country), status = "primary", solidHeader= TRUE,
                
                tabsetPanel(
                  id = "breakdown_vessel_tab2", type = "pills",
                  tabPanel(title = "Map", leafletOutput(ns("map_vessels2"), height = 450)),
                  tabPanel(title = "Data", tags$div(DTOutput(ns("map_vessel_data_breakdown"))), style = "margin:6px;")
                )#,
                
                #plotlyOutput(ns("rep_vessels_home_port"))
                
                
                )

          )
  )
  
}