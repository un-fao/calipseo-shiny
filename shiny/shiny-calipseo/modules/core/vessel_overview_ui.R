#vessel_overview_ui
vessel_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_overview",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("vessel_overview_info"))
            )
          ),
          div(class = 'row',
              div(class = 'col-md-6',sunburst_chart_ui(ns("sb"),title=i18n("OVERVIEW_VESSEL_SUNBURST_BOX_LABEL"),sliderWidth =25)),
          ),
          div(class = 'row',
              div(class = 'col-md-12',pretty_table_ui(ns("pt"),title=i18n("OVERVIEW_VESSEL_TABLE_BOX_LABEL"),sliderWidth =25,sliderOpen=T))
          ),
          fluidRow(
            
            box(width = 6, height = 610, title = sprintf(i18n("OVERVIEW_VESSEL_TITLE_HOMEPORT"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,
                
                
                tabsetPanel(
                  id = "overview_vessel_tab", type = "pills",
                  tabPanel(title = i18n("OVERVIEW_VESSEL_TITLE_MAP"), leafletOutput(ns("map_vessels"), height = 450)),
                  tabPanel(title = i18n("OVERVIEW_VESSEL_TITLE_DATA"), tags$div(DTOutput(ns("map_vessel_data"))), style = "margin:6px;")
                )
                
                
            ),
            
            box(width = 6, height = 610, title = sprintf(i18n("OVERVIEW_VESSELTYPE_TITLE_LANDINGSITE"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,
                
                tabsetPanel(
                  id = "overview_vessel_tab2", type = "pills",
                  tabPanel(title = i18n("OVERVIEW_VESSEL_TITLE_MAP"), leafletOutput(ns("map_vessels2"), height = 450)),
                  tabPanel(title = i18n("OVERVIEW_VESSEL_TITLE_DATA"), tags$div(DTOutput(ns("map_vessel_data_breakdown"))), style = "margin:6px;")
                )
                
                
            )
            
          )
  )
  
}