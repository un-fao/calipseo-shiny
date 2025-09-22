#vessel_overview_ui
vessel_overview_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "vessel_overview",
          fluidRow(
            column(
              width = 12,
              htmlOutput(ns("vessel_overview_info"))
            )
          ),
          div(class = 'row',
              div(class = 'col-md-12',uiOutput(ns("indicators")))
          ),
          bs4Dash::tabsetPanel(
            vertical = TRUE,
            type = "pills",
            #GRAPHS
            tabPanel(
              title = tags$h5(i18n("OVERVIEW_VESSEL_VERTICALTABPANEL_GRAPHS")), box_height='70px',
              div(class = 'row',
                div(class = 'col-md-6',
                    sunburst_chart_ui(
                      id = ns("sb"),
                      title = i18n("OVERVIEW_VESSEL_SUNBURST_BOX_LABEL"), 
                      sliderWidth = 35
                    )
                ),
                div(class = 'col-md-6',
                    pyramid_chart_ui(
                      id = ns("py"),
                      title = i18n("INDIVIDUAL_OVERVIEW_PYRAMID_BOX_LABEL"),
                      sliderWidth =35
                    )
                )
              )
            ),
            #MAPS
            tabPanel(
              title = tags$h5(i18n("OVERVIEW_VESSEL_VERTICALTABPANEL_MAPS")), box_height='70px',
              fluidRow(
                bs4Dash::box(
                  width = 6, height = 610, 
                  title = sprintf(i18n("OVERVIEW_VESSEL_TITLE_HOMEPORT"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,
                  bs4Dash::tabsetPanel(
                    id = "overview_vessel_tab", type = "pills",
                    tabPanel(title = i18n("OVERVIEW_VESSEL_TITLE_MAP"), leafletOutput(ns("map_vessels"), height = 450)),
                    tabPanel(title = i18n("OVERVIEW_VESSEL_TITLE_DATA"), tags$div(DTOutput(ns("map_vessel_data"))), style = "margin:6px;")
                  ),
                  collapsible = FALSE,
                  maximizable = TRUE
                ),
                bs4Dash::box(
                  width = 6, height = 610, 
                  title = sprintf(i18n("OVERVIEW_VESSELTYPE_TITLE_LANDINGSITE"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,
                  bs4Dash::tabsetPanel(
                    id = "overview_vessel_tab2", type = "pills",
                    tabPanel(title = i18n("OVERVIEW_VESSEL_TITLE_MAP"), leafletOutput(ns("map_vessels2"), height = 450)),
                    tabPanel(title = i18n("OVERVIEW_VESSEL_TITLE_DATA"), tags$div(DTOutput(ns("map_vessel_data_breakdown"))), style = "margin:6px;")
                  ),
                  collapsible = FALSE,
                  maximizable = TRUE
                )
              )
            ),
            #DYNAMIC TABLE
            tabPanel(
              title = tags$h5(i18n("OVERVIEW_VESSEL_VERTICALTABPANEL_TABLE")), box_height='70px',
              div(class = 'row',
                div(class = 'col-md-12', height = '800px',
                    pretty_table_ui(
                      id = ns("pt"),
                      title = i18n("OVERVIEW_VESSEL_TABLE_BOX_LABEL"),
                      sliderWidth = 35,
                      sliderOpen = T
                    )
                )
              )
            )
          )
        )
  
}