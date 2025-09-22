vessel_info_ui <- function(id){
  
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "vessel_info",
          
          #1st row - Back to list of vessels button
          fluidRow(column(width = 3, htmlOutput(ns("vessel_header")))),
          #2nd row including vessel identity and indicators
          fluidRow(
            #vessel information card
            bs4Dash::box(
              id = 'vessel-card', status = "primary", width = 6, 
              title = uiOutput(ns("vessel_name")),
              div(class = 'row',
                div(
                  id = 'img-grid', class = "col-md-5",
                  uiOutput(ns('vessel_picture'), style='text-align:center;'),br(),
                  uiOutput(ns('image_source'))
                      
                ),
                div(
                  id = 'info-panel', class = 'col-md-7',
                  div(id = 'info-tabs',
                      bs4Dash::tabsetPanel(
                        tabPanel(i18n("TABPANEL_INFO"), uiOutput(ns("vessel_description"))),
                        tabPanel(i18n("TABPANEL_REGISTRATION"), uiOutput(ns("vessel_registration"))),
                        tabPanel(i18n("TABPANEL_CHARACTERISTICS"), uiOutput(ns("vessel_characteristics")))
                      )
                  )
                )
              ),
              collapsible = FALSE
            ),
            #vessel indicators
            column(width = 6,
              uiOutput(ns('main_indicators'))
              #div(class='row', style = "padding: 15px;",
              #fluidRow(
              #     uiOutput(ns('box_status')),
              #     uiOutput(ns('box_license_status')),
              # #),
              # #div(class='row', style = "padding: 15px;",
              # #fluidRow(
              #     uiOutput(ns('box_owner')),
              #     uiOutput(ns('box_license'))
              #)
            )
          ),
          #3rd row - More indicators on this vesels
          uiOutput(ns('more_indicators')),
          #4th row - Vertical tabset panel
          bs4Dash::box(
            width = 12,
            title = NULL,
            bs4Dash::tabsetPanel(
              vertical = TRUE,
              type = "pills",
              #Vessel history
              tabPanel(
                title = tags$h5(i18n("VERTICALTABPANEL_HISTORY")), box_height = '70px' , 
                DT::dataTableOutput(ns("vessel_history"))
              ),
              #Vessel ownership
              tabPanel(
                title = tags$h5(i18n("VERTICALTABPANEL_OWNERSHIP")), box_height = '70px',
                DT::dataTableOutput(ns("vessel_owners"))
              ),
              #Vessel licenses
              tabPanel(
                title = tags$h5(i18n("VERTICALTABPANEL_LICENCES")), box_height = '70px',
                DT::dataTableOutput(ns("license_table"))
              ),
              #Vessel fishing trips
              tabPanel(
                title = tags$h5(i18n("VERTICALTABPANEL_FISHING_TRIPS")), box_height = '70px',
                uiOutput(ns('warning_vessel_stat_type_fishingtrips')), 
                trip_gantt_ui(
                  id = ns("fishing_trips_chart"),
                  sliderWidth = 25
                )
              ),
              #Vessel catches
              tabPanel(
                title = tags$h5(i18n("VERTICALTABPANEL_CATCHES")), box_height = '70px' ,
                uiOutput(ns('warning_vessel_stat_type_catches')),
                bs4Dash::tabsetPanel(
                  #Catch summary
                  tabPanel(
                    title = i18n("TABPANEL_SUMMARY"),
                    DT::dataTableOutput(ns("vessel_catch_summary"))
                  ),
                  #Catch history
                  tabPanel(
                    title = i18n("TABPANEL_HISTORY"), 
                    DT::dataTableOutput(ns("vessel_catch_history")),
                    htmlOutput(ns("vessel_catch_datasource"))
                  ),
                  #Catch breakdown by species
                  tabPanel(
                    title = i18n("TABPANEL_BREAKDOWN_BY_SPECIES"), 
                    fluidRow(
                      bs4Dash::tabsetPanel(
                        type = "pills",
                        #by species
                        tabPanel(
                          title = i18n("TABPANEL_SPECIES"),
                          line_chart_ui(
                            id = ns("catches_sp"),
                            sliderWidth = 25
                          )
                        ),
                        #by species group
                        tabPanel(
                          title = i18n("TABPANEL_SPECIES_GROUPS"),
                          line_chart_ui(
                            id = ns("catches_spgroups"),
                            sliderWidth =25
                          )
                        )
                      )
                    )
                 )
                )
              )
            )
          )
          
  )
}