vessel_info_ui <- function(id){
  
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_info",
          
          #1st row including vessel identity and indicators
          div(class='row',
              htmlOutput(ns("vessel_header")),br(),
              div(class='row',
                  div(
                    class = "col-md-6",
                    box(id='vessel-card', status = "primary", width = 12, title = uiOutput(ns("vessel_name")),
                        div(id='img-grid', class = "col-md-5",
                            uiOutput(ns('vessel_picture'), style='text-align:center;'),br(),
                            uiOutput(ns('image_source'))
                            
                        ),
                        div(id='info-panel', class = 'col-md-7',
                            div(id='info-tabs',
                                tabsetPanel(
                                  tabPanel(i18n("TABPANEL_INFO"), uiOutput(ns("vessel_description"))),
                                  tabPanel(i18n("TABPANEL_REGISTRATION"), uiOutput(ns("vessel_registration"))),
                                  tabPanel(i18n("TABPANEL_CHARACTERISTICS"), uiOutput(ns("vessel_characteristics")))
                                )
                            )
                        )
                    )
                  ),
                  div(
                    class = "col-md-6",
                    div(class='row', style = "padding: 15px;",
                        uiOutput(ns('box_status')),
                        uiOutput(ns('box_license_status'))
                    ),
                    div(class='row', style = "padding: 15px;",
                        uiOutput(ns('box_owner')),
                        uiOutput(ns('box_license'))
                    )
                  )
              )
              
          ),
          uiOutput(ns('more_indicators')),
          
          div(class='row',style = "margin:0px;", id='vertical-panels',
              shinyWidgets::verticalTabsetPanel(
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_HISTORY"),box_height='70px' , DT::dataTableOutput(ns("vessel_history"))),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_OWNERSHIP"),box_height='70px' , DT::dataTableOutput(ns("vessel_owners"))),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_LICENCES"),box_height='70px', DT::dataTableOutput(ns("license_table"))),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_FISHING_TRIPS"),box_height='70px',uiOutput(ns('warning_vessel_stat_type_fishingtrips')), trip_gantt_ui(ns("fishing_trips_chart"),sliderWidth =25)),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_CATCHES"),box_height='70px' ,
                                               uiOutput(ns('warning_vessel_stat_type_catches')),
                                               tabsetPanel(
                                                 tabPanel(i18n("TABPANEL_SUMMARY"), DT::dataTableOutput(ns("vessel_catch_summary"))),
                                                 tabPanel(i18n("TABPANEL_HISTORY"), DT::dataTableOutput(ns("vessel_catch_history")),htmlOutput(ns("vessel_catch_datasource"))),
                                                 tabPanel(i18n("TABPANEL_BREAKDOWN_BY_SPECIES"), 
                                                          fluidRow(
                                                            tabsetPanel(
                                                              type = "pills",
                                                              tabPanel(i18n("TABPANEL_SPECIES"),line_chart_ui(ns("catches_sp"),sliderWidth =25)),
                                                              tabPanel(i18n("TABPANEL_SPECIES_GROUPS"),line_chart_ui(ns("catches_spgroups"),sliderWidth =25))
                                                            )
                                                          )
                                                 )
                                               )
                )
              )
          )
          
  )
}