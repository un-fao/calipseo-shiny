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
                                  tabPanel('Info', uiOutput(ns("vessel_description"))),
                                  tabPanel('Registration', uiOutput(ns("vessel_registration"))),
                                  tabPanel('Characteristics', uiOutput(ns("vessel_characteristics")))
                                )
                            )
                        )
                    )
                  ),
                  div(
                    class = "col-md-6",
                    div(class='row', style = "padding: 15px;",
                      uiOutput(ns('box_status')),
                      uiOutput(ns('box_owner'))
                    ),
                    div(class='row', style = "padding: 15px;",
                      uiOutput(ns('box_license')),
                      uiOutput(ns('box_gears'))
                    )
                  )
              )
              
          ),
          uiOutput(ns('more_indicators')),
          
          div(class='row',style = "margin:0px;", id='vertical-panels',
              shinyWidgets::verticalTabsetPanel(
                shinyWidgets::verticalTabPanel('History',box_height='50px' , DT::dataTableOutput(ns("vessel_history"))),
                shinyWidgets::verticalTabPanel('Ownership',box_height='50px' , DT::dataTableOutput(ns("vessel_owners"))),
                shinyWidgets::verticalTabPanel('Licenses',box_height='50px', DT::dataTableOutput(ns("license_table"))),
                shinyWidgets::verticalTabPanel('Fishing Trips',box_height='50px',uiOutput(ns('warning_vessel_stat_type_fishingtrips')), trip_gantt_ui(ns("fishing_trips_chart"),sliderWidth =25)),
                shinyWidgets::verticalTabPanel('Catches',box_height='50px' ,
                                               uiOutput(ns('warning_vessel_stat_type_catches')),
                                               tabsetPanel(
                                                 tabPanel('Summary', DT::dataTableOutput(ns("vessel_catch_summary"))),
                                                 tabPanel('History', DT::dataTableOutput(ns("vessel_catch_history")),htmlOutput(ns("vessel_catch_datasource"))),
                                                 tabPanel('Breakdown by species', 
                                                          fluidRow(
                                                            tabsetPanel(
                                                              type = "pills",
                                                              tabPanel('Species',line_chart_ui(ns("catches_sp"),sliderWidth =25)),
                                                              tabPanel('Species groups',line_chart_ui(ns("catches_spgroups"),sliderWidth =25))
                                                            )
                                                          )
                                                 )
                                               )
                )
              )
          )
          
  )
}