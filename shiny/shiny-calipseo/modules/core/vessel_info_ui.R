vessel_info_ui <- function(id){
  
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_info",

    #1st row including vessel identity and indicators
    div(class='row',style = "margin:0px;", 
        htmlOutput(ns("vessel_header")),br(),
        box(id='vessel-card', status = "primary", width = 6, title = uiOutput(ns("vessel_name")),
          div(id='img-grid', class = "col-md-6",
              uiOutput(ns('vessel_picture')),br(),
              uiOutput(ns('image_source'))
          ),
          div(id='info-panel', class = 'col-md-6',
           div(id='info-tabs',
             tabsetPanel(
               tabPanel('Info', uiOutput(ns("vessel_description"))),
               tabPanel('Registration', uiOutput(ns("vessel_registration")))
             )
           )
          )
        ),
        div(class='col-md-6',id='vessel_indicators', '')
    ),
  
    div(class='row',style = "margin:0px;", id='vertical-panels',
        shinyWidgets::verticalTabsetPanel(
          shinyWidgets::verticalTabPanel('Ownership',box_height='70px' , DT::dataTableOutput(ns("vessel_owners"))),
          shinyWidgets::verticalTabPanel('Licenses',box_height='70px', '#TODO'),
          shinyWidgets::verticalTabPanel('Catches',box_height='70px' ,
                                         tabsetPanel(
                                           
                                           tabPanel('Summary', DT::dataTableOutput(ns("vessel_catch_summary"))),
                                           tabPanel('History', DT::dataTableOutput(ns("vessel_catch_history")),htmlOutput(ns("vessel_catch_datasource"))),
                                           tabPanel('Breakdown by species', 
                                                    fluidRow(
                                                      box(width = 6, height = 460, title = sprintf("Breakdown by species in %s total over years.", appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE, plotlyOutput(ns("catches_piechart"))),
                                                      box(width = 6, height = 460, title = sprintf("Evolution of species breakdown by year in %s.", appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE, plotlyOutput(ns("catches_lineplot")))
                                                      
                                                    ))
                                         ))
      )
    )

  )
}