individual_info_ui <- function(id){
  
  
  ns <- NS(id)
  
  tabItem(tabName = "individual_info",
          
          #1st row including individual identity and indicators
          div(class='row',
              htmlOutput(ns("individual_header")),br(),
              div(class='row',
                  div(
                    class = "col-md-6",
                    box(id='individual-card', status = "primary", width = 12, title = uiOutput(ns("individual_name")),
                        div(id='img-grid', class = "col-md-5",
                            uiOutput(ns('individual_picture'), style='text-align:center;')
                           
                            
                        ),
                        div(id='info-panel', class = 'col-md-7',
                            div(id='individual_info-tabs',
                                tabsetPanel(
                                  tabPanel(i18n("TABPANEL_INFO"), uiOutput(ns("individual_description")))
                                )
                            )
                        )
                    )
                  )
                  
              ),
              div(class='row',style = "margin:0px;", id='vertical-panels-individual-description',
                  shinyWidgets::verticalTabsetPanel(
                    shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_INDIVIDUAL_ROLES"),box_height='70px',box(width = 12, height = 480, title = i18n("INDIVIDUAL_ROLES_TITLE"),DT::dataTableOutput(ns("individual_roles"))),
                                                   sliderWidth =25)
                    ))
              
              
              
          )
          
  )
}