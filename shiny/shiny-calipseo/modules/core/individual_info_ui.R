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
                  
              )
              
          )
          
  )
}