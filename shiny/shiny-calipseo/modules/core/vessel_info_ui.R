vessel_info_ui <- function(id){
  
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_info",

div(class='row',
         
         htmlOutput(ns("vessel_header")),br(),
        
         div(class='col-md-5',id='card',style='background-color:#f7f4ff;
             margin-left:15px;
             margin-right:15px;
             padding-left:15px;
             padding-top:15px;
             padding-bottom:15px;
             border:solid 1px;
             border-color:#ddd;
             width:48%;',
             
         div(class = 'col-sm-6',id='img-grid',style='background-color:#f7f4ff;
             text-align: center;
             margin-left:15px;
             margin-right:15px;
             padding-right:15px;
             padding-left:15px;
             padding-top:15px;
             padding-bottom:15px;
             border:solid 1px;
             border-color:#ddd;',
                
                
              uiOutput(ns('vessel_picture')),br(),
             
             uiOutput(ns('image_source'))
             
                         
                  ),br(),
                  
                  div(class = 'col-sm-3',id='info-panel', style='padding-left: 15px;
                      background-color:#f7f4ff;
                      border:solid 1px;
                      border-color:#ddd;',
                         
                         div(class='row',id='info-tabs',style='padding-left: 15px;
                             padding-top: 15px;
                             width:200px;
                             background-color:#f7f4ff',
                           tabsetPanel(
                             
                             tabPanel('Info',
                                      uiOutput(ns("vessel_description"),style = "font-weight:bold;")),
                             tabPanel('Registration',uiOutput(ns("vessel_registration"),style = "font-weight:bold;"))
                           )
                           
                         )
                  
                  
                ),
         
        ),div(class='col-md-5',id='licenses-panel', '#TODO')
         
         
),


div(class='row',id='vertical-panels',
         
         shinyWidgets::verticalTabsetPanel(
         shinyWidgets::verticalTabPanel('Ownership',box_height='70px' , DT::dataTableOutput(ns("vessel_owners"))),
         shinyWidgets::verticalTabPanel('Licenses',box_height='70px', '#TODO'),
         shinyWidgets::verticalTabPanel('Catches',box_height='70px' ,
                            tabsetPanel(
                              
                              tabPanel('Summary', DT::dataTableOutput(ns("vessel_catch_summary"))),
                              tabPanel('History', DT::dataTableOutput(ns("vessel_catch_history")),htmlOutput(ns("vessel_catch_datasource")))
                            ))
         )
         
         
         
         
)

)
}