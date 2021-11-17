vessel_info_ui <- function(id){
  
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_info",

fluidRow(width=12,
         
         htmlOutput(ns("vessel_header")),
         div(class = 'col-sm-4',
                
                div(class = 'row',
                  
                  div(class = 'col-sm-4', style='padding-left: 15px;',
                         
                         
                         tags$img(src='placeholder_pic.png',width='160px')
                         
                  ),
                  
                  div(class = 'col-sm-6 offset-md-1 col-sm-offset-2', style='padding-left: 40px;',
                         
                         div(class='row',style='width:200px;',
                           tabsetPanel(
                             
                             tabPanel('Info',
                                      uiOutput(ns("vessel_description"),style = "font-weight:bold;")),
                             tabPanel('Registration',uiOutput(ns("vessel_registration"),style = "font-weight:bold;"))
                           )
                           
                         ))
                  
                  
                )
                
         ),
         
         div(class = 'col-sm-6 offset-md-1 col-sm-offset-1',
                
                '#TODO'
         )
         
         
),br(),

div(class='row',style='width:12;',
         
         verticalTabsetPanel(
         verticalTabPanel('Ownership',box_height='70px' , DT::dataTableOutput(ns("vessel_owners"))),
         verticalTabPanel('Licenses',box_height='70px', '#TODO'),
         verticalTabPanel('Catches',box_height='70px' ,
                            tabsetPanel(
                              
                              tabPanel('Summary', DT::dataTableOutput(ns("vessel_catch_summary"))),
                              tabPanel('History', DT::dataTableOutput(ns("vessel_catch_history")),htmlOutput(ns("vessel_catch_datasource")))
                            ))
         )
         
         
         
         
)

)
}