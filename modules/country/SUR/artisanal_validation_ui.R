#artisanal_validation_ui
artisanal_validation_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(
    tabName = "artisanal_validation",
    useShinyjs(),
    waiter::use_waiter(), 
     tags$head(
       tags$style(
         HTML(".shiny-notification {
               height: 100px;
               width: 280px;
               position:fixed;
               top: calc(50% - 50px);;
               left: calc(50% - 140px);;
             }
            "
         )
       )
     ),
    tags$h2(i18n("ARTISANAL_VALIDATION_TITLE")),
    fluidRow(
      column(3,uiOutput(ns("file_input_wrapper"))),    
      column(1,style = "margin-top: 25px;", uiOutput(ns("validity_btn"))),
      column(1,style = "margin-top: 25px;", uiOutput(ns("generate_SQL_btn"))),
      column(1,style = "margin-top: 25px;", uiOutput(ns("generate_report_btn")))
    ),
      uiOutput(ns("validity_result"))
  )
}