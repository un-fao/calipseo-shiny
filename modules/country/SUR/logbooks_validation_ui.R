#logbooks_validation_ui
logbooks_validation_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash:: tabItem(
    tabName = "logbooks_validation",
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
    uiOutput(ns("main"))
  )
}