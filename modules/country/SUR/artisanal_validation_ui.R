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
    uiOutput(ns("main"))
  )
}