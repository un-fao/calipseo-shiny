#vessel_qa_ui
vessel_qa_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "vessel_qa",
    uiOutput(ns("main"))      
  )
  
}

