#individual_qa_ui
individual_qa_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "individual_qa",
    uiOutput(ns("main"))      
  )
  
}
