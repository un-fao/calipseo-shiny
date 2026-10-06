#artfish_ui
artfish_accuracy_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_accuracy",
    
    #here we call artfishr::artfish_shiny_accuracy_ui
    #but nothing prevents from bundling this together with another UI with renderUI server side
    artfishr::artfish_shiny_accuracy_ui(ns("artfish_toolbox"))
  )
  
}