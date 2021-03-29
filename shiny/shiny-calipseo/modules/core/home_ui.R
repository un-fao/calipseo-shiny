#homeUI
homeUI <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "home",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("home_info"))
            )
          ),
          uiOutput(ns("nb_infos"))
  )
  
}