#home_ui
home_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "home",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("home_info"))
            )
          ),
          uiOutput(ns("nb_infos"))
          #fluidRow(
          #  box(width = 12, height = 345, title = "Monthly fishing activity", status = "primary", solidHeader= TRUE, plotlyOutput(ns("activity")))
          #)
  )
  
}