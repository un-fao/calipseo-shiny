#individual_breakdown_ui
individual_breakdown_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "individual_breakdown",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("individual_breakdown_info"))
            )
          ),
          fluidRow(
            
            box(width = 6, height = 480, title = sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PIECHART"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE, plotlyOutput(ns("individual_gender"))),
            
            box(width = 6, height = 480, title = sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PYRAMID"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE, plotlyOutput(ns("individual_age_gender")))
          )
          
  )
  
}