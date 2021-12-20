#home_ui
home_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "home",
          
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              
              tags$h2(i18n("HOME_CALIPSEO_TITLE"),tags$small(i18n("HOME_CALIPSEO_SUBTITLE")))
            )
          ),
          withSpinner(uiOutput(ns("nb_infos")))
  )
  
}