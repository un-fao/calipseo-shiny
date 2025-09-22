#artfish_fishing_unit_ui
artfish_fishing_unit_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "artfish_fishing_unit",
          fluidRow(
            tags$h2(i18n("ARTFISH_FISHING_UNIT_TITLE"))
          ),
          uiOutput(ns("no_release")),
          fluidRow(
            column(6,uiOutput(ns("selectors"))),
            column(3,uiOutput(ns("value"))),
            column(3,plotlyOutput(ns("accuracy"),height=200))
          ),
          uiOutput(ns("bg_results")),
          uiOutput(ns("sp_results"))
          
  )
  
  
}