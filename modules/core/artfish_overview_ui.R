#artfish_overview_ui
artfish_overview_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "artfish_overview",
          fluidRow(
            tags$h3(i18n("ARTFISH_OVERVIEW_TITLE")),
          ),
          uiOutput(ns("no_release")),
          uiOutput(ns("fishing_unit_selector")),
          uiOutput(ns("results"))
  )
}
         