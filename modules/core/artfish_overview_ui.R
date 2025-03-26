#artfish_overview_ui
artfish_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "artfish_overview",
          fluidRow(
            tags$h2(i18n("OVERVIEW_TITLE")),
          ),
          uiOutput(ns("no_release")),
          uiOutput(ns("fishing_unit_selector")),
          uiOutput(ns("results"))
  )
}
         