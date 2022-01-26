#artfish_overview_ui
artfish_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "artfish_overview",
          uiOutput(ns("urlPage")),
          fluidRow(
            tags$h2("Overview"),
          ),
          uiOutput(ns("fishing_unit_selector")),
          uiOutput(ns("results"))
  )
}
         