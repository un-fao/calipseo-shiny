#artfish_ui
artfish_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "artfish",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              
              tags$h2(i18n("ARTFISH_TITLE")),
              tags$h3(i18n("ARTFISH_SUBTITLE")),
            )
          ),
          fluidRow(
            column(4,
              uiOutput(ns("period_selector")),
              uiOutput(ns("stratum_selector")),
              uiOutput(ns("button"))
            ),
            column(6,
              uiOutput(ns("results"))
            )
          )
          
  )
  
}