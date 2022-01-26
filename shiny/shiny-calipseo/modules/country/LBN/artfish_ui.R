#artfish_ui
artfish_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "artfish",
          uiOutput(ns("urlPage")),
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              
              tags$h2("Monthly Reports"),
              tags$h3("Details on estimations and statistical diagnostics"),
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