#artfish_report_ui
artfish_report_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "artfish_report",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              
              tags$h2(i18n("ARTFISH_TITLE")),
              tags$h3(i18n("ARTFISH_SUBTITLE")),
            )
          ),
          fluidRow(
            column(4,
              uiOutput(ns("mode_selector")),
              uiOutput(ns("year_selector")),
              uiOutput(ns("month_selector")),
              uiOutput(ns("fishing_unit_selector")),
              uiOutput(ns("button"))
            ),
            column(6,
              uiOutput(ns("results"))
            )
          )
          
  )
  
}