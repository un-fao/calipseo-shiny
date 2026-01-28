#artfish_report_ui
artfish_report_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "artfish_report",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              tags$h2(i18n("ARTFISH_REPORT_TITLE")),tags$h3(class = "text-muted", i18n("ARTFISH_REPORT_SUBTITLE"))
            )
          ),
          fluidRow(
            column(4,
              fluidRow(
                column(12,
                  uiOutput(ns("year_selector")),
                  uiOutput(ns("month_selector")),
                  uiOutput(ns("fishing_unit_selector")),
                  uiOutput(ns("button"))
                )
              ),
              fluidRow(
                column(6,
                  uiOutput(ns("info_block"))
                )
              ),
              fluidRow(
                column(8,
                       plotlyOutput(ns("accuracy"), height = 200)
                )
              )
            ),
            column(6,
              uiOutput(ns("results"))
            )
          )
          
  )
  
}