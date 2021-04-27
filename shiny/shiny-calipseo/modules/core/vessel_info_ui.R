#vessel_info_ui
vessel_info_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_info",
          
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("vessel_header")),
              uiOutput(ns("vessel_description"))
            )
          ),
          fluidRow(
            div(
              class = "col-md-5",
              h3("Ownership"), hr(),
              box(
                width = 12,
                DT::dataTableOutput(ns("vessel_owners"))
              ),
              h3("Licenses"), hr(),
              box(
                width = 12
              ),
              h3("Catch summary"), hr(),
              box(
                width = 12,
                DT::dataTableOutput(ns("vessel_catch_summary"))
              )
            ),
            div(
              class = "col-md-7",
              h3("Catch history", uiOutput(ns("vessel_catch_datasource"), inline = TRUE)),hr(),
              DT::dataTableOutput(ns("vessel_catch_history"))
            )
          )
  )
  
}