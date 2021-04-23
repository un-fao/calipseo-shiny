#logbooks_overview_ui
logbooks_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "logbooks_overview",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("logbooks_overview_info"))
            )
          ),
          uiOutput(ns("nb_infos")),
          fluidRow(
            div(
              class = "col-md-6",
              h3(sprintf("Statistics (kg) by vessel type - %s", as.integer(format(Sys.Date(), "%Y"))-1)), hr(),
              box(
                width = 12,
                DT::dataTableOutput(ns("stats_by_type_lastyear_table"))
              )
            ),
            div(
              class = "col-md-6",
              h3(sprintf("Statistics (kg) by vessel type - %s", format(Sys.Date(), "%Y"))), hr(),
              box(
                width = 12,
                DT::dataTableOutput(ns("stats_by_type_currentyear_table"))
              )
            )
          )
  )
  
}