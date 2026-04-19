#individual_overview_ui
individual_overview_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "individual_overview",
                   
    fluidRow(
      column(
        width = 12,
        htmlOutput(ns("individual_overview_info"))
      )
    ),
    uiOutput(ns("indicators")),
    uiOutput(ns("figures"))
    )
}