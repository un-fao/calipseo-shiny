#vessel_list_ui
vessel_list_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "vessel_list",
          fluidRow(
            bs4Dash::box(
              title = htmlOutput(ns("vessel_list_info")),
              width = 12, 
              withSpinner(DT::dataTableOutput(ns("vessel_list"))),
              maximizable = TRUE,
              collapsible = FALSE
            )
          )
  )
  
}
