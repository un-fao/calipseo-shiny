#logbooks_trips_ui
logbooks_trips_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "logbooks_trips",
          uiOutput(ns("urlPage")),
          fluidRow(
              plotlyOutput(ns("gantt"))%>%withSpinner(type = 4)
            )
  )
}