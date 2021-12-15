#logbooks_trips_ui
logbooks_trips_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "logbooks_trips",
          uiOutput(ns("urlPage")),
          h2("Trips catalog"),
          trip_gantt_ui(ns("trips"),sliderWidth =25)
  )

}