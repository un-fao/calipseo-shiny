#logbooks_trips_ui
logbooks_trips_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "logbooks_trips",
          uiOutput(ns("urlPage")),
          tags$h2(i18n("LOGBOOKS_TRIPS_TITLE")),
          trip_gantt_ui(ns("trips"),sliderWidth =25)
  )

}