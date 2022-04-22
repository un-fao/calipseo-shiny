#logbooks_trips_server
logbooks_trips_server <- function(id, pool){

 moduleServer(id, function(input, output, session) {
  
  ns<-session$ns
  
  output$urlPage<-renderUI({
    session$userData$page("logbooks-trips")
    updatePageUrl("logbooks-trips", session)
  })
  
  trip_gantt_server("trips",pool,vessel_stat_type=2,vesselId=NULL,mode="full")
  
 })
}


