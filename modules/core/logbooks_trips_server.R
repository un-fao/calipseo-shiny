#logbooks_trips_server
logbooks_trips_server <- function(id, parent.session, pool, reloader){

 moduleServer(id, function(input, output, session) {
  
  INFO("logbooks-trips: START")
  MODULE_START_TIME <- Sys.time() 
   
  ns<-session$ns
  
  trip_gantt_server(
    id = "trips",
    pool = pool,
    vessel_stat_type = 2,
    vesselId = NULL,
    mode="full"
  )
  
  MODULE_END_TIME <- Sys.time()
  INFO("logbooks-trips: END")
  DEBUG_MODULE_PROCESSING_TIME("Logbooks-trips", MODULE_START_TIME, MODULE_END_TIME)
  
 })
}


