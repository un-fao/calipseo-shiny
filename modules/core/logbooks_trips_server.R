#logbooks_trips_server
logbooks_trips_server <- function(id, parent.session, lang = NULL, pool, reloader){

 moduleServer(id, function(input, output, session) {
  
  ns<-session$ns
   
  INFO("logbooks-trips: START")
  MODULE_START_TIME <- Sys.time() 
  
  #-----------------------------------------------------------------------------
  i18n_translator <- get_reactive_translator(lang)
  i18n <- function(key){ i18n_translator()$t(key) }
  #-----------------------------------------------------------------------------
  
  trip_gantt_server(
    id = "trips",
    lang = lang,
    pool = pool,
    vessel_stat_type = 2,
    vesselId = NULL,
    mode = "full"
  )
  
  MODULE_END_TIME <- Sys.time()
  INFO("logbooks-trips: END")
  DEBUG_MODULE_PROCESSING_TIME("Logbooks-trips", MODULE_START_TIME, MODULE_END_TIME)
  
 })
}


