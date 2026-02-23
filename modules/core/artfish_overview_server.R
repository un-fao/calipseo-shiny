#artfish_overview_server
artfish_overview_server <- function(id, parent.session, pool, reloader){

 moduleServer(id, function(input, output, session){   
    
  INFO("artfish-overview: START")
  MODULE_START_TIME <- Sys.time()
  
  ns<-session$ns
  
  #reference data
  ref_species <- accessRefSpecies(pool)
  ref_fishing_units <- accessRefFishingUnits(pool)
  
  #get Artfish computation output files
  INFO("Get Artfish computation output files")
  files <- getStatPeriods(config = appConfig, id = "artfish_estimates",target = "release")
  INFO("Retrieved %s computation files", nrow(files))
  
  req(nrow(files) > 0)
  
  INFO("Get Artfish computation outputs for UI")
  estimate <- get_artfish_results_for_ui(files, ref_fishing_units, ref_species)
  
  artfishr::artfish_shiny_overview_server("artfish_overview", lang = appConfig$language, estimate = estimate)
  
  MODULE_END_TIME <- Sys.time()
  INFO("artfish-overview: END")
  DEBUG_MODULE_PROCESSING_TIME("Artfish-overview", MODULE_START_TIME, MODULE_END_TIME)
  
  
 })
  
}