#artfish_overview_server
artfish_overview_server <- function(id, parent.session, lang = NULL, pool, reloader){

 moduleServer(id, function(input, output, session){   
    
  ns<-session$ns
   
  INFO("artfish-overview: START")
  MODULE_START_TIME <- Sys.time()
  
  #i18n
  #-----------------------------------------------------------------------------
  i18n_translator <- get_reactive_translator(lang)
  i18n <- function(key){ i18n_translator()$t(key) }
  #-----------------------------------------------------------------------------
  
  #reference data
  ref_species <- accessRefSpecies(pool)
  ref_fishing_units <- accessRefFishingUnits(pool)
  
  #get Artfish computation output files
  INFO("Get Artfish computation output files")
  files <- getStatPeriods(config = appConfig, id = "artfish_estimates",target = "release")
  INFO("Retrieved %s computation files", nrow(files))
  
  req(nrow(files) > 0)
  
  INFO("Get Artfish computation outputs for UI")
  estimate <- get_artfish_results_for_ui(input=files,input_type = "file", ref_fishing_units, ref_species)
  
  artfishr::artfish_shiny_overview_server("artfish_overview", 
                                          lang = lang, 
                                          estimate = reactive({ estimate }))
 
  MODULE_END_TIME <- Sys.time()
  INFO("artfish-overview: END")
  DEBUG_MODULE_PROCESSING_TIME("Artfish-overview", MODULE_START_TIME, MODULE_END_TIME)
  
  
 })
  
}