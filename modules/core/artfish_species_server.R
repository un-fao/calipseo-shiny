#artfish_species_server
artfish_species_server <- function(id, parent.session, pool, reloader){
 
 moduleServer(id, function(input, output, session){   
   
  INFO("artfish-species: START")
  MODULE_START_TIME <- Sys.time()  
   
  ns<-session$ns
  
  #reference data
  ref_species <- accessRefSpecies(pool)
  ref_species$ID <- as.character(ref_species$ID)
  ref_fishing_units <- accessRefFishingUnits(pool)
  ref_fishing_units$ID <- as.character(ref_fishing_units$ID)
  
  #get Artfish computation output files
  INFO("Get Artfish computation output files")
  files <- getStatPeriods(config = appConfig, id = "artfish_estimates",target = "release")
  INFO("Retrieved %s computation files", nrow(files))
  
  INFO("Get Artfish computation outputs for UI")
  estimate <- get_artfish_results_for_ui(input=files,input_type = "file", ref_fishing_units, ref_species)    
  
  artfishr::artfish_shiny_species_server("artfish_species", lang = appConfig$language, estimate = reactive({ estimate }))
  
  MODULE_END_TIME <- Sys.time()
  INFO("artfish-species: END")
  DEBUG_MODULE_PROCESSING_TIME("Artfish-species", MODULE_START_TIME, MODULE_END_TIME)

 })  
  
}