#artfish_fishing_unit_server
artfish_fishing_unit_server <- function(id, parent.session, pool, reloader){
  
  moduleServer(id, function(input, output, session){   
  
    INFO("artfish-fishing_unit: START")
    MODULE_START_TIME <- Sys.time()
    
    ns<-session$ns
    
    #reference data
    ref_species <- accessRefSpecies(pool)
    ref_fishing_units <- accessRefFishingUnits(pool)
    
    #get Artfish computation output files
    INFO("Get Artfish computation output files")
    files <- getStatPeriods(config = appConfig, id = "artfish_estimates",target = "release")
    INFO("Retrieved %s computation files", nrow(files))
    
    #retrieve indicator definition to retrieve 'effort_source' parameter
    AVAILABLE_INDICATORS <- getLocalCountryDataset(appConfig,"statistical_indicators.json")
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == "artfish_estimates"})]
    effort_source<-indicator[[1]]$compute_with$fun_args$effort_source$source
    if(!is.null(effort_source))effort_source<-gsub("text:","",effort_source)
    
    INFO("Get Artfish computation outputs for UI")
    estimate <- get_artfish_results_for_ui(input=files,input_type = "file", ref_fishing_units, ref_species)
 
    artfishr::artfish_shiny_fishing_unit_server("artfish_fishing_unit", 
                                                 lang = appConfig$language, 
                                                 estimate = estimate, 
                                                 effort_source=effort_source)
    
    MODULE_END_TIME <- Sys.time()
    INFO("artfish-unit: END")
    DEBUG_MODULE_PROCESSING_TIME("Artfish-fishing_unit", MODULE_START_TIME, MODULE_END_TIME)
    
  })  
  
}