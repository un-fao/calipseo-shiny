#artfish_species_explorer_server
artfish_species_explorer_server <- function(id, parent.session, pool, reloader){
  
  moduleServer(id, function(input, output, session){   
    
    INFO("artfish-species_explorer: START")
    MODULE_START_TIME <- Sys.time()  
    
    ns<-session$ns
  
    #run Artfish computation
    INFO("Run Artfish computation output based on available periods")
    
    observeEvent(input$refresh_estimates, {
      INFO("Refreshing computation")
      refresh(refresh() + 1)
    })
    
    # Request provider (this triggers computation only the first time)
    artfish <- session$userData$get_artfish_provider()
    
    estimate_r <- reactive({
      req(artfish$estimates())
      waiter::waiter_hide()
      get_artfish_results_for_ui(
        input = artfish$estimates(),
        input_type = "data.frame",
        ref_fishing_units = artfish$ref_fishing_units(),
        ref_species = artfish$ref_species()
      )
    })
      
    artfishr::artfish_shiny_species_server(
      "artfish_species_explorer", 
      lang = appConfig$language, 
      estimate = estimate_r,
      effort_source = artfish$effort_source,
      minor_strata = artfish$minor_strata
    )
    
    MODULE_END_TIME <- Sys.time()
    INFO("artfish-species_explorer: END")
    DEBUG_MODULE_PROCESSING_TIME("Artfish-species_explorer", MODULE_START_TIME, MODULE_END_TIME)
    
  })  
  
}