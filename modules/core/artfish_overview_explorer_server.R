#artfish_overview_explorer_server
artfish_overview_explorer_server <- function(id, parent.session, pool, reloader){
  
  moduleServer(id, function(input, output, session){
    
    INFO("artfish-overview-explorer: START")
    MODULE_START_TIME <- Sys.time()
    
    ns <- session$ns
    
    result <- reactiveVal(NULL)
    
    #run Artfish computation
    INFO("Run Artfish computation output based on available periods")
    
    #refresh
    observeEvent(input$refresh_artfish_estimates, {
      artfish <- session$userData$get_artfish_provider()
      req(artfish)
      
      # Optional: prevent double refresh while running
      if (isFALSE(artfish$ready())) {
        return()
      }
      
      artfish$trigger_refresh()
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

    artfish_shiny_overview_server(
      "artfish_overview_explorer",
      lang = appConfig$language,
      estimate = estimate_r,
      effort_source = artfish$effort_source,
      minor_strata = artfish$minor_strata
    )
    
    MODULE_END_TIME <- Sys.time()
    
    INFO("artfish-overview-explorer: END")
    DEBUG_MODULE_PROCESSING_TIME("Artfish-overview-explorer", MODULE_START_TIME, MODULE_END_TIME)
    
  })
}