#artfish_fishing_unit_explorer_server
artfish_fishing_unit_explorer_server <- function(id, parent.session, lang = NULL, pool, reloader){
  
  moduleServer(id, function(input, output, session){   
    
    ns<-session$ns
    
    INFO("artfish-fishing_unit_explorer: START")
    MODULE_START_TIME <- Sys.time()
    
    #i18n
    #-----------------------------------------------------------------------------
    i18n_translator <- get_reactive_translator(lang)
    i18n <- function(key){ i18n_translator()$t(key) }
    #-----------------------------------------------------------------------------
    
    #run Artfish computation
    INFO("Run Artfish computation output based on available periods")
    
    refresh <- reactiveVal(0)
    
    #refresh
    observeEvent(input$refresh_artfish_estimates, {
      disable(ns("refresh_artfish_estimates"))
      artfish <- session$userData$get_artfish_provider()
      req(artfish)
      
      # Optional: prevent double refresh while running
      if (isFALSE(artfish$ready())) {
        return()
      }
      
      artfish$trigger_refresh()
      enable(ns("refresh_artfish_estimates"))
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
      
    artfishr::artfish_shiny_fishing_unit_server(
      "artfish_fishing_unit_explorer",
      lang = appConfig$language,
      estimate = estimate_r,
      effort_source = artfish$effort_source,
      minor_strata = artfish$minor_strata,
      opts = list(
        refresh_ui = actionButton(ns("refresh_artfish_estimates"), icon = icon("refresh"), label = "")
      )
    )
      
    
    MODULE_END_TIME <- Sys.time()
    INFO("artfish-fishing_unit_explorer: END")
    DEBUG_MODULE_PROCESSING_TIME("Artfish-fishing_unit_explorer", MODULE_START_TIME, MODULE_END_TIME)
    
  })  
  
}