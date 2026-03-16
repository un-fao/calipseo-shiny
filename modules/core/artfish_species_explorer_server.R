#artfish_species_explorer_server
artfish_species_explorer_server <- function(id, parent.session, pool, reloader){
  
  moduleServer(id, function(input, output, session){   
    
    INFO("artfish-species_explorer: START")
    MODULE_START_TIME <- Sys.time()  
    
    ns<-session$ns
    
    #reference data
    ref_species <- accessRefSpecies(pool)
    ref_species$ID <- as.character(ref_species$ID)
    ref_fishing_units <- accessRefFishingUnits(pool)
    ref_fishing_units$ID <- as.character(ref_fishing_units$ID)
    available_period <- accessEffortSurveyPeriods(pool)
    
    result <- reactiveVal(NULL)
    
    #run Artfish computation
    INFO("Run Artfish computation output based on available periods")
    
    session$onFlushed(function(){
      
      n <- nrow(available_period)
      
      waiting_screen <- div(
        h3(i18n("ARTFISH_SPECIES_EXPLORER_LOADER_TITLE")),
        waiter::spin_fading_circles(),
        h4(id = "progress_percent", ""),
        div(id = "progress_label", i18n("ARTFISH_SPECIES_EXPLORER_LOADER_INIT_MESSAGE")),
        h4(i18n("ARTFISH_SPECIES_EXPLORER_LOADER_LOADING_MESSAGE"))
      )
      
      waiter::waiter_show(
        html = waiting_screen,
        color = "#14141480"
      )
      
      progress_callback <- function(step, label){
        
        session$sendCustomMessage(
          "update_progress_label",
          list(
            percent = sprintf("%d%%", round(step / n * 100)),
            text = sprintf("%s (%d/%d)", label, step, n)
          )
        )
        
      }
      
      res <- artfish_estimates_explorer(
        pool,
        progress_fn = progress_callback
      )
      
      waiter::waiter_hide()
      
      result(res)
      
    }, once = TRUE)
    
    observe({
      
      req(result())
      
      INFO("Computeded %s computation files", length(result()$data))
      
      INFO("Get Artfish computation outputs for UI")
      
      estimate <- get_artfish_results_for_ui(
        input = result()$data,
        input_type = "dataframes",
        ref_fishing_units = ref_fishing_units,
        ref_species = ref_species
      )
      
      artfishr::artfish_shiny_species_server(
        "artfish_species_explorer", 
        lang = appConfig$language, 
        estimate = estimate,
        effort_source = result()$effort_source,
        minor_strata = result()$minor_strata
      )
      
    })
    
    MODULE_END_TIME <- Sys.time()
    INFO("artfish-species_explorer: END")
    DEBUG_MODULE_PROCESSING_TIME("Artfish-species_explorer", MODULE_START_TIME, MODULE_END_TIME)
    
  })  
  
}