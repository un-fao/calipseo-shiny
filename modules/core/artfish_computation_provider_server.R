#artfish_computation_provider_server
artfish_computation_provider_server <- function(id, parent.session, lang = NULL, pool, reloader, progress_fn) {
  moduleServer(id, function(input, output, session) {
    
    #i18n
    #-----------------------------------------------------------------------------
    i18n_translator <- get_reactive_translator(lang)
    i18n <- function(key){ i18n_translator()$t(key) }
    #-----------------------------------------------------------------------------
    
    refresh <- reactiveVal(0)
    
    trigger_refresh <- function(){
      refresh(refresh() + 1)
    }
    
    observeEvent(reloader(), {
      trigger_refresh()
    })
    
    waiting_screen <- div(
      h3(i18n("ARTFISH_EXPLORER_LOADER_TITLE")),
      waiter::spin_fading_circles(),
      h4(id = "progress_percent", ""),
      div(id = "progress_label", i18n("ARTFISH_EXPLORER_LOADER_INIT_MESSAGE")),
      h4(i18n("ARTFISH_EXPLORER_LOADER_LOADING_MESSAGE"))
    )
    
    waiter::waiter_show(
      html = waiting_screen,
      color = "#14141480"
    )
    
    compute_srv <- artfish_estimates_explorer(
      pool = pool,
      refresh = refresh,
      progress_fn = progress_fn
    )
    
    ref_species <- reactive({
      accessRefSpecies(pool)
    }) |> bindCache("ref_species")
    
    ref_fishing_units <- reactive({
      accessRefFishingUnits(pool)
    }) |> bindCache("ref_fishing_units")
    
    #refresh events
    observeEvent(refresh(),{
      waiting_screen <- div(
        h3(i18n("ARTFISH_EXPLORER_LOADER_TITLE")),
        waiter::spin_fading_circles(),
        h4(id = "progress_percent", ""),
        div(id = "progress_label", i18n("ARTFISH_EXPLORER_LOADER_INIT_MESSAGE")),
        h4(i18n("ARTFISH_EXPLORER_LOADER_LOADING_MESSAGE"))
      )
      
      waiter::waiter_show(
        html = waiting_screen,
        color = "#14141480"
      )
    }, ignoreInit = TRUE, priority = 100)
    observe({
      if (isTRUE(compute_srv$ready())) {
        waiter::waiter_hide()
      }
    })
    
    
    # Expose a *stable accessor*, not the module itself
    list(
      ready = compute_srv$ready,
      estimates = compute_srv$estimates,
      effort_source = compute_srv$effort_source,
      minor_strata = compute_srv$minor_strata,
      ref_species = ref_species,
      ref_fishing_units = ref_fishing_units,
      trigger_refresh = trigger_refresh
    )
  })
}
