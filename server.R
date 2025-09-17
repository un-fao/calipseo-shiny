# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  
  session$userData$module_selection <- NULL
  session$userData$record_selection <- NULL
  
  #reactives
  loadedSideUI <- reactiveVal(FALSE)
  loadedMainUI <- reactiveVal(FALSE)
  
  #calipseo-shiny URL decoding mechanism
  observe({
    selected_module <- NA
    selected_record <- NA
    #look if there is module in URL, if yes use it
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$module)) {
      selected_module <- query$module
      session$userData$module_selection <- selected_module
      INFO("Selected module from URL %s", selected_module)
      if(!is.null(query$record)){
        selected_record <- query$record
        session$userData$record_selection <- selected_record
        INFO("Selected record from URL %s", selected_record)
      }
    }else{
      if (!is.null(session$userData$module_selection)) {
        selected_module <- session$userData$module_selection
        INFO("Selected module from userData %s", selected_module)
      }
    }
    if (!is.na(selected_module)) {
      isolate({updateTabItems(session, "calipseo-tabs", gsub("-", "_", selected_module))})
    }
  })
  
  #calipseo-shiny URL encoding mechanism
  observe({
    if(!is.null(input$`calipseo-tabs`)) if(input$`calipseo-tabs` != "home") {
      session$userData$module_selection = input$`calipseo-tabs`
      #updateModuleUrl provides the mechanism to rewrite the URL with the module
      #and an eventual record selection (which business logic for selection is 
      #managed in relevant modules e.g. vessel_info)
      updateModuleUrl(
        session, 
        module = input$`calipseo-tabs`,
        record = session$userData$record_selection      
      )
    }
  })
  
  reloader<-reactiveVal(NULL)
  module_state <- shiny::reactiveValues(
    initialized = list(),
    toreload = list()
  )
  
  #when a module is accessed from the leftside menu panel, we invoke the server
  #the module will be invoked if not initialized, or if it has to be reloaded
  #this is controlled by the module_state
  observeEvent(input[["calipseo-tabs"]], {
    
    module <- input[["calipseo-tabs"]]
    INFO("UI - Accessing module '%s' from sidebar menu", module)
    id_out <- loadModuleServer(input[["calipseo-tabs"]], session, appConfig, pool, module_state)
    switch(attr(id_out, "status"),
      "initialize" = {
        module_state$initialized = c(module_state$initialized, id_out)
      },
      "reload" = {
        module_state$toreload = setdiff(module_state$toreload, id_out)
      }
    )
    DEBUG("Check module state after module load:")
    DEBUG("- modules initialized: %s", paste0(module_state$initialized, collapse = ","))
    DEBUG("- modules to be reloaded: %s", paste0(module_state$toreload, collapse = ","))
  })
  
  #when the reloader() is triggered by some module, we reference the eventual list of
  #modules to be reloaded. These will not be reloaded immediately, but once accessed only
  observeEvent(reloader(),{
    if(!is.null(reloader())){
      INFO("Register reloadable modules triggered by '%s' module", reloader())
      modules_to_reload <- listLinkedModules(reloader(), appConfig)
      module_state$toreload <- unique(c(module_state$toreload, modules_to_reload))
      DEBUG("Check module state after reload triggered:")
      DEBUG("- modules initialized: %s", paste0(module_state$initialized, collapse = ","))
      DEBUG("- modules to be reloaded: %s", paste0(module_state$toreload, collapse = ","))
    }
  },ignoreNULL = F)
  
}