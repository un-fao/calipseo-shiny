# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  #page management
  session$userData$page <- reactiveVal(NULL)
  
  loadedSideUI <- reactiveVal(FALSE)
  loadedMainUI <- reactiveVal(FALSE)

  observe({
    currentPage <- NA
    #look if there is a page in URL, if yes use it
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$page)) {
      currentPage <- query$page
      cat(sprintf("Current page from URL %s\n", currentPage))
    }else{
      if (!is.null(session$userData$page())) {
        currentPage <- session$userData$page()
        cat(sprintf("Current page from userData %s\n", currentPage))
      }
    }
   
    if (!is.na(currentPage)) {
      isolate({updateTabItems(session, "calipseo-tabs", gsub("-", "_", currentPage))})
    } 
  })
  
  observe({
    #Update PageUrl
    if(!is.null(input$`calipseo-tabs`)) if(input$`calipseo-tabs` != "home") {
      PageUrl <- gsub('_', '-', input$`calipseo-tabs`)
      session$userData$page(PageUrl)
      updatePageUrl(PageUrl, session)
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
    id_out <- loadModuleServer(input[["calipseo-tabs"]], appConfig, pool, module_state)
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