# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  onStop(function(){
    
  })
  
  #page management
  session$userData$page <- reactiveVal(NULL)
  
  loadedSideUI <- reactiveVal(FALSE)
  loadedMainUI <- reactiveVal(FALSE)
  
  #anonymous usage
  observe({
    INFO("Set-up calipseo-shiny")
    loadModuleServers(appConfig, pool, reloader)
  })
  
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
  initialized<-reactiveVal(FALSE)
  
  observeEvent(reloader(),{
    INFO("Reloading modules triggered by '%s' module", reloader())
    req(!is.null(initialized()))
    
    if(!initialized()){
      loadModuleServers(appConfig, pool, reloader)
      initialized<-initialized(TRUE)
    }else{
      req(!is.null(reloader()))
      #load module servers
      loadModuleServers(appConfig, pool, reloader)
      reloader<-reloader(NULL)
    }
  },ignoreNULL = F)
  
}