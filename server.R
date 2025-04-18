# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  onStop(function(){
    resetAuthSessionVariables(session)
  })
  
  #page management
  session$userData$page <- reactiveVal(NULL)
  
  loadedSideUI <- reactiveVal(FALSE)
  loadedMainUI <- reactiveVal(FALSE)
  
  if(appConfig$auth){
    #auth mode    
    auth_info <- reactiveVal(NULL)
    
    if(appConfig$auth_ui){
      #auth with UI
      credentials <- authLoginServer(
        id = "login",
        config = appConfig,
        log_out = reactive(logout_init())
      )
    
      #call the logout module with reactive trigger to hide/show
      logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(credentials()$user_auth)
      )
      
      observe({
        if (credentials()$user_auth) {
          
          info = credentials()$auth_info
          info$logged <- credentials()$user_auth
          auth_info(info)
          
          if(!is.null(auth_info())){
            initAuthSessionVariables(session, auth_info())
            INFO("Set-up shiny-calipseo in auth mode")
            shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
            shinyjs::show(selector = "header")
            loadModuleServers(appConfig, pool, reloader = NULL) #TODO pass auth_info to all modules?
          }
          
        } else {
          shinyjs::addClass(selector = "body", class = "sidebar-collapse")
          shinyjs::hide(selector = "header")
        }
      })
    }else{
      #auth without UI
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::show(selector = "header")
      
      #TODO auth through URL token passing?
      #Shiny proxy?
      
    }
  }else{
    #anonymous usage
    observe({
      INFO("Set-up calipseo-shiny in anonymous mode")
      #shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      #shinyjs::show(selector = "header")
      loadModuleServers(appConfig, pool, reloader)
    })
  }
  
  #side UI
  #output$side_ui <- renderUI({
  #  print(loadedSideUI())
  #  if(!loadedSideUI()){
  #    loadedSideUI(TRUE)
  #    if(appConfig$auth){
  #      if(appConfig$auth_ui){
  #        req(credentials()$user_auth)
  #        sidebarMenuFromModules(appConfig)
  #      }else{
  #        sidebarMenuFromModules(appConfig)
  #      }
  #    }else{
  #      sidebarMenuFromModules(appConfig)
  #    }
  #  }
    
  #})
  #main UI
  #output$main_ui <- renderUI({
  #  print(loadedMainUI())
  #  if(!loadedMainUI()){
  #    loadedMainUI(TRUE)
  #    if(appConfig$auth){
  #      if(appConfig$auth_ui){
  #        req(credentials()$user_auth)
  #        loadModuleUIs(appConfig)
  #      }else{
  #        loadModuleUIs(appConfig)
  #      }
  #    }else{
  #      loadModuleUIs(appConfig)
  #    }
  #  }
  #})
  
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