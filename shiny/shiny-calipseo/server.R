# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  #page management
  session$userData$page <- reactiveVal(NULL)
  
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
    #load module server parts    
    loadModuleServers(appConfig, pool)
  })
  
  
  observe({
    #Update PageUrl
    if(!input$`calipseo-tabs` == "home"){
      PageUrl <- gsub('_', '-', input$`calipseo-tabs`)
      session$userData$page(PageUrl)
      updatePageUrl(PageUrl, session)
    }
  })
  
}