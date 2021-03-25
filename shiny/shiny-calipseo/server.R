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
      switch(currentPage,
             'vessel-list' = {isolate({updateTabItems(session, "calipseo-tabs", "vessel_list")})},
             'vessel-info' = {isolate({updateTabItems(session, "calipseo-tabs", "vessel_info")})},
             'vessel-breakdown' = {isolate({updateTabItems(session, "calipseo-tabs", "vessel_breakdown")})},
             {isolate({updateTabItems(session, "calipseo-tabs", "home")})}
      )
    } else {
      isolate({updateTabItems(session, "calipseo-tabs", "homeTab")})
    }
  })
  
  observe({
    
    #HOME
    #-------------------------------------------------------------------------------
    callModule(homeServer, "home", pool)
    
    #VESSELS
    #-------------------------------------------------------------------------------
    callModule(vesselListServer, "vessel_list", pool)
    callModule(vesselInfoServer, "vessel_info", pool)
    callModule(vesselBreakdownServer, "vessel_breakdown", pool)

  })
  
}