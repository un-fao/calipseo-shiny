#home_ui
home_server <- function(input, output, session, pool){
  
  infos_fetched <- reactiveVal(FALSE)
  infos <- reactiveValues(
    count_vessels = NULL,
    landing_sites = NULL,
    years = NULL,
    indicators = NULL
  )
  
  observe({
    #infos$count_vessels <- accessVesselsCountFromDB(pool)
    #infos$landing_sites <- length(accessLandingSitesFromDB(pool))
    #infos$years <- length(accessAvailableYearsFromDB(pool))
    #infos$indicators <- length(accessAvailableIndicators())
  })
  
  observe({
    
    if(all(!sapply(reactiveValuesToList(infos), is.null))) infos_fetched(TRUE)
    
    output$home_info <- renderText({
      session$userData$page("home")
      text <- "<h2>Calipseo R Shiny Dashboard <small>Access and compute statistics over the Calipseo OpenFismis plateform</small></h2>"
    })
    
    output$nb_infos <- renderUI({
      if(infos_fetched()){
        fluidRow(
          #infoBox("Vessels", infos$count_vessels, icon = icon("fa-ship"), fill = TRUE)
          #infoBox("Landing sites", infos$landing_sites, icon = icon("map-marker"), fill = TRUE),
          #infoBox("Years analyzed", infos$years, icon = icon("history"), fill = TRUE),
          #infoBox("Statistical indicators", infos$indicators, icon = icon("th-list"), fill = TRUE)
        )
      }
    })
  })
}