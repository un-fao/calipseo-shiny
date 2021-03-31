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
    infos$count_vessels <- accessVesselsCount(pool)
    infos$count_vessels_owners <- countVesselOwners(pool)
    infos$count_vessels_captains <- countVesselCaptains(pool)
    infos$count_fishing_trips <- countFishingTrips(pool)
    infos$landing_sites <- nrow(accessLandingSites(pool))
    infos$years <- length(accessAvailableYears(pool))
    #infos$indicators <- length(accessAvailableIndicators())
  })
  
  observe({
    
    if(all(!sapply(reactiveValuesToList(infos), is.null))) infos_fetched(TRUE)
    
    output$home_info <- renderText({
      session$userData$page("home")
      text <- "<h2>Calipseo R Shiny Dashboard <small>Access and compute statistics over the Calipseo OpenFismis plateform</small></h2>"
    })
    
    output$nb_infos <- renderUI({

      fluidRow(
        infoBox("Vessels", infos$count_vessels, icon = icon("ship"), fill = TRUE),
        infoBox("Vessel owners", infos$count_vessels_owners, icon = icon("user"), fill = TRUE),
        infoBox("Vessel captains", infos$count_vessels_captains, icon = icon("user-shield"), fill = TRUE),
        infoBox("Fishing trips", infos$count_fishing_trips, icon = icon("ship"), fill = TRUE),
        infoBox("Landing sites", infos$landing_sites, icon = icon("map-marker"), fill = TRUE),
        infoBox("Years analyzed", infos$years, icon = icon("history"), fill = TRUE)
        #infoBox("Statistical indicators", infos$indicators, icon = icon("th-list"), fill = TRUE)
      )
      
    })
    
    #output$activity <- renderPlotly({
    #  activity = accessMonthlyFishingActivity(pool)
    #  activity$idx = 1:nrow(activity)
    #  activity$time = paste0(activity$YEAR, "-", format(ISOdate(activity$YEAR,activity$MONTH,1), "%m"))
    #  plot_ly(activity, x = ~time, y = ~QUANTITY, type='scatter', mode = 'lines+markers') %>%  layout(autosize = TRUE)
    #})
    
  })
}