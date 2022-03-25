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
    
    
    output$nb_infos <- renderUI({
      
      fluidRow(
        if(isTRUE(HAS_REGMANGT)){
          shiny::tagList(
            CalipseoInfoBox(i18n("INFOBOX_TITLE_VESSELS"), infos$count_vessels, icon = icon("ship")),
            CalipseoInfoBox(i18n("INFOBOX_TITLE_VESSEL_OWNERS"), infos$count_vessels_owners, icon = icon("user")),
            CalipseoInfoBox(i18n("INFOBOX_TITLE_VESSEL_CAPTAINS"), infos$count_vessels_captains, icon = icon("user-shield"))
          )
        },
        CalipseoInfoBox(i18n("INFOBOX_TITLE_FISHING_TRIPS"), infos$count_fishing_trips, icon = icon("ship")),
        CalipseoInfoBox(i18n("INFOBOX_TITLE_LANDING_SITES"), infos$landing_sites, icon = icon("map-marker")),
        CalipseoInfoBox(i18n("INFOBOX_TITLE_YEARS_ANALYZED"), infos$years, icon = icon("history"))
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