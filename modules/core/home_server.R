#home_ui
home_server <- function(id, parent.session, lang = NULL, pool, reloader){
  
 moduleServer(id, function(input, output, session) {
  
  ns = session$ns
   
  INFO("home: START")
  MODULE_START_TIME <- Sys.time()
  
  #i18n
  #-----------------------------------------------------------------------------
  i18n_translator <- get_reactive_translator(lang)
  i18n <- function(key){ i18n_translator()$t(key) }
  #-----------------------------------------------------------------------------
  
  infos_fetched <- reactiveVal(FALSE)
  infos <- reactiveValues(
    count_vessels = NULL,
    count_vessel_owners = NULL,
    count_vessel_captains = NULL,
    count_landing_sites = NULL,
    count_years = NULL
  )
  
  observe({
    infos$count_vessels <- countVessels(pool)
    infos$count_vessels_owners <- countVesselOwners(pool)
    infos$count_vessels_captains <- countVesselCaptains(pool)
    infos$count_fishing_trips <- countFishingTrips(pool)
    infos$count_landing_sites <- countLandingSites(pool)
    infos$count_years <- nrow(accessAvailableYears(pool))
  })
  
  observe({
    
    if(all(!sapply(reactiveValuesToList(infos), is.null))) infos_fetched(TRUE)
    
    output$main <- renderUI({
      tagList(
        uiOutput(ns("header")),
        withSpinner(uiOutput(ns("nb_infos")))
      )
    })
    
    output$header <- renderUI({
      fluidRow(
        div(
          width = 12, style = "margin:12px;",
          tags$h2(i18n("HOME_CALIPSEO_TITLE"),tags$small(i18n("HOME_CALIPSEO_SUBTITLE")))
        )
      )
    })
    
    output$nb_infos <- renderUI({
      fluidRow(
        if(isTRUE(HAS_REGMANGT)){
          shiny::tagList(
            bs4Dash::infoBox(i18n("INFOBOX_TITLE_VESSELS"), infos$count_vessels, icon = icon("ship"), color = "navy"),
            bs4Dash::infoBox(i18n("INFOBOX_TITLE_VESSEL_OWNERS"), infos$count_vessels_owners, icon = icon("user"), color = "primary"),
            bs4Dash::infoBox(i18n("INFOBOX_TITLE_VESSEL_CAPTAINS"), infos$count_vessels_captains, icon = icon("user-shield"), color = "primary")
          )
        },
        bs4Dash::infoBox(i18n("INFOBOX_TITLE_FISHING_TRIPS"), infos$count_fishing_trips, icon = icon("ship"), color = "navy"),
        bs4Dash::infoBox(i18n("INFOBOX_TITLE_LANDING_SITES"), infos$count_landing_sites, icon = icon("map-marker"), color = "maroon"),
        bs4Dash::infoBox(i18n("INFOBOX_TITLE_YEARS_ANALYZED"), infos$count_years, icon = icon("history"), color = "fuchsia")
      )
    })
    
    MODULE_END_TIME <- Sys.time()
    INFO("home: END")
    DEBUG_MODULE_PROCESSING_TIME("Home", MODULE_START_TIME, MODULE_END_TIME)
  })
  
 })
}