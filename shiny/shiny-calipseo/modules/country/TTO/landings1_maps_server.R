#landings1_maps_server
landings1_maps_server <- function(id, pool){
  
 moduleServer(id, function(input, output, session){  
  
  output$landings1_maps_info <- renderText({
    text <- paste0("<h2>", i18n("LANDINGS1_MAPS_TITLE")," <small>", i18n("LANDINGS1_MAPS_SUBTITLE"),
                   userTooltip("These maps represent the different statistical descriptors by year including the 1st raised landings (LAN), value (VAL), number of fishing trips (TRP) and ratios such as Landings/Trip (L/T), Value/Trip (V/T), and Value/Landing (P/K)",
                               style = "font-size: 75%;"),"</small></h2><hr>")
    text
  })
  
  #mapDescriptorTotal
  mapDescriptorTotal <- function(tsdata, year, descriptor, color){
    
    sites_descriptor <- accessLandingSites(pool)
    if(!is.null(tsdata)){
      maxValue <- max(tsdata[is.na(tsdata$gear_id)&is.na(tsdata$species_id)&is.na(tsdata$month)&tsdata$descriptor == descriptor,"value"], na.rm = TRUE)
      bch_tsdata <- tsdata[is.na(tsdata$gear_id)&is.na(tsdata$species_id)&is.na(tsdata$month)&tsdata$year == year,]
      bch_tsdata_descriptor <- bch_tsdata[bch_tsdata$descriptor == descriptor,]
      sites_descriptor <- merge(
        sites_descriptor,
        bch_tsdata_descriptor,
        by.x = "NAME",
        by.y = "bch_name",
        all.x = TRUE,
        all.y = FALSE
      )
      sites_descriptor <- sites_descriptor[,c("NAME", "value")]
      
      #build the map
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%  
        addCircles(data = sites_descriptor, weight = 1, color = color, fillColor = color, fillOpacity = 0.7, 
                   radius = 7000*sqrt(sites_descriptor$value/maxValue), 
                   popup = paste(
                     em(paste0(i18n("LANDINGS1_MAP_LANDING_SITE_LABEL"),": ")), sites_descriptor$NAME,br(),
                     em(paste0(i18n("LANDINGS1_MAP_VALUE_LABEL")," (", descriptor,"):")), sites_descriptor$value
                   ))
    }else{
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%
        addCircles(data = sites_descriptor, weight = 1, color = "#000000", fillColor = "#000000", fillOpacity = 0.7,
                   popup = paste(
                     em(paste0(i18n("LANDINGS1_MAP_LANDING_SITE_LABEL"),": ")), sites_descriptor$NAME,br()
                   ))
    }
  }
  
  #CONTROLLERS
  #TOTAL LANDINGS maps
  #---------------------
  
  tsr <- reactiveValues(
    data = NULL 
  )
  
  observeEvent(input$year_map_total,{
    targetRelease <- file.path(sprintf("out/release/artisanal_fisheries_landings1/%s", input$year_map_total), sprintf("artisanal_fisheries_landings1_%s.csv", input$year_map_total))
    hasRelease <- file.exists(targetRelease)
    tsdata <- NULL
    if(hasRelease){
      tsdata <- readr::read_csv(targetRelease)
      tsdata <- tsdata[order(tsdata$bch_name),]
    }
    tsr$data <- tsdata
  })
  
  output$map_LAN <- renderLeaflet({
    mapDescriptorTotal(tsr$data, input$year_map_total, "LAN", "#e6550d")
  })
  output$map_VAL <- renderLeaflet({
    mapDescriptorTotal(tsr$data, input$year_map_total, "VAL", "#e6550d")
  })
  output$map_TRP <- renderLeaflet({
    mapDescriptorTotal(tsr$data, input$year_map_total, "TRP", "#e6550d")
  })
  output$map_LT <- renderLeaflet({
    mapDescriptorTotal(tsr$data, input$year_map_total, "L/T", "#e6550d")
  })
  output$map_VT <- renderLeaflet({
    mapDescriptorTotal(tsr$data, input$year_map_total, "V/T", "#e6550d")
  })
  output$map_PK <- renderLeaflet({
    mapDescriptorTotal(tsr$data, input$year_map_total, "P/K", "#e6550d")
  })
  
 })
  
}