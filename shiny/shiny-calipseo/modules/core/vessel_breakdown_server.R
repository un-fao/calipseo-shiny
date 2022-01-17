#vessel_breakdown_server
vessel_breakdown_server <- function(input, output, session, pool) {
  
 
  
  output$vessel_breakdown_info <- renderText({
    session$userData$page("vessel-breakdown")
    updatePageUrl("vessel-breakdown", session)
    text <- paste0("<h2>", i18n("VESSEL_BREAKDOWN_TITLE")," <small>", i18n("VESSEL_BREAKDOWN_SUBTITLE"),"</small></h2><hr>")
    text
  })
  
  
  
  
  #vessels breakdown by type (pie chart)
  output$rep_vessels <- renderPlotly({
    vessel_breakdown = accessVesselsCountByType(pool)
    
    plot_ly(vessel_breakdown, labels = ~NAME, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  df_vessel_type <- accessVesselsCountByType(pool)[,-1]
  names(df_vessel_type) <- c(i18n("BREAKDOWN_VESSEL_HOMEPORT_TABLE_COLNAME_1"),i18n("BREAKDOWN_VESSEL_HOMEPORT_TABLE_COLNAME_2"))
  
  output$rep_vessels_data <- renderDataTable(
    df_vessel_type,
    server = FALSE,
    escape = FALSE,
    rownames = FALSE,
    extensions = c("Buttons"), 
    options = list(
      autoWidth = TRUE,
      dom = 'Bfrtip',
      deferRender = TRUE,
      scroll = FALSE,
      buttons = list(
        list(extend = 'copy'),
        list(extend = 'csv', filename =  paste(i18n("BREAKDOWN_VESSEL_HOMEPORT_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = 'excel', filename =  paste(i18n("BREAKDOWN_VESSEL_HOMEPORT_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = "pdf", title = paste(i18n("BREAKDOWN_VESSEL_HOMEPORT_PDF_TITLE"),appConfig$country_profile$data$NAME), header = TRUE, orientation = "landscape")
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      language = list(url = i18n("TABLE_LANGUAGE"))
    ),
    filter = list(position = 'top', clear = FALSE))
  
  
  
  
  
  
  output$map_vessels <- renderLeaflet({
    
    sites_vessels <- accessVesselsCountByLandingSite(pool)
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%  
      addCircles(data = sites_vessels, weight = 1, color = "blue", fillColor = "blue", fillOpacity = 0.7, 
                 radius = 7000*sqrt(sites_vessels$COUNT/max(sites_vessels$COUNT,na.rm = TRUE)), 
                 popup = paste(
                   em(paste0(i18n("BREAKDOWN_VESSEL_HOMEPORT_MAP_LABEL_HOMEPORT"),": ")), sites_vessels$NAME,br(),
                   em(paste0(i18n("BREAKDOWN_VESSEL_HOMEPORT_MAP_LABEL_NUMBER_OF_VESSELS"),":")), sites_vessels$COUNT
                 ))
  })
  
  
  
  df_vessel_landingsite_data <- as.data.frame(accessVesselsCountByLandingSite(pool))[,-3] 
  
  names(df_vessel_landingsite_data) <- c(i18n("BREAKDOWN_VESSEL_VESSELTYPE_HOMEPORT_TABLE_COLNAME_1"),
                                         i18n("BREAKDOWN_VESSEL_VESSELTYPE_HOMEPORT_TABLE_COLNAME_2"))
  
  
  output$map_vessel_data <- renderDataTable(
    df_vessel_landingsite_data,
    server = FALSE,
    escape = FALSE,
    rownames = FALSE,
    extensions = c("Buttons"), 
    options = list(
      autoWidth = TRUE,
      dom = 'Bfrtip',
      deferRender = TRUE,
      scroll = FALSE,
      buttons = list(
        list(extend = 'copy'),
        list(extend = 'csv', filename =  paste(i18n("BREAKDOWN_VESSEL_VESSELTYPE_HOMEPORT_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = 'excel', filename = paste(i18n("BREAKDOWN_VESSEL_VESSELTYPE_HOMEPORT_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = "pdf", title = paste(i18n("BREAKDOWN_VESSEL_VESSELTYPE_HOMEPORT_PDF_TITLE"),appConfig$country_profile$data$NAME), header = TRUE, orientation = "landscape")
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      language = list(url = i18n("TABLE_LANGUAGE"))
    ),
    filter = list(position = 'top', clear = FALSE))
  
  
  output$rep_vessels_home_port <- renderPlotly({
    
    plot_ly(df_vessel_landingsite_data, labels = ~`HOME PORT`, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  

  df_vessel_landingsite_data_breakdown <- vesselsLandingSitesVesselTypesCount(pool)[,c(1,2,5)]

  names(df_vessel_landingsite_data_breakdown) <- c(i18n("BREAKDOWN_VESSELTYPE_LANDINGSITE_TABLE_COLNAME_1"),
                                                   i18n("BREAKDOWN_VESSELTYPE_LANDINGSITE_TABLE_COLNAME_2"),
                                                   i18n("BREAKDOWN_VESSELTYPE_LANDINGSITE_TABLE_COLNAME_3"))

  output$map_vessel_data_breakdown <- renderDataTable(
    df_vessel_landingsite_data_breakdown,
    server = FALSE,
    escape = FALSE,
    rownames = FALSE,
    extensions = c("Buttons"),
    options = list(
      autoWidth = TRUE,
      dom = 'Bfrtip',
      deferRender = TRUE,
      scroll = FALSE,
      buttons = list(
        list(extend = 'copy'),
        list(extend = 'csv', filename =  paste(i18n("BREAKDOWN_VESSELTYPE_LANDINGSITE_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = 'excel', filename = paste(i18n("BREAKDOWN_VESSELTYPE_LANDINGSITE_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = "pdf", title = paste(i18n("BREAKDOWN_VESSELTYPE_LANDINGSITE_PDF_TITLE"),appConfig$country_profile$data$NAME), header = TRUE, orientation = "landscape")
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      language = list(url = i18n("TABLE_LANGUAGE"))
    ),
    filter = list(position = 'top', clear = FALSE))
  
  
  vessel_landingsite_breakdown <- vesselsLandingSitesVesselTypesCount(pool)
  
  vessel_landingsite_breakdown <- reshape(vessel_landingsite_breakdown, direction = 'wide', idvar = c('LATITUDE','LONGITUDE','HOME_PORT_LANDING_SITE'),
          timevar = 'VESSEL_TYPE')
  
  names(vessel_landingsite_breakdown) <- gsub("COUNT.", "", names(vessel_landingsite_breakdown), fixed = TRUE)
  
  for (i in 2:ncol(vessel_landingsite_breakdown)) {
    
    vessel_landingsite_breakdown[,i]<- as.numeric(vessel_landingsite_breakdown[,i])
    
  }
  
  vessel_landingsite_breakdown$Total <- rowSums(vessel_landingsite_breakdown[,4:ncol(vessel_landingsite_breakdown)], na.rm = T)
 

  output$map_vessels2 <- renderLeaflet({


    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%
      addCircles(data = vessel_landingsite_breakdown, lat = vessel_landingsite_breakdown$LATITUDE, lng = vessel_landingsite_breakdown$LONGITUDE,weight = 1, color = "blue", fillColor = "blue", fillOpacity = 0.7
      ) %>%
      addMinicharts(lng = vessel_landingsite_breakdown$LONGITUDE,
                    lat = vessel_landingsite_breakdown$LATITUDE,
                    type = "pie",
                    legendPosition = 'bottomright',
                    chartdata = vessel_landingsite_breakdown[,c(4:(ncol(vessel_landingsite_breakdown)-1))],
                    width = 40 * sqrt(vessel_landingsite_breakdown$Total) / sqrt(max(vessel_landingsite_breakdown$Total)), transitionTime = 0

                    )
  })

  
}

