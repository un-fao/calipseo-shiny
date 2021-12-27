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
  names(df_vessel_type) <- c("VESSEL TYPE", "COUNT")
  
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
        list(extend = 'csv', filename =  paste("Vessel Type Data Breakdown in",appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = 'excel', filename =  paste("Vessel Type Data Breakdown in",appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = "pdf", title = paste("Vessel Type Data Breakdown in",appConfig$country_profile$data$NAME), header = TRUE, orientation = "landscape")
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      )
    ),
    filter = list(position = 'top', clear = FALSE))
  
  
  
  
  
  
  output$map_vessels <- renderLeaflet({
    
    sites_vessels <- accessVesselsCountByLandingSite(pool)
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%  
      addCircles(data = sites_vessels, weight = 1, color = "blue", fillColor = "blue", fillOpacity = 0.7, 
                 radius = 7000*sqrt(sites_vessels$COUNT/max(sites_vessels$COUNT,na.rm = TRUE)), 
                 popup = paste(
                   em("Home port: "), sites_vessels$NAME,br(),
                   em(paste0("Number of vessels:")), sites_vessels$COUNT
                 ))
  })
  
  
  
  df_vessel_landingsite_data <- as.data.frame(accessVesselsCountByLandingSite(pool))[,-3] 
  
  names(df_vessel_landingsite_data) <- c("HOME PORT", "COUNT")
  
  
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
        list(extend = 'csv', filename =  paste("Vessel LandingSite Data Breakdown in",appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = 'excel', filename = paste("Vessel LandingSite Data Breakdown in",appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = "pdf", title = paste("Vessel LandingSite Data Breakdown in",appConfig$country_profile$data$NAME), header = TRUE, orientation = "landscape")
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      )
    ),
    filter = list(position = 'top', clear = FALSE))
  
  
  output$rep_vessels_home_port <- renderPlotly({
    
    plot_ly(df_vessel_landingsite_data, labels = ~`HOME PORT`, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  

  df_vessel_landingsite_data_breakdown <- vesselsLandingSitesVesselTypesCount(pool)

 

  output$map_vessel_data_breakdown <- renderDataTable(
    df_vessel_landingsite_data_breakdown[,c(1,2,5)],
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
        list(extend = 'csv', filename =  paste("Home port/LandingSite Vessel Data Breakdown in",appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = 'excel', filename = paste("Home port/LandingSite Vessel Data Breakdown in",appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
        list(extend = "pdf", title = paste("Home port/LandingSite Vessel Data Breakdown in",appConfig$country_profile$data$NAME), header = TRUE, orientation = "landscape")
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      )
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

