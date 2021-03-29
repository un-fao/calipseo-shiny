#vessel_breakdown_server
vessel_breakdown_server <- function(input, output, session, pool) {
  
  output$vessel_breakdown_info <- renderText({
    session$userData$page("vessel-breakdown")
    updatePageUrl("vessel-breakdown", session)
    text <- "<h2>Breakdown of vessels <small>Visualize the breakdown of vessels</small></h2><hr>"
    text
  })
  
  #vessels breakdown by type (pie chart)
  output$rep_vessels <- renderPlotly({
    vessel_breakdown = accessVesselsCountByType(pool)
    plot_ly(vessel_breakdown, labels = ~NAME, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
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
  
}
