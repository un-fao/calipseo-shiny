#logbooks_details_server
logbooks_details_server <- function(id, pool, reloader){
 
 moduleServer(id, function(input, output, session) {  
  
  INFO("logbooks-details: START")
  MODULE_START_TIME <- Sys.time() 
  
  emptyDataFrame <- function(){
    data.frame(year = integer(0), quantity = integer(0), stringsAsFactors = FALSE)
  }
  
  #view controller
  ctrl <- reactiveValues(
    data_for_vessel = emptyDataFrame(),
    data_for_vessel_owner = emptyDataFrame(),
    data_for_map_total = emptyDataFrame(),
    data_for_map_species = emptyDataFrame()
  )
  
  #by vessel
  #---------------------------------------------------------------------- 
  observeEvent(input$logbooks_vessel,{
    
    if(input$logbooks_vessel != ""){
      years <- accessAvailableYears(pool)$year
      data_for_vessel <- do.call("rbind", lapply(years, function(year){
        df <- accessLogBooks(pool, year = year, vesselId = input$logbooks_vessel)
        if(nrow(df)==0) df <- NULL
        if(!is.null(df)){
          df <- data.frame(
            year = year,
            quantity = sum(df$quantity, na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        }
        return(df)
      }))
      if(!is.null(data_for_vessel)){
        data_for_vessel<-data_for_vessel[order(data_for_vessel$year),]
        ctrl$data_for_vessel <- data_for_vessel
      }else{
        ctrl$data_for_vessel <- emptyDataFrame()
      }
    }
  })
  
  output$plot_vessel <- renderPlotly({
    plot_ly(ctrl$data_for_vessel, x = ~ctrl$data_for_vessel[,1], y = ~ctrl$data_for_vessel[,2], mode = 'lines+markers') %>%  layout(autosize = TRUE, height = 290,xaxis = list(title=i18n("TABLE_COLNAME_YEAR")),yaxis =list(title=sprintf('%s (%s)',i18n("TABLE_COLNAME_QUANTITY"),PREF_UNIT_WEIGHT$CODE)))
  })
  
  output$data_vessel <- renderDataTable(
    ctrl$data_for_vessel,
    colnames = c(i18n("TABLE_COLNAME_YEAR"),sprintf('%s (%s)',i18n("TABLE_COLNAME_QUANTITY"),PREF_UNIT_WEIGHT$CODE)),
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
        list(extend = 'csv', filename =  sprintf(paste0(i18n("BY_VESSEL_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_vessel), title = NULL, header = TRUE),
        list(extend = 'excel', filename =  sprintf(paste0(i18n("BY_VESSEL_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_vessel), title = NULL, header = TRUE),
        list(extend = "pdf", filename = sprintf(paste0(i18n("BY_VESSEL_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_vessel), 
             title = sprintf(paste0(i18n("BY_VESSEL_PDF_TITLE")," - %s"), input$logbooks_vessel), header = TRUE)
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      language = list(url = i18n("TABLE_LANGUAGE"))
    )
  )
  
  #by owner
  #----------------------------------------------------------------------
  
  observeEvent(input$logbooks_vessel_owner,{
    if(input$logbooks_vessel_owner != ""){
      years <- accessAvailableYears(pool)$year
      data_for_vessel_owner <- do.call("rbind", lapply(years, function(year){
        df <- accessLogBooks(pool, year = year, entityOwner = input$logbooks_vessel_owner)
        if(nrow(df)==0) df <- NULL
        if(!is.null(df)){
          df <- data.frame(
            year = year,
            quantity = sum(df$quantity, na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        }
        return(df)
      }))
      if(!is.null(data_for_vessel_owner)){
        data_for_vessel_owner<-data_for_vessel_owner[order(data_for_vessel_owner$year),]
        ctrl$data_for_vessel_owner <- data_for_vessel_owner
      }else{
        ctrl$data_for_vessel_owner <- emptyDataFrame()
      }
    }
  })
  
 
  output$plot_vessel_owner <- renderPlotly({
    plot_ly(ctrl$data_for_vessel_owner, x = ~ctrl$data_for_vessel_owner[,1], y = ~ctrl$data_for_vessel_owner[,2], mode = 'lines+markers') %>%  layout(autosize = TRUE, height = 290,xaxis = list(title=i18n("TABLE_COLNAME_YEAR")),yaxis =list(title=sprintf('%s (%s)',i18n("TABLE_COLNAME_QUANTITY"),PREF_UNIT_WEIGHT$CODE)))
  })
  
  output$data_vessel_owner <- renderDataTable(
    ctrl$data_for_vessel_owner,
    colnames = c(i18n("TABLE_COLNAME_YEAR"),sprintf('%s (%s)',i18n("TABLE_COLNAME_QUANTITY"),PREF_UNIT_WEIGHT$CODE)),
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
        list(extend = 'csv', filename =  sprintf(paste0(i18n("BY_OWNER_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_vessel_owner), title = NULL, header = TRUE),
        list(extend = 'excel', filename =  sprintf(paste0(i18n("BY_OWNER_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_vessel_owner), title = NULL, header = TRUE),
        list(extend = "pdf", filename = sprintf(paste0(i18n("BY_OWNER_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_vessel_owner), 
             title = sprintf(paste0(i18n("BY_OWNER_PDF_TITLE"),"- %s"), input$logbooks_vessel_owner), header = TRUE)
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      language = list(url = i18n("TABLE_LANGUAGE"))
    )
  )
  
  #by landing site
  #----------------------------------------------------------------------
  
  topspecies <- 10
  
  #computeTotalsByLandingsite
  computeTotalsByLandingsite <- function(pool, year){
    sites_descriptor <- accessLandingSites(pool)
    tsdata <- accessLogBooks(pool, year)
    if(!is.null(tsdata)){
      tsdata <- aggregate(
        tsdata$quantity,
        by = list(
          bch_id = tsdata$bch_id
        ),
        sum
      )
      colnames(tsdata)[colnames(tsdata)=="x"] <- "quantity"
      sites_descriptor <- merge(
        sites_descriptor,
        tsdata,
        by.x = "ID",
        by.y = "bch_id",
        all.x = TRUE,
        all.y = FALSE
      )
      sites_descriptor <- sites_descriptor[,c("NAME", "quantity")]
    }
    return(sites_descriptor)
  }
  
  #computeSpeciesTotalsByLandingsite
  computeSpeciesTotalsByLandingsite <- function(pool, year, top){
    
    sites_descriptor <- accessLandingSites(pool)
    tsdata <- accessLogBooks(pool, year)
    if(!is.null(tsdata)){
      ts_species <- aggregate(
        tsdata$quantity,
        by = list(
          bch_id = tsdata$bch_id,
          species_desc = tsdata$species_desc
        ),
        FUN = function(x){round2(sum(x, na.rm = TRUE))}
      )
      colnames(ts_species)[ncol(ts_species)] <- "quantity"
      
      #top species
      species <- aggregate(
        ts_species$quantity,
        by = list(
          species_desc = ts_species$species_desc
        ),
        FUN = function(x){round2(sum(x,na.rm=TRUE))}
      )
      colnames(species)[ncol(species)] <- "quantity"
      species <- species[order(species$quantity, decreasing = TRUE),]
      top_species <- species[1:top,"species_desc"]
      
      ts_species_top <- ts_species[ts_species$species_desc %in% top_species, ]
      ts_species_top <- ts_species_top[order(match(ts_species_top$species_desc,top_species)),]
      ts_species_other <- ts_species[!(ts_species$species_desc %in% top_species), ]
      if(nrow(ts_species_other)>0){
        ts_species_other <- aggregate(
          ts_species_other$quantity,
          by = list(
            bch_id = ts_species_other$bch_id,
            species_desc = rep("OTHERS", nrow(ts_species_other))
          ),
          FUN = function(x){round2(sum(x, na.rm = TRUE))}
        )
        colnames(ts_species_other)[ncol(ts_species_other)] <- "quantity"
        ts_species <- rbind(ts_species_top, ts_species_other)
      }
      
      resh <- reshape(ts_species, timevar = "species_desc", idvar = "bch_id", direction = "wide", v.names = "quantity")
      colnames(resh)[startsWith(colnames(resh),"quantity.")] <- gsub("quantity.", "", colnames(resh)[startsWith(colnames(resh),"quantity.")])
      resh$TOTAL <- if(ncol(resh)>2) rowSums(resh[,colnames(resh)[2:ncol(resh)]], na.rm = TRUE) else resh[,2]
      
      sites_descriptor <- merge(
        sites_descriptor,
        resh,
        by.x = "ID",
        by.y = "bch_id",
        all.x = TRUE,
        all.y = FALSE
      )
      sites_descriptor <- sites_descriptor[,c("NAME", colnames(resh)[colnames(resh)!="bch_id"])]
      sites_descriptor <- as(sites_descriptor, "Spatial")
      colnames(sites_descriptor@data) <- gsub("\\.", " ", gsub("\\.\\.", "; ", colnames(sites_descriptor@data)))
      
    }
    return(sites_descriptor)
  }
  
  #mapTotal
  mapTotal <- function(sites_descriptor, color){
    if(nrow(sites_descriptor)>0){
      maxValue <- max(sites_descriptor$quantity, na.rm = TRUE)
      #build the map
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%  
        addCircles(data = sites_descriptor, weight = 1, color = color, fillColor = color, fillOpacity = 0.7, 
                   radius = 700*sqrt(sites_descriptor$quantity/maxValue), 
                   popup = paste(
                     em(paste0(i18n("MAP_POPUP_LANDINGSITE_LABEL"),": ")),sites_descriptor$NAME,br(),
                     em(paste0(i18n("MAP_POPUP_QUANTITY_LABEL"),": ")), sites_descriptor$quantity 
                   ))
    }else{
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%
        addCircles(data = accessLandingSites(pool), weight = 1, color = "#000000", fillColor = "#000000", fillOpacity = 0.7,
                   popup = paste(
                     em(paste0(i18n("MAP_POPUP_LANDINGSITE_LABEL"),": ")), sites_descriptor$NAME,br()
                   ))
    }
  }
  
  #mapSpeciesTotal
  mapSpeciesTotal <- function(sites_descriptor, top){
    
    #colors
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    colors <- col_vector[1:(top+1)]
    
    if(nrow(sites_descriptor)>0){
      #build the map
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%  
        addMinicharts(
          coordinates(sites_descriptor)[,1L], coordinates(sites_descriptor)[,2L],
          type = "pie",
          chartdata = sites_descriptor@data[,colnames(sites_descriptor@data)[2:(ncol(sites_descriptor@data)-1)]], 
          width = 60 * sqrt(sites_descriptor@data[,"TOTAL"]) / sqrt(max(sites_descriptor@data[,"TOTAL"], na.rm = TRUE)), transitionTime = 0,
          colorPalette = colors)
    }else{
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%
        addCircles(data = accessLandingSites(pool), weight = 1, color = "#000000", fillColor = "#000000", fillOpacity = 0.7,
                   popup = paste(
                     em(paste0(i18n("MAP_POPUP_LANDINGSITE_LABEL"),": ")), sites_descriptor[,1],br()
                   ))
    }
  }
  
  #reactives
  observeEvent(input$logbooks_year,{
    if(input$logbooks_year != ""){
      ctrl$data_for_map_total <- computeTotalsByLandingsite(pool, input$logbooks_year)
      ctrl$data_for_map_species <- computeSpeciesTotalsByLandingsite(pool, input$logbooks_year, topspecies)
    }
  })

  #UIs
  output$map_total <- renderLeaflet({
    mapTotal(ctrl$data_for_map_total, color = "#e6550d")
  })
  
  output$data_total <- renderDataTable(
    {
      df <- as.data.frame(ctrl$data_for_map_total)
      df$geometry <- NULL
      df[!is.na(df$quantity),]
    },
    colnames = c(i18n("TABLE_COLNAME_NAME"),sprintf('%s (%s)',i18n("TABLE_COLNAME_QUANTITY"),PREF_UNIT_WEIGHT$CODE)),
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
        list(extend = 'csv', filename =  sprintf(paste0(i18n("BREAKDOWN_OF_TOTAL_QUANTITIES_LANDINGSITE_YEAR_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_year), title = NULL, header = TRUE),
        list(extend = 'excel', filename =  sprintf(paste0(i18n("BREAKDOWN_OF_TOTAL_QUANTITIES_LANDINGSITE_YEAR_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_year), title = NULL, header = TRUE),
        list(extend = "pdf", filename = sprintf(paste0(i18n("BREAKDOWN_OF_TOTAL_QUANTITIES_LANDINGSITE_YEAR_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_year), 
             title = sprintf(paste0(i18n("BREAKDOWN_OF_TOTAL_QUANTITIES_LANDINGSITE_YEAR_PDF_TITLE")," - %s"), input$logbooks_year), header = TRUE)
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      language = list(url = i18n("TABLE_LANGUAGE"))
    )
  )
  output$map_species <- renderLeaflet({
    mapSpeciesTotal(ctrl$data_for_map_species, topspecies)
  })
  
  output$data_species <- renderDataTable(
    {
      df <- ctrl$data_for_map_species
      if(nrow(df)>0 && is(df, "SpatialPointsDataFrame")) df <- df@data
      df
    },
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
        list(extend = 'csv', filename =  sprintf("me %s", input$year_map_species), title = NULL, header = TRUE),
        list(extend = 'excel', filename =  sprintf(paste0(i18n("BREAKDOWN_OF_TOTAL_QUANTITIES_LANDINGSITE_SPICIES_YEAR_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_year), title = NULL, header = TRUE),
        list(extend = "pdf", filename = sprintf(paste0(i18n("BREAKDOWN_OF_TOTAL_QUANTITIES_LANDINGSITE_SPICIES_YEAR_DATA_EXPORT_FILENAME"),"_%s"), input$logbooks_year), 
             title = sprintf(paste0(i18n("BREAKDOWN_OF_TOTAL_QUANTITIES_LANDINGSITE_SPICIES_YEAR_PDF_TITLE")," - %s"), input$logbooks_year), header = TRUE, orientation = "landscape")
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      language = list(url = i18n("TABLE_LANGUAGE"))
    )
  )
  
  MODULE_END_TIME <- Sys.time()
  INFO("logbooks-details: END")
  DEBUG_MODULE_PROCESSING_TIME("Logbooks-details", MODULE_START_TIME, MODULE_END_TIME)
  
 }) 
}