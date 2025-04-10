#landings1_species_maps_server
landings1_species_maps_server <- function(id, pool, reloader){
  
 moduleServer(id, function(input, output, session){  
   
   ns<-session$ns
   
   output$mode_selector<-renderUI({
     
     selectizeInput(ns("mode"),paste0(i18n("LANDINGS1_SPECIES_MAPS_MODE_LABEL")," :"),choices=c("release","staging"),multiple = F,selected="release")
     
   })
   
   observeEvent(c(input$mode,session$userData$computation_new()),{
     req(!is.null(input$mode)&input$mode!="")
   
     output$year_map_species_selector<-renderUI({
       choices<-unique(getStatPeriods(config = appConfig, id = "artisanal_fisheries_landings1",target=input$mode)$year)
       selectizeInput(ns("year_map_species"), label = i18n("LANDINGS1_SPECIES_MAPS_YEAR_LABEL"), 
                      choice = choices[order(as.numeric(choices))], selected = NULL, 
                      options = list(
                        placeholder = i18n("LANDINGS1_SPECIES_MAPS_YEAR_PLACEHOLDER_LABEL"),
                        onInitialize = I('function() { this.setValue(""); }')
                      ))
     })
   })
  
  output$landings1_species_maps_info <- renderText({
    text <- paste0("<h2>", i18n("LANDINGS1_SPECIES_MAPS_TITLE")," <small>", i18n("LANDINGS1_SPECIES_MAPS_SUBTITLE"),
                   userTooltip("These maps represent the different landings by yearincluding the 1st raised landings (LAN), value (VAL) by landing site and giving proportions by the main species.",
                               style = "font-size: 75%;"),"</small></h2><hr>")
    text
  })
  
  #mapDescriptorYearSpecies
  mapDescriptorYearSpecies <- function(tsdata, year, descriptor, top){
    
    sites_descriptor <- accessLandingSites(pool)
    
    if(!is.null(tsdata)){
      
      #maxValue <- max(tsdata[is.na(tsdata$gear_id)&is.na(tsdata$species_id)&is.na(tsdata$month)&tsdata$descriptor == descriptor,"value"], na.rm = TRUE)
      bch_ts <- tsdata[!is.na(tsdata$species_id)&is.na(tsdata$month)&tsdata$year == year & tsdata$descriptor == descriptor,]
      bch_ts_species <- aggregate(
        bch_ts$value,
        by = list(
          bch_name = bch_ts$bch_name,
          species_name = bch_ts$species_name
        ),
        FUN = function(x){round2(sum(x, na.rm = TRUE))}
      )
      colnames(bch_ts_species)[ncol(bch_ts_species)] <- "value"
      
      #top species
      species <- aggregate(
        bch_ts_species$value,
        by = list(
          species_name = bch_ts_species$species_name
        ),
        FUN = function(x){round2(sum(x,na.rm=TRUE))}
      )
      colnames(species)[ncol(species)] <- "value"
      species <- species[order(species$value, decreasing = TRUE),]
      top_species <- species[1:top,"species_name"]
      
      bch_ts_species_top <- bch_ts_species[bch_ts_species$species_name %in% top_species, ]
      bch_ts_species_top <- bch_ts_species_top[order(match(bch_ts_species_top$species_name,top_species)),]
      bch_ts_species_other <- bch_ts_species[!(bch_ts_species$species_name %in% top_species), ]
      bch_ts_species_other <- aggregate(
        bch_ts_species_other$value,
        by = list(
          bch_name = bch_ts_species_other$bch_name,
          species_name = rep(i18n("LANDINGS1_SPECIES_MAPS_OTHERS_LABEL"), nrow(bch_ts_species_other))
        ),
        FUN = function(x){round2(sum(x, na.rm = TRUE))}
      )
      colnames(bch_ts_species_other)[ncol(bch_ts_species_other)] <- "value"
      bch_ts_species <- rbind(bch_ts_species_top, bch_ts_species_other)
      
      resh <- reshape(bch_ts_species, timevar = "species_name", idvar = "bch_name", direction = "wide", v.names = "value")
      colnames(resh)[startsWith(colnames(resh),"value.")] <- gsub("value.", "", colnames(resh)[startsWith(colnames(resh),"value.")])
      resh$TOTAL <- rowSums(resh[,colnames(resh)[2:ncol(resh)]], na.rm = TRUE)
      
      sites_descriptor <- merge(
        sites_descriptor,
        resh,
        by.x = "NAME",
        by.y = "bch_name",
        all.x = TRUE,
        all.y = FALSE
      )
      sites_descriptor <- sites_descriptor[,c("NAME", colnames(resh)[colnames(resh)!="bch_name"])]
      sites_descriptor <- as(sites_descriptor, "Spatial")
      
      #colors
      qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
      colors <- col_vector[1:(top+1)]
      
      #build the map
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%  
        addMinicharts(
          coordinates(sites_descriptor)[,1L], coordinates(sites_descriptor)[,2L],
          type = "pie",
          chartdata = sites_descriptor@data[,colnames(sites_descriptor@data)[2:(ncol(sites_descriptor@data)-1)]], 
          width = 60 * sqrt(sites_descriptor$TOTAL) / sqrt(max(sites_descriptor$TOTAL, na.rm = TRUE)), transitionTime = 0,
          colorPalette = colors)
    }else{
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%
        addCircles(data = sites_descriptor, weight = 1, color = "#000000", fillColor = "#000000", fillOpacity = 0.7,
                   popup = paste(
                     em(paste0(i18n("LANDINGS1_SPECIES_MAPS_LANDING_SITE_LABEL"),": ")), sites_descriptor$NAME,br()
                   ))
    }
    
  }
  
  #CONTROLLERS
  #TOTAL LANDINGS maps
  #---------------------
  
  tsr <- reactiveValues(
    data = NULL 
  )
  
  observeEvent(input$year_map_species,{
    req(!is.null(input$mode)&input$mode!="")
    req(!is.null(input$year_map_species)&input$year_map_species!="")
    
    target<-getStatPeriods(config=appConfig, "artisanal_fisheries_landings1",target = input$mode)
    target<-subset(target,year==input$year_map_species)$file
    tsdata<-readr::read_csv(target)
    tsdata <- tsdata[order(tsdata$bch_name),]
    tsr$data <- tsdata
  })
  
  #SPECIES LANDINGS maps
  #---------------------
  output$map_species_LAN <- renderLeaflet({
    mapDescriptorYearSpecies(tsr$data, input$year_map_species, "LAN", 15)
  })
  output$map_species_VAL <- renderLeaflet({
    mapDescriptorYearSpecies(tsr$data, input$year_map_species, "VAL", 15)
  })
  
 })
  
}