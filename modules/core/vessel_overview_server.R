#vessel_overview_server
vessel_overview_server <- function(id, pool, reloader) {
  
  moduleServer(id, function(input, output, session) {  
    
    output$vessel_overview_info <- renderText({
      text <- paste0("<h2>", i18n("VESSEL_OVERVIEW_TITLE")," <small>", i18n("VESSEL_OVERVIEW_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
    vessel_info <- accessVesselInfo(pool)
    INFO("vessel-overview server: Fetching vessel info list '%s'", nrow(vessel_info))
    
    country_params<-accessCountryParam(pool)
    
    is_vessel_active_query<-subset(country_params,CODE=="ISVESSELACTIVE")$TEXT
    if(length(is_vessel_active_query)>0){
      is_vessel_active_table<-suppressWarnings(dbGetQuery(pool, is_vessel_active_query))
      names(is_vessel_active_table)<-c("ID","Status")
      is_vessel_active_table$Status[is_vessel_active_table$Status==0]<-i18n("VESSEL_STATUS_INACTIVE")
      is_vessel_active_table$Status[is_vessel_active_table$Status==1]<-i18n("VESSEL_STATUS_ACTIVE")
      vessel_info<-merge(vessel_info,is_vessel_active_table)
    }else{
      vessel_info$Status<-NA
    }
    
    total_nb<-length(unique(vessel_info$ID))
    
    vessel_active_nb<-length(unique(subset(vessel_info,Status==i18n("VESSEL_STATUS_ACTIVE"))$ID))
    
    
    output$indicators<-renderUI({
      div(
        column(12,
               infoBox(i18n("INFOBOX_TITLE_VESSEL_TOTAL"),total_nb , icon = icon("ship"), fill = TRUE,color="blue",width = 6),
               infoBox(i18n("INFOBOX_TITLE_VESSEL_ACTIVE"),vessel_active_nb, icon = icon("circle-check"), fill = TRUE,color="green",width = 6)
               
        )
      )
    })
    
    colVariables<-c()
    
    if(!all(is.na(vessel_info$Status))){
      colVariables<-c(colVariables,c("Status"=i18n("VESSEL_PROPERTY_STATUS")))
      vessel_info$Status[is.na(vessel_info$Status)] <- i18n("VESSEL_UNKNOWN_VALUE")
    }
    
    if(!all(is.na(vessel_info$OPERATIONAL_STATUS))){
      colVariables<-c(colVariables,c("OPERATIONAL_STATUS"=i18n("VESSEL_PROPERTY_OPERATIONAL_STATUS")))
      vessel_info$OPERATIONAL_STATUS[is.na(vessel_info$OPERATIONAL_STATUS)] <- i18n("VESSEL_UNKNOWN_VALUE")
    }
    
    if(!all(is.na(vessel_info$VESSEL_TYPE))){
      colVariables<-c(colVariables,c("VESSEL_TYPE"=i18n("VESSEL_PROPERTY_VESSELTYPE")))
      vessel_info$VESSEL_TYPE[is.na(vessel_info$VESSEL_TYPE)] <- i18n("VESSEL_UNKNOWN_VALUE")
    }
    
    if(!all(is.na(vessel_info$HOME_PORT_LANDING_SITE))){
      colVariables<-c(colVariables,c("HOME_PORT_LANDING_SITE"=i18n("VESSEL_PROPERTY_HOMEPORT")))
      vessel_info$HOME_PORT_LANDING_SITE[is.na(vessel_info$HOME_PORT)] <- i18n("VESSEL_UNKNOWN_VALUE")
    }
    
    if(!all(is.na(vessel_info$REG_PORT_LANDING_SITE))){
      colVariables<-c(colVariables,c("REG_PORT_LANDING_SITE"=i18n("VESSEL_PROPERTY_REGPORT")))
      vessel_info$REG_PORT_LANDING_SITE[is.na(vessel_info$REG_PORT_LANDING_SITE)] <- i18n("VESSEL_UNKNOWN_VALUE")
    }
    
    if(!all(is.na(vessel_info$MANUFACTURER))){
      colVariables<-c(colVariables,c("MANUFACTURER"=i18n("VESSEL_PROPERTY_MANUFACTURER")))
      vessel_info$MANUFACTURER[is.na(vessel_info$MANUFACTURER)] <- i18n("VESSEL_UNKNOWN_VALUE")
    }
    
    if(!all(is.na(vessel_info$ENGINE_TYPE))){
      colVariables<-c(colVariables,c("ENGINE_TYPE"=i18n("VESSEL_PROPERTY_ENGINETYPE")))
      vessel_info$ENGINE_TYPE[is.na(vessel_info$ENGINE_TYPE)] <- i18n("VESSEL_UNKNOWN_VALUE")
    }
    
    if(!all(is.na(vessel_info$ENERGY_TYPE))){
      colVariables<-c(colVariables,c("ENERGY_TYPE"=i18n("VESSEL_PROPERTY_ENERGYTYPE")))
      vessel_info$ENERGY_TYPE[is.na(vessel_info$ENERGY_TYPE)] <- i18n("VESSEL_UNKNOWN_VALUE")
    }
    
    sunburst_data<-vessel_info%>%
      arrange(ID,MANUFACTURER,ENGINE_TYPE,ENERGY_TYPE)%>%
      group_by(ID) %>%
      mutate(MANUFACTURER=paste0(unique(MANUFACTURER),collapse = "+"),
             ENGINE_TYPE=paste0(unique(ENGINE_TYPE),collapse = "+"),
             ENERGY_TYPE=paste0(unique(ENERGY_TYPE),collapse = "+"))%>%
      ungroup()%>%
      distinct()%>%
      mutate(value=1)
    
    pyramid_data<-vessel_info%>%
      filter(OPERATIONAL_STATUS_CODE%in%c(1,2))%>%
      filter(!is.na(REGISTRATION_DATE))%>%
      mutate(Age=round(time_length(interval(REGISTRATION_DATE,Sys.Date()),"years"),0))%>%
      select(-REGISTRATION_DATE,-OPERATIONAL_STATUS_CODE)%>%
      arrange(ID,MANUFACTURER,ENGINE_TYPE,ENERGY_TYPE)%>%
      group_by(ID) %>%
      mutate(MANUFACTURER=paste0(unique(MANUFACTURER),collapse = "+"),
             ENGINE_TYPE=paste0(unique(ENGINE_TYPE),collapse = "+"),
             ENERGY_TYPE=paste0(unique(ENERGY_TYPE),collapse = "+"))%>%
      ungroup()%>%
      distinct()%>%
      filter(Age>0)
    
    pyramid_chart_server("py", df=pyramid_data,colAge="Age",colGender=NULL,colVariables=setNames(names(colVariables),colVariables),mode="plot+table")
    
    sunburst_chart_server("sb", df=sunburst_data,colVariables=colVariables,colValue="value",mode="plot+table")
    
    pretty_table_server("pt", df=sunburst_data,colVariables=colVariables,colValue="value")
    
    output$map_vessels <- renderLeaflet({
      sites_vessels <- accessVesselsCountByLandingSite(pool, sf = TRUE)
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%  
        addCircles(data = sites_vessels, weight = 1, color = "blue", fillColor = "blue", fillOpacity = 0.7, 
                   radius = 700*sqrt(sites_vessels$COUNT/max(sites_vessels$COUNT,na.rm = TRUE)), 
                   popup = paste(
                     em(paste0(i18n("OVERVIEW_VESSEL_HOMEPORT_MAP_LABEL_HOMEPORT"),": ")), sites_vessels$NAME,br(),
                     em(paste0(i18n("OVERVIEW_VESSEL_HOMEPORT_MAP_LABEL_NUMBER_OF_VESSELS"),":")), sites_vessels$COUNT
                   ))
    })
    
    
    
    df_vessel_landingsite_data <- as.data.frame(accessVesselsCountByLandingSite(pool, sf = FALSE))[,-c(2,3)]
    names(df_vessel_landingsite_data) <- c(i18n("OVERVIEW_VESSEL_VESSELTYPE_HOMEPORT_TABLE_COLNAME_1"),
                                           i18n("OVERVIEW_VESSEL_VESSELTYPE_HOMEPORT_TABLE_COLNAME_2"))
    
    
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
          list(extend = 'csv', filename =  paste(i18n("OVERVIEW_VESSEL_VESSELTYPE_HOMEPORT_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
          list(extend = 'excel', filename = paste(i18n("OVERVIEW_VESSEL_VESSELTYPE_HOMEPORT_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
          list(extend = "pdf", title = paste(i18n("OVERVIEW_VESSEL_VESSELTYPE_HOMEPORT_PDF_TITLE"),appConfig$country_profile$data$NAME), header = TRUE, orientation = "landscape")
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
    
    
    
    df_vessel_landingsite_data_breakdown <- vesselsLandingSitesVesselTypesCount(pool, sf = FALSE)[,c(1,2,5)]
    
    names(df_vessel_landingsite_data_breakdown) <- c(i18n("OVERVIEW_VESSELTYPE_LANDINGSITE_TABLE_COLNAME_1"),
                                                     i18n("OVERVIEW_VESSELTYPE_LANDINGSITE_TABLE_COLNAME_2"),
                                                     i18n("OVERVIEW_VESSELTYPE_LANDINGSITE_TABLE_COLNAME_3"))
    
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
          list(extend = 'csv', filename =  paste(i18n("OVERVIEW_VESSELTYPE_LANDINGSITE_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
          list(extend = 'excel', filename = paste(i18n("OVERVIEW_VESSELTYPE_LANDINGSITE_DATA_EXPORT_FILENAME"),appConfig$country_profile$data$NAME), title = NULL, header = TRUE),
          list(extend = "pdf", title = paste(i18n("OVERVIEW_VESSELTYPE_LANDINGSITE_PDF_TITLE"),appConfig$country_profile$data$NAME), header = TRUE, orientation = "landscape")
        ),
        exportOptions = list(
          modifiers = list(page = "all", selected = TRUE)
        ),
        language = list(url = i18n("TABLE_LANGUAGE"))
      ),
      filter = list(position = 'top', clear = FALSE))
    
    
    vessel_landingsite_breakdown <- vesselsLandingSitesVesselTypesCount(pool, sf = TRUE)
    vessel_landingsite_breakdown_df = as.data.frame(vessel_landingsite_breakdown)
    vessel_landingsite_breakdown_df$geometry = NULL
    vessel_landingsite_breakdown_df <- reshape(vessel_landingsite_breakdown_df, direction = 'wide', idvar = c('LATITUDE','LONGITUDE','HOME_PORT_LANDING_SITE'),
                                               timevar = 'VESSEL_TYPE')
    names(vessel_landingsite_breakdown_df) <- gsub("COUNT.", "", names(vessel_landingsite_breakdown_df), fixed = TRUE)
    for (i in 2:ncol(vessel_landingsite_breakdown_df)) {
      vessel_landingsite_breakdown_df[,i]<- as.numeric(vessel_landingsite_breakdown_df[,i])
    }
    if(ncol(vessel_landingsite_breakdown_df)==4){
      vessel_landingsite_breakdown_df$Total = vessel_landingsite_breakdown_df[,4]
    }else{
      vessel_landingsite_breakdown_df$Total <- rowSums(vessel_landingsite_breakdown_df[,4:ncol(vessel_landingsite_breakdown_df)], na.rm = T)
    }
    
    output$map_vessels2 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%
        addCircles(data = vessel_landingsite_breakdown, weight = 1, color = "blue", fillColor = "blue", fillOpacity = 0.7
        ) %>%
        addMinicharts(lng = vessel_landingsite_breakdown_df$LONGITUDE,
                      lat = vessel_landingsite_breakdown_df$LATITUDE,
                      type = "pie",
                      legendPosition = 'bottomright',
                      chartdata = vessel_landingsite_breakdown_df[,c(4:(ncol(vessel_landingsite_breakdown_df)-1))],
                      width = 40 * sqrt(vessel_landingsite_breakdown_df$Total) / sqrt(max(vessel_landingsite_breakdown_df$Total)), transitionTime = 0
                      
        )
    })
    
    
  }) 
}

