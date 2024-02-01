#vessel_overview_server
vessel_overview_server <- function(id, pool) {
  
 moduleServer(id, function(input, output, session) {  
  
  output$vessel_overview_info <- renderText({
    text <- paste0("<h2>", i18n("VESSEL_OVERVIEW_TITLE")," <small>", i18n("VESSEL_OVERVIEW_SUBTITLE"),"</small></h2><hr>")
    text
  })
  
  vessel_info <- accessVesselInfo(pool)
  
  country_params<-accessCountryParam(pool)
  
  is_vessel_active_query<-subset(country_params,CODE=="ISVESSELACTIVE")$TEXT
   if(length(is_vessel_active_query)>0){
     is_vessel_active_table<-suppressWarnings(dbGetQuery(pool, is_vessel_active_query))
     names(is_vessel_active_table)<-c("ID","Active")
     is_vessel_active_table$Active[is_vessel_active_table$Active==0]<-"Vessel Non Active"
     is_vessel_active_table$Active[is_vessel_active_table$Active==1]<-"Vessel Active"
     vessel_info<-merge(vessel_info,is_vessel_active_table)
   }else{
    vessel_info$Active<-NA
   }
  
  total_nb<-length(unique(vessel_info$ID))

  fisher_active_nb<-length(unique(subset(vessel_info,Active=="Vessel Active")$ID))
  
  
  output$indicators<-renderUI({
    div(
      column(12,
             infoBox(i18n("INFOBOX_TITLE_VESSEL_TOTAL"),total_nb , icon = icon("ship"), fill = TRUE,color="blue",width = 6),
             infoBox(i18n("INFOBOX_TITLE_VESSEL_ACTIVE"),fisher_active_nb, icon = icon("circle-check"), fill = TRUE,color="green",width = 6)
             
      )
    )
  })
  
  colVariables<-c()
  
  if(!all(is.na(vessel_info$Active))){
    colVariables<-c(colVariables,c("Active"="Active"))
    vessel_info$Active[is.na(vessel_info$Active)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$OPERATIONAL_STATUS))){
    colVariables<-c(colVariables,c("OPERATIONAL_STATUS"="Operational Status"))
    vessel_info$OPERATIONAL_STATUS[is.na(vessel_info$OPERATIONAL_STATUS)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$VESSEL_TYPE))){
    colVariables<-c(colVariables,c("VESSEL_TYPE"="Vessel Type"))
    vessel_info$VESSEL_TYPE[is.na(vessel_info$VESSEL_TYPE)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$HOME_PORT_LANDING_SITE))){
    colVariables<-c(colVariables,c("HOME_PORT_LANDING_SITE"="Home Port"))
    vessel_info$HOME_PORT_LANDING_SITE[is.na(vessel_info$HOME_PORT)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$REG_PORT_LANDING_SITE))){
    colVariables<-c(colVariables,c("REG_PORT_LANDING_SITE"="Registration Port"))
    vessel_info$REG_PORT_LANDING_SITE[is.na(vessel_info$REG_PORT_LANDING_SITE)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$MANUFACTURER))){
    colVariables<-c(colVariables,c("MANUFACTURER"="Manufacturer"))
    vessel_info$MANUFACTURER[is.na(vessel_info$MANUFACTURER)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$ENGINE_TYPE))){
    colVariables<-c(colVariables,c("ENGINE_TYPE"="Engine Type"))
    vessel_info$ENGINE_TYPE[is.na(vessel_info$ENGINE_TYPE)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$ENERGY_TYPE))){
    colVariables<-c(colVariables,c("ENERGY_TYPE"="Energy type"))
    vessel_info$ENERGY_TYPE[is.na(vessel_info$ENERGY_TYPE)] <- "Unknown"
  }
  
  print(head(vessel_info))
  
  sunburst_data<-vessel_info%>%
    arrange(REGISTRATION_NUMBER,MANUFACTURER,ENGINE_TYPE,ENERGY_TYPE)%>%
    group_by(REGISTRATION_NUMBER) %>%
    mutate(MANUFACTURE=paste0(unique(MANUFACTURER),collapse = "+"),
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
    arrange(REGISTRATION_NUMBER,MANUFACTURER,ENGINE_TYPE,ENERGY_TYPE)%>%
    group_by(REGISTRATION_NUMBER) %>%
    mutate(MANUFACTURE=paste0(unique(MANUFACTURER),collapse = "+"),
           ENGINE_TYPE=paste0(unique(ENGINE_TYPE),collapse = "+"),
           ENERGY_TYPE=paste0(unique(ENERGY_TYPE),collapse = "+"))%>%
    ungroup()%>%
    distinct()%>%
    filter(Age>0)
  
  print("COLVARIABLES")
  print(colVariables)
  print("COLNAMES")
  print(names(pyramid_data))
  
  pyramid_chart_server("py", df=pyramid_data,colAge="Age",colGender=NULL,colVariables=setNames(names(colVariables),colVariables),mode="plot+table")
  
  sunburst_chart_server("sb", df=sunburst_data,colVariables=colVariables,colValue="value",mode="plot+table")
  
  pretty_table_server("pt", df=sunburst_data,colVariables=colVariables,colValue="value")
  
  output$map_vessels <- renderLeaflet({
    sites_vessels <- accessVesselsCountByLandingSite(pool, sf = TRUE)
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%  
      addCircles(data = sites_vessels, weight = 1, color = "blue", fillColor = "blue", fillOpacity = 0.7, 
                 radius = 7000*sqrt(sites_vessels$COUNT/max(sites_vessels$COUNT,na.rm = TRUE)), 
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
  
  

  df_vessel_landingsite_data_breakdown <- vesselsLandingSitesVesselTypesCount(pool)[,c(1,2,5)]

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
  
  
 }) 
}

