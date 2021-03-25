#vesselListServer
vesselListServer <- function(input, output, session, pool) {

  output$vessel_list_info <- renderText({
    session$userData$page("vessel-list")
    updatePageUrl("vessel-list", session)
    text <- "<h2>List of vessels <small>Access information on vessels</small></h2><hr>"
    text
  })
  
  outp <- accessVessels(pool)
  
  #factorize types for access to codelists
  outp$VESSEL_TYPE <- as.factor(outp$VESSEL_TYPE)
  outp$VESSEL_OPERATIONAL_STATUS <- as.factor(outp$VESSEL_OPERATIONAL_STATUS)
  outp$VESSEL_STAT_TYPE <- as.factor(outp$VESSEL_STAT_TYPE)
  outp$HOME_PORT_LANDING_SITE <- as.factor(outp$HOME_PORT_LANDING_SITE)
  outp$REG_PORT_LANDING_SITE <- as.factor(outp$REG_PORT_LANDING_SITE)
  
  #TODO add buttons
  outp$Details <- sapply(outp$REGISTRATION_NUMBER, function(x){ 
    outhtml <- sprintf("<a href=\"/?page=vessel-info&registrationNumber=%s\" target=\"_blank\">Details</a>", x)
    return(outhtml)
  })
  
  df <- reactiveValues(data = outp)
  
  output$vessel_list <- renderDataTable(
    df$data,
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
        list(extend = 'csv', filename =  "vessels", title = NULL, header = TRUE),
        list(extend = 'excel', filename =  "vessels", title = NULL, header = TRUE),
        list(extend = "pdf", title = "List of vessels", header = TRUE, orientation = "landscape")
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      )
    ),
    filter = list(position = 'top', clear = FALSE))
   
}


#vesselInfoServer
vesselInfoServer <- function(input, output, session, pool, lastETLJob) {
  
  output$vessel_header <- renderText({
    session$userData$page("vessel-info")
    text <- "<h2>Vessel information</h2>"
    text <-paste0(text, "<hr>")
    text <- paste0(text, "<a href=\"/?page=vessel-list\" style=\"float:right;font-weight:bold;\"><< Back to list of vessels</a>")
    text
  })
  
  observe({
    
    vesselId <- NULL
    #inherit vessel Id
    query <- parseQueryString(session$clientData$url_search)
    if(length(query)>0){
      if(!is.null(query[["registrationNumber"]])) {
        cat(sprintf("Selecting vessel '%s'\n", query[["registrationNumber"]]))
        vesselId <- query[["registrationNumber"]]
      }
    }
    
    #inherit vessel information
    vessel <- accessVessel(pool, vesselId)
    vesselOwners <- accessVesselOwners(pool, vesselId)
    vesselOwnerColumnNames <- c("FULL_NAME", "ENTITY_DOCUMENT_NUMBER", "ADDRESS", "ADDRESS_CITY", "ADDRESS_ZIP_CODE", "PHONE_NUMBER", "MOBILE_NUMBER")
    if(nrow(vesselOwners)>0){
      vesselOwners$FULL_NAME <- sapply(1:nrow(vesselOwners), function(i){
        owner <- vesselOwners[i,]
        fullname <- owner$FIRSTNAME
        if(length(owner$MIDDLE_NAME)>0) fullname <- paste0(" ", owner$MIDDLE_NAME)
        if(length(owner$NAME)>0) fullname <- paste0(" ", owner$NAME)
        return(fullname)
      })
      vesselOwners <- vesselOwners[,vesselOwnerColumnNames]
    }else{
      vesselOwners <- data.frame(matrix(ncol = length(vesselOwnerColumnNames), nrow = 0))
      colnames(vesselOwners) <- vesselOwnerColumnNames
    }
    
    vesselCatches <- accessVesselCatches(pool, vesselId)
    if(nrow(vesselCatches)>0){
      vesselCatches$dep_datetime <- as.POSIXct(as.character(vesselCatches$dep_datetime), tz = "UTC")
      vesselCatches$ret_datetime <- as.POSIXct(as.character(vesselCatches$ret_datetime), tz = "UTC")
      atSea = as(vesselCatches$ret_datetime-vesselCatches$dep_datetime, "numeric")
      if(attr(test, "units")=="hours") atSea = atSea/24
      vesselCatches$daysAtSea <- round(atSea, 2)
      
      vesselCatches$year <- as.factor(format(vesselCatches$ret_datetime, "%Y"))
      vesselCatches <- vesselCatches[,c("year", "dep_datetime", "ret_datetime", "daysAtSea", "crew", "gr_f_area", "bch_name", 
                                        "f_mthd", "species_desc", "quantity", "quantity_unit", "value")]
      vesselCatches$gr_f_area <- as.factor(vesselCatches$gr_f_area)
      vesselCatches$bch_name <- as.factor(vesselCatches$bch_name)
      vesselCatches$f_mthd <- as.factor(vesselCatches$f_mthd)
      vesselCatches$species_desc <- as.factor(vesselCatches$species_desc)
      vesselCatches <- vesselCatches[order(vesselCatches$ret_datetime, decreasing = TRUE),]
    }else{
      vesselCatches <- data.frame(
        year = character(0),
        dep_datetime = character(0),
        ret_datetime = character(0),
        daysAtSea = character(0),
        crew = character(0),
        gr_f_area = character(0),
        bch_name = character(0),
        f_mthd = character(0),
        species_desc = character(0),
        quantity = character(0),
        quantity_unit = character(0),
        value = character(0)
      )
    }
    
    #general description
    output$vessel_description <- renderUI({
      tags$ul(
        tags$li(tags$span("Vessel name: ", style = "font-weight:bold;"), tags$span(vessel$NAME)),
        tags$li(tags$span("Vessel type: ", style = "font-weight:bold;"), tags$span(vessel$VESSEL_TYPE)),
        tags$li(tags$span("Registration Number: ", style = "font-weight:bold;"), tags$span(vessel$REGISTRATION_NUMBER)),
        tags$li(tags$span("Registation port: ", style = "font-weight:bold;"), tags$span(vessel$REG_PORT_LANDING_SITE)),
        tags$li(tags$span("Home port: ", style = "font-weight:bold;"), tags$span(vessel$HOME_PORT_LANDING_SITE)),
        class = "vessel"
      )
    })
    
    #ownership
    output$vessel_owners <- renderDataTable(
      vesselOwners,
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
          list(extend = 'csv', filename =  sprintf("vessel_owners_%s", vesselId), title = NULL, header = TRUE),
          list(extend = 'excel', filename =  sprintf("vessel_owners_%s", vesselId), title = NULL, header = TRUE),
          list(extend = "pdf", filename = sprintf("vessel_owners_%s", vesselId), 
               title = sprintf("Vessel '%s' (%s) ownership", vesselId, vessel$name), header = TRUE)
        ),
        exportOptions = list(
          modifiers = list(page = "all", selected = TRUE)
        )
      )
    )
    
    #licenses
    #TODO
    
    #catch summary
    if(nrow(vesselCatches)>0){
      vesselCatchSummary <- cbind(
        aggregate(vesselCatches$daysAtSea, by = list(vesselCatches$year), FUN = sum),
        aggregate(vesselCatches$quantity, by = list(vesselCatches$year), FUN = sum)$x
      )
      colnames(vesselCatchSummary) <- c("year", "daysAtSea", "quantity")
    }else{
      vesselCatchSummary <- data.frame(year = character(0), daysAtSea = character(0), weight_lb = character(0))
    }
    output$vessel_catch_summary <- renderDataTable(
      datatable(vesselCatchSummary,
                rownames = FALSE,
                extensions = c("Buttons"),
                filter = list(position = 'top', clear = FALSE),
                options = list(
                  autoWidth = TRUE,
                  dom = 'Bfrtip',
                  deferRender = TRUE,
                  scroll = FALSE,
                  buttons = list(
                    list(extend = 'copy'),
                    list(extend = 'csv', filename =  sprintf("vessel_catch_summary_%s", vesselId), title = NULL, header = TRUE),
                    list(extend = 'excel', filename =  sprintf("vessel_catch_summary_%s", vesselId), title = NULL, header = TRUE),
                    list(extend = "pdf", filename = sprintf("vessel_catch_summary_%s", vesselId),
                         title = sprintf("Vessel '%s' (%s) Catch summary (as of %s)", vesselId, vessel$name, Sys.Date()), header = TRUE)
                  ),
                  exportOptions = list(
                    modifiers = list(page = "all", selected = TRUE)
                  )
                )),
      server = FALSE
    )
    
    #catch history
    output$vessel_catch_history <- renderDataTable(
      datatable(vesselCatches,
                rownames = FALSE,
                extensions = c("Buttons"),
                filter = list(position = 'top', clear = FALSE),
                options = list(
                  autoWidth = TRUE,
                  dom = 'Bfrtip',
                  deferRender = TRUE,
                  scroll = FALSE,
                  buttons = list(
                    list(extend = 'copy'),
                    list(extend = 'csv', filename =  sprintf("vessel_catch_history_%s", vesselId), title = NULL, header = TRUE),
                    list(extend = 'excel', filename =  sprintf("vessel_catch_history_%s", vesselId), title = NULL, header = TRUE),
                    list(extend = "pdf", filename = sprintf("vessel_catch_history_%s", vesselId), orientation = "landscape",
                         title = sprintf("Vessel '%s' (%s) Catch history (as of %s)", vesselId, vessel$name, Sys.Date()), header = TRUE)
                  ),
                  exportOptions = list(
                    modifiers = list(page = "all", selected = TRUE)
                  )
                )) %>% 
        formatDate(
          'dep_datetime',
          method = "toLocaleString",
          params = list("se", list(timeZone = appConfig$country$timezone))
        ) %>% 
        formatDate(
          'ret_datetime',
          method = "toLocaleString",
          params = list("se", list(timeZone = appConfig$country$timezone))
        ),
      server = FALSE
    )
  })
}

#vesselBreakdownServer
vesselBreakdownServer <- function(input, output, session, pool) {
  
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
  
  #output$map_vessels <- renderLeaflet({
  #  
  #  queryString <-'select longitude , latitude , \'(\'|| cast ( latitude as varchar) ||\', \'||cast ( longitude as varchar)||\')\' coordinates, name as  NAME,  COALESCE (vessels,0) as "Number of vessels"
  #    	from "landing-site" as ls
  #    	left join 
  #    	(SELECT "homePort" code, count(*) as vessels
  #    	FROM public.vessel
  #    	group by "homePort")as p
  #    	on p.code = ls.code
  #       order by name'
  #  
  #  query <- sqlInterpolate(ANSI(),queryString)
  #  outp <- dbGetQuery(pool, query) 
  #  
  #  sites_vessels <- outp
  #  
  #  #build the map
  #  leaflet() %>%
  #    addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%  
  #    addCircles(data = sites_vessels, weight = 1, color = "blue", fillColor = "blue", fillOpacity = 0.7, 
  #               radius = 7000*sqrt(sites_vessels$"Number of vessels"/max(sites_vessels$"Number of vessels",na.rm = TRUE)), 
  #               popup = paste(
  #                 em("Home port: "), sites_vessels$NAME,br(),
  #                 em(paste0("Number of vessels:")), sites_vessels$"Number of vessels"
  #               ))
  #})
  
}
