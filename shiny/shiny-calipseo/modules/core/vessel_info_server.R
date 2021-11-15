#vessel_info_server
vessel_info_server <- function(input, output, session, pool, lastETLJob) {
  
  
  
  
  output$vessel_header <- renderText({
    session$userData$page("vessel-info")
    text <- "<h2>Vessel information</h2>"
    text <-paste0(text, "<hr>")
    text <- paste0(text, "<a href=\"./?page=vessel-list\" style=\"float:right;font-weight:bold;\"><< Back to list of vessels</a>")
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
      vesselCatches$dep_datetime <- as.POSIXct(as.character(vesselCatches$dep_datetime)) 
      attr(vesselCatches$dep_datetime, "tzone") <- appConfig$country_profile$timezone
      vesselCatches$ret_datetime <- as.POSIXct(as.character(vesselCatches$ret_datetime))
      attr(vesselCatches$ret_datetime, "tzone") <- appConfig$country_profile$timezone
      atSea = vesselCatches$ret_datetime-vesselCatches$dep_datetime
      atSea <- switch(attr(atSea, "units"),
                      "mins" = as.numeric(atSea)/60/24,
                      "hours" = as.numeric(atSea)/24,
                      "days" = as.numeric(atSea)
      )
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
      colnames(vesselOwners)
      HTML(
        
        '<br>Vessel name:',vessel$NAME,'</br>',
        '<br>Vessel type:', vessel$VESSEL_TYPE,'</br>',
        '<br>Vessel stat type:', vessel$VESSEL_STAT_TYPE,'</br>',
        '<br>Home port:', vessel$HOME_PORT_LANDING_SITE,'</br>'
        
      )
    })
    
    
    
    #registration
    output$vessel_registration <- renderUI({
      
      HTML(
        
        '<br>Registration Number:', vessel$REGISTRATION_NUMBER,'</br>',
        '<br>Registation port:', vessel$REG_PORT_LANDING_SITE,'</br>'
        
      )
    })
    
    #ownership
    output$vessel_owners <- renderDataTable(server = FALSE,{
      names(vesselOwners) <- c("Full Name", "Entity Document Number", "Address", "Address City", "Address Zip Code", "Pone Number", "Mobile Number")
      
      datatable(
      vesselOwners,
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
      ))
    })
    
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
    
    
    output$vessel_catch_summary <- renderDataTable(server = FALSE,{
      names(vesselCatchSummary) <- c("Year", "Days At Sea", "Quantity")
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
                ))
    })
    
    #catch history
    output$vessel_catch_history <- renderDataTable(server = FALSE,{
      
      names(vesselCatches) <- c("Year", "Departure Datetime", "Return Datetime", "Days At Sea", "Crew", "Greater Fishing Area", "Beach Name",
                   "Fishing Method", "Species Description", "Quantity", "Quantity Unit", "Value")

     
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
          'Departure Datetime',
          method = "toLocaleString",
          params = list("se", list(timeZone = appConfig$country_profile$timezone)) #TODO check if needed
        ) %>% 
        formatDate(
          'Return Datetime',
          method = "toLocaleString",
          params = list("se", list(timeZone = appConfig$country_profile$timezone)) #TODO check if needed
        )
    })
    
    #catch data source
    output$vessel_catch_datasource <- renderUI({
      tags$small(switch(vessel$VESSEL_STAT_TYPE_CODE,
        "ART" = "From sample-based survey",
        "INDUS" = "From lobgooks"
      ))
    })
  })
}