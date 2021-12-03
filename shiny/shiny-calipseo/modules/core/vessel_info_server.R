#vessel_info_server
vessel_info_server <- function(input, output, session, pool, lastETLJob) {
  
  output$vessel_header <- renderText({
    session$userData$page("vessel-info")
    text <- "<h2>Vessel information</h2>"
    text <-paste0(text, "<hr>")
    text <- paste0(text, "<a href=\"./?page=vessel-list\" style=\"float:right;font-weight:bold;margin-right:10px;\"><< Back to list of vessels</a>")
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
    
    #name
    output$vessel_name <- renderUI({ tags$b(vessel$NAME) })
    
    #general description
    output$vessel_description <- renderUI({
      colnames(vesselOwners)
      tags$ul(style = "margin-top:10px;",
              tags$li('Vessel name: ', tags$b(vessel$NAME)),
              tags$li('Vessel type: ', tags$b(vessel$VESSEL_TYPE)),
              tags$li('Vessel stat type: ', tags$b(vessel$VESSEL_STAT_TYPE)),
              tags$li('Home port: ', tags$b(vessel$HOME_PORT_LANDING_SITE))
      )
    })
    
    #registration
    output$vessel_registration <- renderUI({
      tags$ul(style = "margin-top:10px;",
              tags$li('Registration Number: ', tags$b(vessel$REGISTRATION_NUMBER)),
              tags$li('Registation port: ', tags$b(vessel$REG_PORT_LANDING_SITE))
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
    output$license_table <- renderDT(server = FALSE,{
      
      vessellicensepermits <- accessVesselLicensePermit(pool,vesselId)
      
      if(nrow(vessellicensepermits>0)){
        
        dt <- reactive({
          
          unique_permits <- dplyr::distinct(vessellicensepermits, PERMIT_NUMBER,.keep_all = TRUE)
          
          unique_gears <- unique(vessellicensepermits$Gears)
          
          vessellicensepermits$Gears <- paste0(unique_gears, collapse = ',')
          
          vessellicensepermits$Valid_to_date <- as.Date(vessellicensepermits$Valid_to_date)
          
          valid_to_date <- vessellicensepermits$Valid_to_date
          
          vessellicensepermits$Validity <- NA
          
          for (i in 1:length(valid_to_date)) {
            validity_status <- Sys.Date()-valid_to_date[i]
            
            if(validity_status<0){
              vessellicensepermits$Validity[i] <- 'ok'
            }else{
              
              vessellicensepermits$Validity[i] <- 'remove'
            }
          }
          
          vessellicensepermits <- dplyr::arrange(vessellicensepermits, desc(Valid_to_date))
          
          names(vessellicensepermits)<- c('Permit Number', 'Application Date', 'Permit Date',
                                          'Valid From (Date)', 'Valid To (Date)', 'Gears', 'Validity')
          
          vessellicensepermits <- vessellicensepermits[,c(1,2,3,4,5,7,6)]
          
          return(unique(vessellicensepermits))
          
        })
        
        
        
        DT::datatable(dt(),
                      rownames = FALSE, extensions = c("Select","Buttons"),
                      selection = "none",
                      filter = list(position = 'top', clear = FALSE),
                      
                      options = list(
                        autoWidth = TRUE,
                        dom = 'Bfrtip',
                        deferRender = TRUE,
                        scroll = FALSE,
                        buttons = list(
                          list(extend = 'copy'),
                          list(extend = 'csv', filename =  sprintf("vessel_license_permits_%s", vesselId), title = NULL, header = TRUE),
                          list(extend = 'excel', filename =  sprintf("vessel_license_permits_%s", vesselId), title = NULL, header = TRUE),
                          list(extend = "pdf", filename = sprintf("vessel_license_permits_%s", vesselId), orientation = "landscape",
                               title = sprintf("Vessel '%s' (%s) License Permits (as of %s)", vesselId, vessel$name, Sys.Date()), header = TRUE)
                        ),
                        exportOptions = list(
                          modifiers = list(page = "all", selected = TRUE)
                        ),
                        
                        
                        columnDefs = list(
                          list(targets = 5, render = JS(js_render_for_license_table)) 
                        )
                      ))
        
        
        
      }else{
        
        vessellicensepermits <- data.frame(
          `Permit Number` = character(0), 
          'Application Date' = character(0), 
          'Permit Date' = character(0),
          'Valid From (Date)' = character(0), 
          'Valid To (Date)' = character(0), 
          'Validity' = character(0),
          'Gears' = character(0)
          
        )
        
        names(vessellicensepermits)<- c('Permit Number', 'Application Date', 'Permit Date',
                                        'Valid From (Date)', 'Valid To (Date)', 'Validity', 'Gears')
        
        DT::datatable(vessellicensepermits)
        
      }
      
    })
    
    
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
    
    
    vessel_found <- vesselFindeR(vessel$NAME, appConfig$country_profile$data$ISO_2_CODE)
    
    
    if(!is.null(vessel_found$img_href)){
      vessel_picture_html <- createBase64Image(src = vessel_found$img_href, width = "250px", alt = vessel$NAME)
      vessel_picture_html <- HTML(vessel_picture_html,paste0("<div style=\"font-size:80%\">Image source: <a href=",vessel_found$link, " target=\"_blank\" a> VesselFinder </a></div>"))
    }else{
      vessel_picture_html <- HTML(createPlaceholderImage("vessel"))
    }
    
    output$vessel_picture <- renderUI({
      
      vessel_picture_html
      
    })
    
    
    SpeciesCatchesYear <- accessSpeciesCatchesYear(pool,vesselId)
    
    
    rank_species <- SpeciesCatchesYear %>%
      mutate(rank = rank(-catches)) %>%
      filter(rank <=10)
    
    
    df_SpeciesCatchesYear <- SpeciesCatchesYear %>%
      filter(species_desc %in% rank_species$species_desc)
    
    df_SpeciesCatchesYear$year <- as.factor(df_SpeciesCatchesYear$year)
    
    
    output$catches_piechart <- renderPlotly({
      
      
      plot_ly(df_SpeciesCatchesYear, labels = ~species_desc, values = ~catches, type = 'pie', sort = FALSE, direction = "clockwise") %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(orientation = "h"))
      
    })
    
    output$catches_lineplot <- renderPlotly({
      
      fig_sp <- df_SpeciesCatchesYear %>% plot_ly()
      fig_sp<-fig_sp%>% add_trace(
        x = ~year, 
        y = ~catches,
        split=~species_desc,
        type = 'scatter', 
        mode = 'lines',
        line = list(simplyfy = F),
        text = ~paste("Year: ", year, "<br>catches: ", catches, "<br>species: ",species_desc)
      )
      
      fig_sp <- fig_sp %>% layout(
        hovermode ='closest',
        legend = list(orientation = "h",
                      font = list(size = 10),
                      bgcolor ='rgba(0,0,0,0)',
                      xanchor = "center",
                      yanchor = "top",
                      y =-0.1,
                      x = 0.5),
        xaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          title = "Year",
          zeroline = F
        ),
        yaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          range = range(df_SpeciesCatchesYear$catches),
          title = "Catches (Tons)",
          zeroline = F
        ))
      
      fig_sp
      
      
    })
    
    
    
    ftpv <- countFishingTripsPerVessel(pool,vesselId)
    
    if(nrow(ftpv)>0){
      ftpv$No_years_trips_made <- nrow(ftpv)
      ftpv$sum_no_trips_per_year <- sum(ftpv$sum_no_trips_per_year)
    }else{
      ftpv <- data.frame(
        Year= character(0),
        sum_no_trips_per_year = numeric(0),
        No_years_trips_made = numeric(0)
      )
    }
    
    ftpv$Mean <- ftpv$sum_no_trips_per_year/ftpv$No_years_trips_made
    ftpv <- ftpv[,c(1,4)]
    ftpv$Mean <- round(ftpv$Mean, digits = 2)
    
    
    
    vesselDaysATSea <- countVesselDaysAtSea(pool,vesselId)
    
    for (i in 2:ncol(vesselDaysATSea)) {
      vesselDaysATSea[,i] <- as.POSIXct(as.character(vesselDaysATSea[,i]))
      attr(vesselDaysATSea[,i], "tzone") <- appConfig$country_profile$timezone
    }
    
    DaysatSea = vesselDaysATSea$ret_datetime-vesselDaysATSea$dep_datetime
    DaysatSea <- switch(attr(DaysatSea, "units"),
                        "mins" = as.numeric(DaysatSea)/60/24,
                        "hours" = as.numeric(DaysatSea)/24,
                        "days" = as.numeric(DaysatSea)
    )
    vesselDaysATSea$daysAtSea <- round(as.numeric(DaysatSea), 2)
    
    sum_no_daysAtSea <- sum(as.numeric(vesselDaysATSea$daysAtSea), na.rm = TRUE)
    No_of_trips <- nrow(vesselDaysATSea)
    Mean_no_daysAtSea <- round(sum_no_daysAtSea/No_of_trips, digits = 2)
    
    
    vessel_infos_fetched <- reactiveVal(FALSE)
    vessel_indicators_infos <- reactiveValues(
      vessel_operational_status = NULL,
      number_of_owners = NULL,
      number_of_licenses = NULL,
      number_of_fishing_gears = NULL,
      mean_number_of_days_at_sea = NULL,
      mean_number_of_fishing_trips = NULL,
      number_of_landing_sites = NULL,
      number_of_species_fished = NULL )
    
    vessel_indicators_infos$vessel_operational_status <- as.character(vessel$VESSEL_OPERATIONAL_STATUS)
    vessel_indicators_infos$number_of_owners <- as.character(countVesselOwnersPerVessel(pool,vesselId))
    vessel_indicators_infos$number_of_licenses <- as.character(countVesselLicensePermit(pool,vesselId))
    vessel_indicators_infos$number_of_fishing_gears <- as.character(countVesselFishingGears(pool,vesselId))
    vessel_indicators_infos$number_of_landing_sites <- as.character(length(levels(vesselCatches$bch_name))) 
    vessel_indicators_infos$number_of_species_fished <- as.character(length(levels(vesselCatches$species_desc)))
    vessel_indicators_infos$mean_number_of_days_at_sea <- as.character(Mean_no_daysAtSea)
    vessel_indicators_infos$mean_number_of_fishing_trips <- as.character(ftpv$Mean[1])
    
    if(all(!sapply(reactiveValuesToList(vessel_indicators_infos), is.null))) vessel_infos_fetched(TRUE)
    
    
    output$box_status <- renderUI({
      #TODO change color depending on operational status
      infoBox('Vessel Operational Status',icon = icon('check'),vessel_indicators_infos$vessel_operational_status, fill = TRUE, width = 6)
    })
    
    output$box_owner <- renderUI({
      infoBox('Number of owners',icon = icon('user'),vessel_indicators_infos$number_of_owners, fill = TRUE, width = 6)
    })
    
    output$box_license <- renderUI({
      infoBox('Number of licenses',icon = icon('ship'),vessel_indicators_infos$number_of_licenses, fill = TRUE, width = 6)
    })
    
    output$box_gears <- renderUI({
      infoBox('Number of fishing gears',icon = icon('gear'),vessel_indicators_infos$number_of_fishing_gears, fill = TRUE, width = 6)
    })
    
    output$more_indicators <- renderUI({
      fluidRow(
        infoBox(span('Mean fishing trips/year',style='font-size:10px;'),icon = icon('line-chart'),vessel_indicators_infos$mean_number_of_fishing_trips, fill = TRUE,width = 3),
        infoBox(span('Mean days at sea / fishing trip',style='font-size:10px;'),icon = icon('line-chart'),vessel_indicators_infos$mean_number_of_days_at_sea, fill = TRUE,width = 3),
        infoBox(span('Number of landingsites',style='font-size:10px;'),icon = icon('ship'),vessel_indicators_infos$number_of_landing_sites, fill = TRUE,width = 3),
        infoBox(span('Nnumber of species caught',style='font-size:10px;'),icon = icon('fish'),vessel_indicators_infos$number_of_species_fished, fill = TRUE,width = 3)
      )
    })
    
    
  })
  
 
}