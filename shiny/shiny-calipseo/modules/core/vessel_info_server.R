#vessel_info_server
vessel_info_server <- function(input, output, session, pool, lastETLJob) {
  
  output$vessel_header <- renderText({
    session$userData$page("vessel-info")
    text <- paste0("<a href=\"./?page=vessel-list\" style=\"float:right;font-weight:bold;margin-right:10px;\">",i18n("BACK_TO_LIST_OF_VESSELS"),"</a>")
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
        INFO("Displaying info on vessel registration number '%s'", vesselId)
      }
    }
    
    if(!is.null(vesselId)){
      
      #vessel owners information
      vessel <- accessVessel(pool, vesselId)
      vesselOwners <- accessVesselOwners(pool, vesselId)
      vesselOwnerColumnNames <- c("ENTITY_TYPE","FULL_NAME", "ENTITY_DOCUMENT_NUMBER", "ADDRESS", "ADDRESS_CITY", "ADDRESS_ZIP_CODE", "PHONE_NUMBER", "MOBILE_NUMBER")
      vesselOwners[is.na(vesselOwners)] = '-'
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
        
        INFO("Returning NULL dataframe for vesselOwners")
        
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
        
        INFO("Returning NULL dataframe for vesselcatches")
        
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
      output$vessel_name <- renderUI({tags$b(paste0(i18n("VESSEL_INFO_TITLE")," - ",vessel$NAME))})
      
      #general description
      output$vessel_description <- renderUI({
        colnames(vesselOwners)
        tags$ul(style = "margin-top:10px;",
                tags$li(paste0(i18n("VESSEL_NAME"),": "), tags$b(vessel$NAME)),
                tags$li(paste0(i18n("VESSEL_TYPE"),": "), tags$b(vessel$VESSEL_TYPE)),
                tags$li(paste0(i18n("VESSEL_STAT_TYPE"),": "), tags$b(vessel$VESSEL_STAT_TYPE)),
                tags$li(paste0(i18n("HOME_PORT"),": "), tags$b(vessel$HOME_PORT_LANDING_SITE)),
                tags$li('IRCS: ', tags$b(ifelse(!is.na(vessel$IRCS),vessel$IRCS, '-'))),
                tags$li('IMO: ', tags$b(ifelse(!is.na(vessel$IMO),vessel$IMO, '-')))
        )
      })
      
      #registration
      output$vessel_registration <- renderUI({
        tags$ul(style = "margin-top:10px;",
                tags$li(paste0(i18n("REGISTRATION_NUMBER"),": "), tags$b(vessel$REGISTRATION_NUMBER)),
                tags$li(paste0(i18n("REGISTRATION_PORT"),": "), tags$b(vessel$REG_PORT_LANDING_SITE))
        )
      })
      
      
      #history
      
      historical_df <- reactive({
        historical_data <- accessVesselHistoricalCharacteristics(pool,vesselId)
        
        if(nrow(historical_data)>0){
          
          const_data <- historical_data[,c(1,10,11)]
          
          var_data <-  historical_data[,c(2:9)]
          
          old_data <- var_data[,c(1,3,5,7)]
          new_data <- var_data[,-c(1,3,5,7)]
          
          
          old_data <- within(old_data,  value <- paste(OLD_VALUE_FLOAT,OLD_ID_LABEL,OLD_VALUE_STRING,OLD_VALUE_DATE, sep=""))
          new_data <- within(new_data,  value <- paste(NEW_VALUE_FLOAT,NEW_ID_LABEL,NEW_VALUE_STRING,NEW_VALUE_DATE, sep=""))
          
          old_data <- old_data[,5]
          new_data <- new_data[,5]
          
          cl_data <- function(data){
            data <- gsub('NA', '',data)
            data <- gsub("[ \t](2,)", "", data)
            data<- gsub("^\\s+|\\s+$", "", data)
            return(data)
          }
          
          
          cl_df <- data.frame(
            old=cl_data(old_data),
            new=cl_data(new_data)
          )
          
          df <- cbind(cl_df,const_data)
          
          for (i in 1:4) {
            
            df[,i][df[,i]==""] <- 'NULL'
            
            df[,i][df[,i]=='N/AN/A'] <- 'N/A'
            
          }
          
          df <- df[,c(3,4,1,2,5)]
          print(df)
          df$UPDATED_AT <- as.POSIXct(as.character(df$UPDATED_AT))
          attr(df$UPDATED_AT, "tzone") <- appConfig$country_profile$timezone
          df <- df[order(rank(df$UPDATED_AT),decreasing=TRUE),]
          df$UPDATED_AT <- as.character.Date(df$UPDATED_AT)
          
        }else{
          
          INFO("Returning NULL dataframe for historical data")
          
          df <- data.frame(
            Type = character(0),
            Description = character(0),
            Old_Value = character(0),
            New_Value = character(0),
            Change_Date = character(0)
          )
        }
        return(df)
      })
      
      
      output$vessel_history <- renderDataTable(server = FALSE,{
        datatable(
          historical_df(),
          escape = FALSE,
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
              list(extend = 'csv', filename =  sprintf(i18n("HISTORY_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
              list(extend = 'excel', filename =  sprintf(i18n("HISTORY_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
              list(extend = "pdf", filename = sprintf(i18n("HISTORY_DATA_EXPORT_FILENAME"), vesselId), 
                   title = sprintf("HISTORY_PDF_TITLE", vesselId, vessel$NAME), header = TRUE)
            ),
            exportOptions = list(
              modifiers = list(page = "all", selected = TRUE)
            ),
            language = list(url = i18n("TABLE_LANGUAGE"))
          ),
          colnames = c(i18n("HISTORY_TABLE_COLNAME_1"),i18n("HISTORY_TABLE_COLNAME_2"),
                       i18n("HISTORY_TABLE_COLNAME_3"),i18n("HISTORY_TABLE_COLNAME_4"),
                       i18n("HISTORY_TABLE_COLNAME_5"))
        )
        
      })
      
      #ownership
      output$vessel_owners <- renderDataTable(server = FALSE,{
        names(vesselOwners) <- c(i18n("OWNERSHIP_TABLE_COLNAME_0"),
                                 i18n("OWNERSHIP_TABLE_COLNAME_1"),i18n("OWNERSHIP_TABLE_COLNAME_2"),
                                 i18n("OWNERSHIP_TABLE_COLNAME_3"),i18n("OWNERSHIP_TABLE_COLNAME_4"),
                                 i18n("OWNERSHIP_TABLE_COLNAME_5"),i18n("OWNERSHIP_TABLE_COLNAME_6"),
                                 i18n("OWNERSHIP_TABLE_COLNAME_7"))
        
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
              list(extend = 'csv', filename =  sprintf(i18n("OWNERSHIP_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
              list(extend = 'excel', filename =  sprintf(i18n("OWNERSHIP_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
              list(extend = "pdf", filename = sprintf(i18n("OWNERSHIP_DATA_EXPORT_FILENAME"), vesselId), 
                   title = sprintf(i18n("OWNERSHIP_PDF_TITLE"), vesselId, vessel$NAME), header = TRUE)
            ),
            exportOptions = list(
              modifiers = list(page = "all", selected = TRUE)
            ),
            language = list(url = i18n("TABLE_LANGUAGE"))
          ))
      })
      
      #licenses
      
      license_df <- reactive({
        
        vessellicensepermits <- accessVesselLicensePermit(pool,vesselId)
        
        if(nrow(vessellicensepermits>0)){
          
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
          
          vessellicensepermits <- vessellicensepermits[order(rank(vessellicensepermits$Valid_to_date),decreasing=TRUE),]
          vessellicensepermits <- vessellicensepermits[,-7]
          names(vessellicensepermits)<- c(i18n("LICENCES_TABLE_COLNAME_1"),i18n("LICENCES_TABLE_COLNAME_2"),
                                          i18n("LICENCES_TABLE_COLNAME_3"),i18n("LICENCES_TABLE_COLNAME_4"),
                                          i18n("LICENCES_TABLE_COLNAME_5"),i18n("LICENCES_TABLE_COLNAME_6"),
                                          i18n("LICENCES_TABLE_COLNAME_7"))
          vessellicensepermits <- vessellicensepermits[,c(1,2,3,4,5,7,6)]
          
          vessellicensepermits$Validity_status <- vessellicensepermits$Validity
          
          vessellicensepermits$Validity[vessellicensepermits$Validity=='ok'] <- paste(tags$span(title=i18n("LICENSE_STATUS_VALID_TOOLTIP"),style = 'color:green;font-size:18px;',icon("ok",lib = "glyphicon")))
          
          vessellicensepermits$Validity[vessellicensepermits$Validity=='remove'] <- paste(tags$span(title=i18n("LICENSE_STATUS_EXPIRED_TOOLTIP"),style = 'color:red;font-size:18px;',icon("remove",lib = "glyphicon")))
          
          
        }else{
          INFO("Returning NULL dataframe for vessel license permits")
          
          vessellicensepermits <- data.frame(
            `Permit Number` = character(0),
            'Application Date' = character(0),
            'Permit Date' = character(0),
            'Valid From (Date)' = character(0),
            'Valid To (Date)' = character(0),
            'Validity' = character(0),
            'Gears' = character(0)
            
          )
          
          names(vessellicensepermits)<- c(i18n("LICENCES_TABLE_COLNAME_1"),i18n("LICENCES_TABLE_COLNAME_2"),
                                          i18n("LICENCES_TABLE_COLNAME_3"),i18n("LICENCES_TABLE_COLNAME_4"),
                                          i18n("LICENCES_TABLE_COLNAME_5"),i18n("LICENCES_TABLE_COLNAME_6"),
                                          i18n("LICENCES_TABLE_COLNAME_7"),i18n("LICENCES_TABLE_COLNAME_8"))
          
          
        }
        
        return(unique(vessellicensepermits))
      })
      
      
      output$license_table <- renderDT(server = FALSE,{
        
        if(nrow(license_df()>0)){
          
          DT::datatable(license_df()[,1:7],
                        rownames = FALSE, extensions = c("Select","Buttons"),
                        selection = "none",
                        filter = list(position = 'top', clear = FALSE),
                        escape = FALSE,
                        options = list(
                          autoWidth = TRUE,
                          dom = 'Bfrtip',
                          deferRender = TRUE,
                          scroll = FALSE,
                          buttons = list(
                            list(extend = 'copy'),
                            list(extend = 'csv', filename =  sprintf(i18n("LICENCES_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
                            list(extend = 'excel', filename =  sprintf(i18n("LICENCES_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
                            list(extend = "pdf", filename = sprintf(i18n("LICENCES_DATA_EXPORT_FILENAME"), vesselId), orientation = "landscape",
                                 title = sprintf(i18n("LICENCES_PDF_TITLE"), vesselId, vessel$NAME, Sys.Date()), header = TRUE)
                          ),
                          exportOptions = list(
                            modifiers = list(page = "all", selected = TRUE)
                          ),
                          
                          language = list(url = i18n("TABLE_LANGUAGE"))
                        ))
          
        }else{
          
          DT::datatable(license_df())
          
        }
        
      })
      
      #warnings
      warning_msg <- reactive({
        if(nrow(vesselCatches)>0){
          if(accessVesselCatches(pool, vesselId)$stat_type_id=='1'){
            div(class="alert alert-warning", role="alert",style='font-size:90%;',
                icon("warning", "fa-2x"), tags$em(i18n("WARNING_MESSAGE"))  
            )
          }
        }
        
      })
      
      #warningvesselstattypefishingtrips
      output$warning_vessel_stat_type_fishingtrips <- renderUI({
        warning_msg()
      })
      
      
      #warningvesselstattypecatches
      output$warning_vessel_stat_type_catches <- renderUI({
        warning_msg()
      })
      
      
      #fishing trips chart
      trip_gantt_server(id="fishing_trips_chart",pool,vessel_stat_type=NULL,vesselId=vesselId,mode="light")
      
      
      #catch summary
      if(nrow(vesselCatches)>0){
        
        ActualDaysAtSea_df <- distinct(vesselCatches,ret_datetime, .keep_all=TRUE)
        
        ActualDaysAtSea_df$daysAtSea = difftime(ActualDaysAtSea_df$ret_datetime,ActualDaysAtSea_df$dep_datetime, units = 'days')
        
        ActualDaysAtSea_df$daysAtSea <- round(ActualDaysAtSea_df$daysAtSea, 2)
        
        vesselCatchSummary <- cbind(
          aggregate(ActualDaysAtSea_df$daysAtSea, by = list(ActualDaysAtSea_df$year), FUN = sum),
          aggregate(vesselCatches$quantity, by = list(vesselCatches$year), FUN = sum)$x
        )
        
        colnames(vesselCatchSummary) <- c("year", "daysAtSea", "quantity")
        
      }else{
        vesselCatchSummary <- data.frame(year = character(0), daysAtSea = character(0), weight_lb = character(0))
        
      }
      
      
      output$vessel_catch_summary <- renderDataTable(server = FALSE,{
        names(vesselCatchSummary) <- c(i18n("SUMMARY_CATCHES_COLNAME_1"),i18n("SUMMARY_CATCHES_COLNAME_2"),
                                       i18n("SUMMARY_CATCHES_COLNAME_3"))
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
                      list(extend = 'csv', filename =  sprintf(i18n("SUMMARY_CATCHES_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
                      list(extend = 'excel', filename =  sprintf(i18n("SUMMARY_CATCHES_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
                      list(extend = "pdf", filename = sprintf(i18n("SUMMARY_CATCHES_DATA_EXPORT_FILENAME"), vesselId),
                           title = sprintf(i18n("SUMMARY_CATCHES_PDF_TITLE"), vesselId, vessel$NAME, Sys.Date()), header = TRUE)
                    ),
                    exportOptions = list(
                      modifiers = list(page = "all", selected = TRUE)
                    ),
                    language = list(url = i18n("TABLE_LANGUAGE"))
                  ))
      })
      
      #catch history
      output$vessel_catch_history <- renderDataTable(server = FALSE,{
        
        names(vesselCatches) <- c(i18n("HISTORY_CATCHES_COLNAME_1"),i18n("HISTORY_CATCHES_COLNAME_2"),i18n("HISTORY_CATCHES_COLNAME_3"),
                                  i18n("HISTORY_CATCHES_COLNAME_4"),i18n("HISTORY_CATCHES_COLNAME_5"),i18n("HISTORY_CATCHES_COLNAME_6"),
                                  i18n("HISTORY_CATCHES_COLNAME_7"),i18n("HISTORY_CATCHES_COLNAME_8"),i18n("HISTORY_CATCHES_COLNAME_9"),
                                  i18n("HISTORY_CATCHES_COLNAME_10"),i18n("HISTORY_CATCHES_COLNAME_11"),i18n("HISTORY_CATCHES_COLNAME_12"))
        
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
                      list(extend = 'csv', filename =  sprintf(i18n("HISTORY_CATCHES_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
                      list(extend = 'excel', filename =  sprintf(i18n("HISTORY_CATCHES_DATA_EXPORT_FILENAME"), vesselId), title = NULL, header = TRUE),
                      list(extend = "pdf", filename = sprintf(i18n("HISTORY_CATCHES_DATA_EXPORT_FILENAME"), vesselId), orientation = "landscape",
                           title = sprintf(i18n("HISTORY_CATCHES_PDF_TITLE"), vesselId, vessel$NAME, Sys.Date()), header = TRUE)
                    ),
                    exportOptions = list(
                      modifiers = list(page = "all", selected = TRUE)
                    ),
                    language = list(url = i18n("TABLE_LANGUAGE"))
                  )) %>% 
          formatDate(
            i18n("HISTORY_CATCHES_COLNAME_2"),
            method = "toLocaleString",
            params = list("se", list(timeZone = appConfig$country_profile$timezone)) #TODO check if needed
          ) %>% 
          formatDate(
            i18n("HISTORY_CATCHES_COLNAME_3"),
            method = "toLocaleString",
            params = list("se", list(timeZone = appConfig$country_profile$timezone)) #TODO check if needed
          )
      })
      
      #catch data source
      output$vessel_catch_datasource <- renderUI({
        tags$small(switch(vessel$VESSEL_STAT_TYPE_CODE,
                          "ART" = i18n("CATCHES_DATA_SOURCE_ART"),
                          "INDUS" = i18n("CATCHES_DATA_SOURCE_INDUS")
        ))
      })
      
      
      vessel_found <- vesselFindeR(vessel$NAME, appConfig$country_profile$data$ISO_2_CODE)
      
      
      
      if(nrow(vessel_found)>0){
        vessel_picture_html <- createBase64Image(src = vessel_found$Value[2], width = "180px", alt = vessel$NAME)
        vessel_picture_html <- HTML(vessel_picture_html,paste0("<div style=\"font-size:80%\">",paste0(i18n("IMAGE_SOURCE"),":"),"<a href=",vessel_found$Value[1], " target=\"_blank\" a>",i18n("VESSEL_FINDER"),"</a></div>"))
      }else{
        
        INFO("Returning Placeholder image for vessel")
        
        vessel_picture_html <- HTML(createPlaceholderImage("vessel"))
      }
      
      output$vessel_picture <- renderUI({
        
        vessel_picture_html
        
      })
      
      #vesselcharacteristics
      
      df_characteristics <- reactive({
        
        descr_calipseo <- vessel[,c(11:16)]
        
        descr_calipseo$id <- row.names(descr_calipseo)
        
        
        descr_calipseo <- reshape(descr_calipseo, direction = 'long',idvar = 'id', v.names = c('LOA','DRA','GT', 'SPEED','TRAWLING.SPEED','POWER') ,
                                  varying = list(1:6),times = c('LOA','DRA','GT', 'SPEED','TRAWLING.SPEED','POWER'))
        
        descr_calipseo <- data.frame(
          Description = descr_calipseo$time,
          Value = descr_calipseo$LOA
        )
        
        descr_calipseo$Description[descr_calipseo$Description=='LOA'] <- 'Length Overall (m)'
        descr_calipseo$Description[descr_calipseo$Description=='DRA'] <- 'Draught (m)'
        descr_calipseo$Description[descr_calipseo$Description=='GT'] <- 'Gross Tonnage'
        descr_calipseo$Description[descr_calipseo$Description=='SPEED'] <- 'Speed'
        descr_calipseo$Description[descr_calipseo$Description=='TRAWLING.SPEED'] <- 'Trawling Speed'
        descr_calipseo$Description[descr_calipseo$Description=='POWER'] <- 'Power'
        descr_calipseo$Value[is.na(descr_calipseo$Value)] <- '-'
        
        
        extra_calipseo <- data.frame(
          Description = c('Beam (m)','Summer Deadweight (t)'),
          Value = c('-', '-')
        )
        
        df_calipseo <- rbind(descr_calipseo,extra_calipseo)
        
        df_calipseo <- df_calipseo[c(1,2,7,3,8,4,5,6),]
        
        
        names(df_calipseo) <- c('Description','Calipseo')
        df_calipseo$Description[1] <- i18n("LENGTH_OVERALL")
        df_calipseo$Description[2] <- i18n("DRAUGHT")
        df_calipseo$Description[3] <- i18n("BEAM")
        df_calipseo$Description[4] <- i18n("GROSS TONNAGE")
        df_calipseo$Description[5] <- i18n("SUMMER_DEADWEIGHT")
        df_calipseo$Description[6] <- i18n("SPEED")
        df_calipseo$Description[7] <- i18n("TRAWLING_SPEED")
        df_calipseo$Description[8] <- i18n("POWER")
        return(df_calipseo)
        
      })
      
      
      
      output$vessel_characteristics <- renderUI({
        
        df_calipseo_char <- df_characteristics()
        df_vesselfinder_char <- vessel_found[c(3:10),2]
        
        if(!is.null(df_vesselfinder_char)){
          
          INFO("Returning Calipseo and VesselFinder data on vessel characteristics")
          df <- cbind(df_calipseo_char,df_vesselfinder_char)
          names(df) <- c('Description','Calipseo','VesselFinder')
          
          tags$table(class="table table-striped", style="font-size:80% !important;",
                     tags$thead(
                       tags$tr(
                         tags$th(
                           scope='col'
                         ),
                         tags$th(
                           scope='col',i18n("CALIPSEO_DATA_COLNAME")
                         ),
                         tags$th(
                           scope='col',i18n("VESSEL_FINDER_DATA_COLNAME")
                         )
                       )
                     ),
                     tags$tbody(
                       tags$tr(
                         tags$th(
                           scope="row", df$Description[1]
                         ),
                         tags$td(
                           df$Calipseo[1]
                         ),
                         tags$td(
                           df$VesselFinder[1]
                         )
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df$Description[2]
                         ),
                         tags$td(
                           df$Calipseo[2]
                         ),
                         tags$td(
                           df$VesselFinder[2]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df$Description[3]
                         ),
                         tags$td(
                           df$Calipseo[3]
                         ),
                         tags$td(
                           df$VesselFinder[3]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df$Description[4]
                         ),
                         tags$td(
                           df$Calipseo[4]
                         ),
                         tags$td(
                           df$VesselFinder[4]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df$Description[5]
                         ),
                         tags$td(
                           df$Calipseo[5]
                         ),
                         tags$td(
                           df$VesselFinder[5]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df$Description[6]
                         ),
                         tags$td(
                           df$Calipseo[6]
                         ),
                         tags$td(
                           df$VesselFinder[6]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df$Description[7]
                         ),
                         tags$td(
                           df$Calipseo[7]
                         ),
                         tags$td(
                           df$VesselFinder[7]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df$Description[8]
                         ),
                         tags$td(
                           df$Calipseo[8]
                         ),
                         tags$td(
                           df$VesselFinder[8]
                         )
                         
                       )
                     ))
          
          
          
        }else{
          INFO("Returning Calipseo data on vessel characteristics")
          
          tags$table(class="table table-striped", style="font-size:80% !important;",
                     tags$thead(
                       tags$tr(
                         tags$th(
                           scope='col'
                         ),
                         tags$th(
                           scope='col',i18n("CALIPSEO_DATA_COLNAME")
                         )
                       )
                     ),
                     tags$tbody(
                       tags$tr(
                         tags$th(
                           scope="row", df_calipseo_char$Description[1]
                         ),
                         tags$td(
                           df_calipseo_char$Calipseo[1]
                         )
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df_calipseo_char$Description[2]
                         ),
                         tags$td(
                           df_calipseo_char$Calipseo[2]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df_calipseo_char$Description[3]
                         ),
                         tags$td(
                           df_calipseo_char$Calipseo[3]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df_calipseo_char$Description[4]
                         ),
                         tags$td(
                           df_calipseo_char$Calipseo[4]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df_calipseo_char$Description[5]
                         ),
                         tags$td(
                           df_calipseo_char$Calipseo[5]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df_calipseo_char$Description[6]
                         ),
                         tags$td(
                           df_calipseo_char$Calipseo[6]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df_calipseo_char$Description[7]
                         ),
                         tags$td(
                           df_calipseo_char$Calipseo[7]
                         )
                         
                       ),
                       tags$tr(
                         tags$th(
                           scope="row", df_calipseo_char$Description[8]
                         ),
                         tags$td(
                           df_calipseo_char$Calipseo[8]
                         )
                         
                       )
                     ))
          
        }
        
        
        
        
        
        
      })
      
      
      SpeciesCatchesYear <- accessSpeciesCatchesYear(pool,vesselId)
      
      fish_group<-getRemoteReferenceDataset("asfis_enrished")
      fish_group<-subset(fish_group,select=c('3A_Code','ISSCAAP_Group_En'))
      names(fish_group)<-c('species_asfis','ISSCAAP_Group_En')
      
      #linechart
      line_chart_server("catches_sp", SpeciesCatchesYear%>%
                          mutate(text=sprintf("%s-<em>%s</em>(<b>%s</b>)",
                                              species_desc,species_sci,species_asfis)),
                        mode = "plot+table", label = i18n("SPECIES_STATISTIC_LABEL"),
                        colDate = "date",colTarget="species_desc",
                        colValue="quantity",colText="text",
                        rank=TRUE,nbToShow=5,rankLabel=i18n("RANK_LABEL"))
      
      line_chart_server("catches_spgroups", 
                        SpeciesCatchesYear%>%left_join(fish_group, by = "species_asfis"),
                        mode = "plot+table", label = i18n("SPECIES_GROUP_STATISTIC_LABEL"),
                        colDate = "date", colTarget="ISSCAAP_Group_En",
                        colValue="quantity", rank=FALSE)
      
      
      
      
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
      
      
      colRList <- reactive({
        
        if(vessel_indicators_infos$vessel_operational_status=='IN SERVICE / COMMISSION'){
          
          colorlist <- c('green','black','check-circle')
        }else if(vessel_indicators_infos$vessel_operational_status=='UNKNOWN'){
          
          colorlist <- c('lightgray', 'black','') 
        }else if(vessel_indicators_infos$vessel_operational_status=='TOTAL LOSS'){
          
          colorlist <- c('black','white','calendar-times')
        }else if(vessel_indicators_infos$vessel_operational_status=='BROKEN UP'){
          
          colorlist <- c('darkred', 'wheat','crutches')
        }else if(vessel_indicators_infos$vessel_operational_status=='LAID UP'){
          
          colorlist <- c('orange', 'black','anchor')
        }else{
          
          colorlist <- c('purple', 'black','ban')
        }
        return(colorlist)
      })
      
      license_status <- reactive({
        req(license_df())
        if(nrow(license_df()>1)){
          if(license_df()[1,8]=='ok'){
            i18n("LICENSE_STATUS_VALID")
          }else if(license_df()[1,8]=='remove'){
            i18n("LICENSE_STATUS_EXPIRED")
          }
        }else{
          i18n("LICENSE_STATUS_NO_LICENSE")
        }
      })
      
      
      colRList_license_status <- reactive({
        req(license_status())
        
        if(license_status()==i18n("LICENSE_STATUS_VALID")){
          
          colorlist <- c('green','black','check-circle')
          
        }else if(license_status()==i18n("LICENSE_STATUS_EXPIRED")){
          
          colorlist <- c('red','black','times-circle')
          
        }else if(license_status()==i18n("LICENSE_STATUS_NO_LICENSE")){
          
          colorlist <- c('#8b0000','black','times-circle')
          
        }
        
        
      })
      
      output$box_status <- renderUI({
        
        CalipseoInfoBox(span(i18n("INFOBOX_VESSEL_OPERATIONAL_STATUS"),style='font-size:11px;'),icon = icon(colRList()[3]),span(vessel_indicators_infos$vessel_operational_status,style='font-size:15px;'), width = 6, color=colRList()[1], text_color=colRList()[2])
      })
      
      output$box_license_status <- renderUI({
        CalipseoInfoBox(span(i18n("INFOBOX_LICENSE_STATUS"),style='font-size:11px;'),icon = icon(colRList_license_status()[3]),span(toupper(license_status()),style='font-size:15px;'), width = 6, color=colRList_license_status()[1], text_color=colRList_license_status()[2])
      })
      
      output$box_owner <- renderUI({
        CalipseoInfoBox(i18n("INFOBOX_NUMBER_OF_OWNERS"),icon = icon('user'),vessel_indicators_infos$number_of_owners, width = 6)
      })
      
      output$box_license <- renderUI({
        CalipseoInfoBox(i18n("INFOBOX_NUMBER_OF_LICENSES"),icon = icon('ship'),vessel_indicators_infos$number_of_licenses, width = 6)
      })
      
      output$more_indicators <- renderUI({
        fluidRow(
          tags$div(class = "col-sm-1"),
          CalipseoInfoBox(i18n("INFOBOX_MEAN_FISHING_TRIPS_YEAR"),style_title ='font-size:10px;',icon = icon('line-chart'),vessel_indicators_infos$mean_number_of_fishing_trips,width = 2, content_margin_left = '60px',icon_width = '60px'),
          CalipseoInfoBox(i18n("INFOBOX_MEAN_DAYS_AT_SEA_FISHING_TRIPS"),style_title ='font-size:10px;',icon = icon('line-chart'),vessel_indicators_infos$mean_number_of_days_at_sea,width = 2, content_margin_left = '60px',icon_width = '60px'),
          CalipseoInfoBox(i18n("INFOBOX_NUMBER_OF_LANDING_SITES"),style_title ='font-size:10px;',icon = icon('ship'),vessel_indicators_infos$number_of_landing_sites,width = 2, content_margin_left = '60px',icon_width = '60px'),
          CalipseoInfoBox(i18n("INFOBOX_NUMBER_OF_FISHING_GEARS"),style_title ='font-size:10px;',icon = icon('gear'),vessel_indicators_infos$number_of_fishing_gears, width = 2, content_margin_left = '60px',icon_width = '60px'),
          CalipseoInfoBox(i18n("INFOBOX_NUMBER_OF_SPECIES_CAUGHT"),style_title = 'font-size:10px;',icon = icon('fish'),vessel_indicators_infos$number_of_species_fished,width = 2, content_margin_left = '60px',icon_width = '60px'),
          tags$div(class = "col-sm-1")
        )
      })
      
    }
    
  })
  
  
}