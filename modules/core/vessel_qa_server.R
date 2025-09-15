#vessel_qa_server
vessel_qa_server <- function(id, pool, reloader) {
  
 moduleServer(id, function(input, output, session) {
    
  INFO("vessel-qa: START")
  MODULE_START_TIME = Sys.time()
   
  output$vessel_qa_info <- renderText({
    text <- paste0("<h2>", i18n("VESSEL_QA_TITLE")," <small>", i18n("VESSEL_QA_SUBTITLE"),"</small></h2><hr>")
    text
  })
  
  #vessel_qa_data
  vessel_qa_names_df <- accessVesselQaNames(pool)
  vessel_qa_ports_df <- accessVesselQaPorts(pool)
  vessel_qa_characteristics_df <- accessVesselQaCharacteristics(pool)
  vessel_qa_license_permits <- accessVesselLicensePermit(pool,registrationNumber = NULL)
  vessel_qa_vop_status <- data.frame(VOP_STATUS = accessVessels(pool)$VESSEL_OPERATIONAL_STATUS)
  ftpv_activity_count <- dplyr::distinct(accessVesselsWithFishingTrips(pool, year = format(Sys.Date(), "%Y")), REGISTRATION_NUMBER, .keep_all = TRUE)
  ftpv_GrandTotal <- nrow(dplyr::distinct(accessVesselsWithFishingTrips(pool, year = NULL), REGISTRATION_NUMBER, .keep_all = TRUE))
  GrandTotal_vessels <- as.numeric(countVessels(pool))
  ftpv_GrandTotal_inactive <- GrandTotal_vessels - ftpv_GrandTotal
  
  hist_ftpv <- data.frame(Validity = c('active_hist', 'inactive_hist'),
                          Count = c(ftpv_GrandTotal, ftpv_GrandTotal_inactive))
  
  
  if(nrow(ftpv_activity_count)>0){
    ftpv_activity_count$DATE_TO <- as.Date(ftpv_activity_count$DATE_TO)
    ftpv_activity_count$Validity <- sapply(ftpv_activity_count$DATE_TO, function(x){
      ifelse(Sys.Date()-x < 0, 'active', 'inactive')
    })
    ftpv_activity_count <- count(ftpv_activity_count, Validity, name = 'Count')
    if(nrow(ftpv_activity_count)<2){
      if(ftpv_activity_count$Validity == "active"){
        #only active data, we complete with the inactive ones
        count_inactive <- data.frame(Validity = 'inactive', Count = GrandTotal_vessels - ftpv_activity_count$Count)
        ftpv_activity_count <- rbind(ftpv_activity_count, count_inactive)
      }else{
        #only inactive data, we complete with the active ones
        count_active <- data.frame(Validity = 'active', Count = GrandTotal_vessels-ftpv_activity_count$Count)
        ftpv_activity_count <- rbind(ftpv_activity_count, count_active)
      }
    }
  }else{
    #no data for the current year
    ftpv_activity_count <- data.frame(
      Validity = c('active', 'inactive'),
      Count = c(0, GrandTotal_vessels)
    )
  }
  #merge historical data (all years) + current year
  ftpv_activity_count <- rbind(hist_ftpv, ftpv_activity_count)
  ftpv_activity_count <- data.frame(
    Status = c('In_activity', 'No_activity'),
    History = c(ftpv_activity_count$Count[1],(ftpv_activity_count$Count[2])),
    Current = c(ftpv_activity_count$Count[3],(ftpv_activity_count$Count[4]))
  )
  
  #UI handling for activities
  ftpv_activity_count$Status[ftpv_activity_count$Status=='In_activity'] <- paste(as.character(icon("ok",lib = "glyphicon", style = 'color:green;')),i18n("VESSEL_QA_STATUS_INACTIVITY"))
  ftpv_activity_count$Status[ftpv_activity_count$Status=='No_activity'] <- paste(as.character(icon("alert",lib = "glyphicon", style = 'color:red;')),i18n("VESSEL_QA_STATUS_NOACTIVITY"))
  names(ftpv_activity_count) <- c(i18n("VESSEL_QA_TABLE_FISHINGTRIPS_COLNAME_1"),i18n("VESSEL_QA_TABLE_FISHINGTRIPS_COLNAME_2"),i18n("VESSEL_QA_TABLE_FISHINGTRIPS_COLNAME_3")) 
  
  #process + UI handling for operational status
  vessel_qa_vop_status$VOP_STATUS <- as.factor(vessel_qa_vop_status$VOP_STATUS)
  vessel_qa_vop_status <- count(vessel_qa_vop_status,VOP_STATUS,name = 'Count')
  vessel_qa_vop_status$VOP_STATUS <- as.character( vessel_qa_vop_status$VOP_STATUS)
  vessel_qa_vop_status$VOP_STATUS[tolower(vessel_qa_vop_status$VOP_STATUS)=="unknown"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:darkorange;')),span("UNKNOWN",style='color:darkorange;'))
  names(vessel_qa_vop_status) <- c(i18n("VESSEL_QA_TABLE_VOP_STATUS_COLNAME_1"),i18n("VESSEL_QA_TABLE_VOP_STATUS_COLNAME_2"))
  
  
  #function to handle vessel QA tables
  vessel_qa <- function(data){
    
    if(ncol(data)==2){
      data$ACTION[data$ACTION=='ok'] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),i18n("VESSEL_QA_STATUS_VALID"))
      data$ACTION[data$ACTION=='to_check'] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')),i18n("VESSEL_QA_STATUS_TOCHECK"))
      names(data) <- c(i18n("VESSEL_QA_TABLE_COLNAME_1"),i18n("VESSEL_QA_TABLE_COLNAME_2"))
      
    }else{
      data <- reshape(data, direction = 'wide', timevar = 'CHARA',idvar = 'ACTION')
      names(data) <- gsub("COUNT.", "", names(data), fixed = TRUE)
      
      if(ncol(data)==3){
        data$ACTION[data$ACTION=='ok'] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),i18n("VESSEL_QA_STATUS_VALID"))
        data$ACTION[data$ACTION=='to_check'] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')),i18n("VESSEL_QA_STATUS_TOCHECK"))
        names(data) <- c(i18n("VESSEL_QA_TABLE_PORT_COLNAME_1"),i18n("VESSEL_QA_TABLE_PORT_COLNAME_2"),i18n("VESSEL_QA_TABLE_PORT_COLNAME_3")) 
      
      }else if(ncol(data)>4){
        data$ACTION[data$ACTION=='ok'] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),i18n("VESSEL_QA_STATUS_CHAR_VALID"))
        data$ACTION[data$ACTION=='to_check'] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')),i18n("VESSEL_QA_STATUS_CHAR_TOCHECK"))
        names(data) <- c(i18n("VESSEL_QA_TABLE_CHAR_COLNAME_1"),i18n("VESSEL_QA_TABLE_CHAR_COLNAME_2"),
                         i18n("VESSEL_QA_TABLE_CHAR_COLNAME_3"),i18n("VESSEL_QA_TABLE_CHAR_COLNAME_4"),
                         i18n("VESSEL_QA_TABLE_CHAR_COLNAME_5"),i18n("VESSEL_QA_TABLE_CHAR_COLNAME_6"))
      }
    }
    
    out <- DT::datatable(data,
                         escape = FALSE,
                         rownames = FALSE,
                         options = list(
                           searching = FALSE,
                           dom = 't',
                           language = list(url = i18n("TABLE_LANGUAGE"))
                         ))
    
    return(out)
    
  }
  
  
  #vessel_qa_license_counts
  valid_categories <- vessel_qa_license_permits[!is.na(vessel_qa_license_permits$PERMIT_NUMBER),]
  invalid_category <- vessel_qa_license_permits[is.na(vessel_qa_license_permits$PERMIT_NUMBER),]
  invalid_category$Validity <- 'notcompleted'
  invalid_category <- dplyr::distinct(invalid_category, REGISTRATION_NUMBER,.keep_all = TRUE)
  invalid_category <- count(invalid_category,Validity,name = 'Count')
  valid_categories <- dplyr::distinct(valid_categories, PERMIT_NUMBER,.keep_all = TRUE)
  
  if(nrow(valid_categories)>0){
    
    valid_categories$Valid_to_date <- as.Date(valid_categories$Valid_to_date)
    
    valid_categories$Validity <- sapply(valid_categories$Valid_to_date, function(x){
      ifelse(Sys.Date() < x, 'ok', 'expired')
    })
  
    valid_dates <- valid_categories[valid_categories['Validity']=='ok',]
    invalid_dates <- valid_categories[valid_categories['Validity']=='expired',]
    valid_dates <- dplyr::distinct(valid_dates, REGISTRATION_NUMBER,.keep_all = TRUE)
    invalid_dates <- dplyr::distinct(invalid_dates, REGISTRATION_NUMBER,.keep_all = TRUE)
    valid_categories <- rbind(valid_dates,invalid_dates)
    valid_categories <- dplyr::distinct(valid_categories,REGISTRATION_NUMBER,.keep_all = TRUE)
    valid_categories <- count(valid_categories,Validity,name = 'Count')
    
    license_df <- rbind(valid_categories,invalid_category)
    
    license_df$Validity[license_df$Validity=='ok'] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),i18n("VESSEL_QA_LICENSE_STATUS_VALID"))
    license_df$Validity[license_df$Validity=='notcompleted'] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')),i18n("VESSEL_QA_LICENSE_STATUS_NOTCOMPLETED"))
    license_df$Validity[license_df$Validity=='expired'] <- paste(as.character(icon("remove",lib = "glyphicon",style = 'color:red;')),i18n("VESSEL_QA_LICENSE_STATUS_EXPIRED"))
  
  }else{
    license_df <- data.frame(License_status=character(0),
                            Count=character(0))
  }
  
  names(license_df) <- c(i18n("VESSEL_QA_TABLE_LICENSE_COLNAME_1"),i18n("VESSEL_QA_TABLE_LICENSE_COLNAME_2"))
  
  #DATA TABL RENDERS
  
  #vessel_qa_names
  output$vessel_names <- renderDataTable({
    vessel_qa(vessel_qa_names_df)
  }) 
  
  #vessel_qa_ports
  output$vessel_ports <- renderDataTable({
    vessel_qa(vessel_qa_ports_df)
  }) 
  
  #vessel_qa_characteristics
  output$vessel_characteristics <- renderDataTable({
    vessel_qa(vessel_qa_characteristics_df)
  })
  
  #vessel_qa_license
  output$vessel_license <- renderDataTable({
    DT::datatable(license_df,
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    searching = FALSE,
                    dom = 't',
                    language = list(url = i18n("TABLE_LANGUAGE"))
                  ))
  })
  
  #vessel_qa_operational_status
  output$vessel_operational_status <- renderDataTable({
    DT::datatable(vessel_qa_vop_status,
                  escape = FALSE,
                  rownames = FALSE,
                  
                  options = list(
                    searching = FALSE,
                    dom = 't',
                    language = list(url = i18n("TABLE_LANGUAGE"))
                  ))
  })
  
  
  #vessel_qa_fishing_activity_count
  output$vessel_activity <- renderDataTable({
    DT::datatable(ftpv_activity_count,
                  escape = FALSE,
                  rownames = FALSE,
                  
                  options = list(
                    searching = FALSE,
                    dom = 't',
                    language = list(url = i18n("TABLE_LANGUAGE"))
                  ))
  })
  
  MODULE_END_TIME <- Sys.time()
  MODULE_TIME <- MODULE_END_TIME - MODULE_START_TIME
  INFO("vessel-qa: END")
  DEBUG_MODULE_PROCESSING_TIME("Vessel-qa", MODULE_START_TIME, MODULE_END_TIME)
 
 }) 
  
}