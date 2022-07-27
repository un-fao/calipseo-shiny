#vessel_list_server
vessel_list_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {  
    
    ns <- session$ns
    
    output$vessel_list_info <- renderText({
      text <- paste0("<h2>", i18n("VESSEL_LIST_TITLE")," <small>", i18n("VESSEL_LIST_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
    outp <- accessVessels(pool)
    INFO("vessel-list server: Fetching vessel list data with rows '%s'", nrow(outp))
    
    ls_permits <- accessVesselLicensePermit(pool,registrationNumber = NULL)
    INFO("vessel-list server: Fetching license permits data with rows '%s'", nrow(ls_permits))
    
    
    #TODO add buttons
    outp$Details <- sapply(outp$REGISTRATION_NUMBER, function(x){ 
      outhtml <- sprintf("<a href=\"./?page=vessel-info&registrationNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
      return(outhtml)
    })
    
    
    not_complete_ls_permits <- ls_permits[is.na(ls_permits$PERMIT_NUMBER),]
    INFO("vessel-list server: Vessels with no permit numbers '%s'", nrow(not_complete_ls_permits))
    
    not_complete_ls_permits$Validity <- 'missing license'
    
    ls_permits <- ls_permits[!is.na(ls_permits$PERMIT_NUMBER),]
    
    
    df <- reactiveValues(data = outp)
    
    df <- df$data[,-which(endsWith(colnames(df$data), "_CODE"))]
    names(df) <- gsub("_", " ", names(df))
    INFO("vessel-list server: Renaming vessel list data columns")
    names(df)[names(df)=="REGISTRATION NUMBER"] <- "REGISTRATION_NUMBER"
    names(df)[names(df)=="HOME PORT LANDING SITE"] <- "HOME_PORT"
    names(df)[names(df)=="REG PORT LANDING SITE"] <- "REG_PORT"
    names(df)[names(df)=="VESSEL OPERATIONAL STATUS"] <- "OP_STATUS"
    
    INFO("vessel-list server: Joining vessle list data and license permits data")
    
    df <- df[,c("REGISTRATION_NUMBER","NAME","VESSEL TYPE","OP_STATUS","VESSEL STAT TYPE",
                "HOME_PORT","REG_PORT","Details")]
    
    
    if(nrow(ls_permits)>0){
      
      js <- js_select2_filter_provider(ns("vessel_list"))
      
      ls_permits <- dplyr::distinct(ls_permits, PERMIT_NUMBER,.keep_all = TRUE)
      INFO("vessel-list server: Vessels with permit numbers '%s'", nrow(ls_permits))
      
      
      INFO("vessel-list server: Computing valid and expired license permits")
      ls_permits <- LicenseValidity(ls_permits)
      
      valid_dates <- ls_permits[ls_permits['Validity']=='valid',]
      invalid_dates <- ls_permits[ls_permits['REGISTRATION_NUMBER'] != valid_dates$REGISTRATION_NUMBER,]
      ls_permits <- rbind(valid_dates,invalid_dates)
      ls_permits <- rbind(ls_permits,not_complete_ls_permits)
      
      INFO("vessel-list server: Joining vessles with license permits and those without license permits")
      df <- left_join(df,ls_permits,by="REGISTRATION_NUMBER")
      df <- dplyr::distinct(df,REGISTRATION_NUMBER,.keep_all = TRUE)
      df$Validity[is.na(df$Validity)] <- 'missing license'
      df$HOME_PORT[is.na(df$HOME_PORT)] <- 'unknown'
      df$REG_PORT[is.na(df$REG_PORT)] <- 'unknown'
      
      df <- df[,c("REGISTRATION_NUMBER","NAME","VESSEL TYPE","OP_STATUS","VESSEL STAT TYPE",
                  "HOME_PORT","REG_PORT","Validity","Details")]
      
      INFO("vessel-list server: Applying font colour and icon to the desired columns")
      df$HOME_PORT <- as.character(df$HOME_PORT)
      df$REG_PORT <- as.character(df$REG_PORT)
      df$OP_STATUS <- as.character(df$OP_STATUS)
      df$NAME[tolower(df$NAME)=="name to be completed"] <- paste(gicon("alert", style='color:darkorange;'),span("NAME TO BE COMPLETED",style='color:darkorange;'))
      df$HOME_PORT[tolower(df$HOME_PORT)=="unknown"] <- paste(gicon("alert", style = 'color:darkorange;'),span("Unknown",style='color:darkorange;'))
      df$REG_PORT[tolower(df$REG_PORT)=="unknown"] <- paste(gicon("alert", style = 'color:darkorange;'),span("Unknown",style='color:darkorange;'))
      df$OP_STATUS[tolower(df$OP_STATUS)=="unknown"] <- paste(gicon("alert",style = 'color:darkorange;'),span("Unknown",style='color:darkorange;'))
      df$Validity[tolower(df$Validity)=="valid"] <- paste(gicon("ok",style = 'color:green;'),span("Valid",style='color:green;'))
      df$Validity[tolower(df$Validity)=="expired"] <- paste(gicon("remove",style = 'color:red;'),span("Expired",style='color:red;'))
      df$Validity[tolower(df$Validity)=="missing license"] <- paste(gicon("remove",style = 'color:red;'),span("Missing license",style='color:red;'))
      
    }else{
      
      df$Validity <- 'No License'
      
      df <- df[,c("REGISTRATION_NUMBER","NAME","VESSEL TYPE","OP_STATUS","VESSEL STAT TYPE",
                  "HOME_PORT","REG_PORT","Validity","Details")]
    }
    
    #factorize types for access to codelists
    df[,1:8] <- lapply(df[,1:8],as.factor)
    
    INFO("vessel-list server: Applying the I18n_terms to the vessel list columns")
    names(df) <- c(i18n("VESSEL_LIST_TABLE_COLNAME_1"),i18n("VESSEL_LIST_TABLE_COLNAME_2"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_3"),i18n("VESSEL_LIST_TABLE_COLNAME_4"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_5"),i18n("VESSEL_LIST_TABLE_COLNAME_6"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_7"),i18n("VESSEL_LIST_TABLE_COLNAME_8"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_9"))
    
    
    INFO("vessel-list server: Rendering the vessel list data to the table")
    output$vessel_list <- DT::renderDT(
      df,
      container = initDTContainer(df),
      escape = FALSE,
      rownames = FALSE,
      server = FALSE,
      extensions = c("Buttons"),
      filter = list(position = 'top', clear = FALSE),
      options = list(
        autoWidth = FALSE,
        dom = 'Bfrtip',
        deferRender = TRUE,
        scroll = FALSE,
        buttons = list(
          list(extend = 'copy'),
          list(extend = 'csv', filename = i18n("VESSELS") , title = NULL, header = TRUE),
          list(extend = 'excel', filename =  i18n("VESSELS"), title = NULL, header = TRUE),
          list(extend = "pdf", title = i18n("VESSEL_LIST_TITLE"), header = TRUE, orientation = "landscape")
        ),
        columnDefs = list(
          list(targets=8,searchable = FALSE, sortable = FALSE)
        ),
        exportOptions = list(
          modifiers = list(page = "all", selected = TRUE)
        ),
        language = list(url = i18n("TABLE_LANGUAGE")),
        pageLength = 10,
        initComplete = js
      )
      
    )
  }) 
}