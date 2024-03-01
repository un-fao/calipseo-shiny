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
    
    #add status
    country_params<-accessCountryParam(pool)
    
    is_vessel_active_query<-subset(country_params,CODE=="ISVESSELACTIVE")$TEXT
    if(length(is_vessel_active_query)>0){
      is_vessel_active_table<-suppressWarnings(dbGetQuery(pool, is_vessel_active_query))
      names(is_vessel_active_table)<-c("ID","Status")
      is_vessel_active_table$Status[is_vessel_active_table$Status==0]<-"inactive"
      is_vessel_active_table$Status[is_vessel_active_table$Status==1]<-"active"
      outp<-merge(outp,is_vessel_active_table)
    }else{
      outp$Status<-NA
    }
    INFO("vessel-list server: Fetching vessel list data after enriching with ISVESSELACTIVE '%s'", nrow(outp))
    
    ls_permits <- accessVesselLicensePermit(pool,registrationNumber = NULL)
    INFO("vessel-list server: Fetching license permits data with rows '%s'", nrow(ls_permits))
    
    
    #TODO add buttons
    #!!!! problematic when no registration number/ empty registration number
    outp$Details <- sapply(outp$REGISTRATION_NUMBER, function(x){ 
      outhtml <- sprintf("<a href=\"./?page=vessel-info&registrationNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
      return(outhtml)
    })
    
    df = NULL
    outp <- outp[,-which(endsWith(colnames(outp), "_CODE"))]
    names(outp) <- gsub("_", " ", names(outp))
    INFO("vessel-list server: Renaming vessel list data columns")
    names(outp)[names(outp)=="REGISTRATION NUMBER"] <- "REGISTRATION_NUMBER"
    names(outp)[names(outp)=="HOME PORT LANDING SITE"] <- "HOME_PORT"
    names(outp)[names(outp)=="REG PORT LANDING SITE"] <- "REG_PORT"
    names(outp)[names(outp)=="VESSEL OPERATIONAL STATUS"] <- "OP_STATUS"
    
    INFO("vessel-list server: Joining vessel list data and license permits data")
    outp <- outp[,c("ID", "REGISTRATION_NUMBER","NAME","Status", "VESSEL TYPE","OP_STATUS","VESSEL STAT TYPE",
                "HOME_PORT","REG_PORT","Details")]
    
    #vessels with no licenses
    not_complete_ls_permits <- ls_permits[is.na(ls_permits$PERMIT_NUMBER),]
    not_complete_ls_permits$Validity <- 'missing license'
    INFO("vessel-list server: Vessels with no permit numbers '%s'", nrow(not_complete_ls_permits))
    
    #process vessels with license
    ls_permits <- ls_permits[!is.na(ls_permits$PERMIT_NUMBER),]

    if(nrow(ls_permits)>0){
      
      js <- js_select2_filter_provider(ns("vessel_list"))
      
      ls_permits <- dplyr::distinct(ls_permits, PERMIT_NUMBER,.keep_all = TRUE)
      INFO("vessel-list server: Vessels with permit numbers '%s'", nrow(ls_permits))
      
      
      INFO("vessel-list server: Computing valid and expired license permits")
      ls_permits <- LicenseValidity(ls_permits)
      INFO("vessel-list server: Vessels with permit numbers after computing valid/expired license permits '%s'", nrow(ls_permits))
      
      #merge vessels with license + no license
      ls_permits <- rbind(ls_permits,not_complete_ls_permits)
      INFO("vessel-list server: all vessels before display '%s'", nrow(ls_permits))
      ls_permits$REGISTRATION_NUMBER = NULL
      
      INFO("vessel-list server: Joining vessels with license permits and those without license permits")
      df <- left_join(outp,ls_permits,by="ID")
      df <- dplyr::distinct(df,ID,.keep_all = TRUE)
      df$Validity[is.na(df$Validity)] <- 'missing license'
      df$HOME_PORT[is.na(df$HOME_PORT)] <- 'unknown'
      df$REG_PORT[is.na(df$REG_PORT)] <- 'unknown'
      
      df <- df[,c("REGISTRATION_NUMBER","NAME","Status", "VESSEL TYPE","OP_STATUS","VESSEL STAT TYPE",
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
      df = outp
      df$Validity <- paste(span("No License",style='color:darkred;font-weight:bold;'))
      
      df <- df[,c("REGISTRATION_NUMBER","NAME","Status", "VESSEL TYPE","OP_STATUS","VESSEL STAT TYPE",
                  "HOME_PORT","REG_PORT","Validity","Details")]
    }
    
    df$Status<-ifelse(df$Status=="active",paste0(icon("anchor-circle-check",style = 'color:green'),span(paste0(" ",i18n("VESSEL_LIST_VESSEL_STATUS_ACTIVE")),style='color:green;')),
                              ifelse(df$Status=="inactive",paste0(icon("anchor-circle-exclamation",style = 'color:orange'),span(paste0(" ", i18n("VESSEL_LIST_VESSEL_STATUS_INACTIVE")),style='color:orange;')),
                                     paste0(icon("ban",style = 'color:gray'),span(paste0(" ",i18n("VESSEL_LIST_VESSEL_STATUS_UNDEFINED")),style='color:gray;'))))
    
    #factorize types for access to codelists
    df[,1:9] <- lapply(df[,1:9],as.factor)
    
    INFO("vessel-list server: Applying the I18n_terms to the vessel list columns")
    names(df) <- c(i18n("VESSEL_LIST_TABLE_COLNAME_REGNUMBER"),i18n("VESSEL_LIST_TABLE_COLNAME_NAME"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_VESSELSTATUS"),i18n("VESSEL_LIST_TABLE_COLNAME_VESSELTYPE"),i18n("VESSEL_LIST_TABLE_COLNAME_OPSTATUS"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_VESSELSTATTYPE"),i18n("VESSEL_LIST_TABLE_COLNAME_HOMEPORT"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_REGPORT"),i18n("VESSEL_LIST_TABLE_COLNAME_LICENSESTATUS"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_DETAILS"))
    
    
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