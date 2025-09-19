#vessel_list_server
vessel_list_server <- function(id, parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session) {  
    
    INFO("vessel-list: START")
    MODULE_START_TIME = Sys.time()
    
    ns <- session$ns
    
    vessel_selection <- reactiveVal(NULL)
    
    output$vessel_list_info <- renderText({
      text <- paste0("<h2>", i18n("VESSEL_LIST_TITLE")," <small>", i18n("VESSEL_LIST_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
    outp <- accessVessels(pool)
    INFO("vessel-list: Fetching vessel list data with - %s rows", nrow(outp))
    
    #add status
    is_vessel_active_query <- subset(COUNTRY_PARAMS, CODE == "ISVESSELACTIVE")$TEXT
    if(length(is_vessel_active_query)>0){
      DEBUG("Query is_vessel_active query: %s", is_vessel_active_query)
      is_vessel_active_table <- getFromSQL(pool, is_vessel_active_query)
      names(is_vessel_active_table) <- c("ID","Status")
      is_vessel_active_table$Status[is_vessel_active_table$Status == 0] <- "inactive"
      is_vessel_active_table$Status[is_vessel_active_table$Status == 1] <- "active"
      outp <- merge(outp, is_vessel_active_table)
    }else{
      outp$Status <- NA
    }
    INFO("vessel-list: Fetching vessel list data after enriching with ISVESSELACTIVE - %s rows", nrow(outp))
    
    outp$Details <- sprintf(
      '<a href="#" onclick="Shiny.setInputValue(\'%s\', %d, {priority: \'event\'});">%s</a>',
      ns("access_vessel_info"), outp$ID, i18n("VESSEL_LIST_TABLE_RECORD_ACCESS")
    )
    observeEvent(input$access_vessel_info, {
      DEBUG("Click to access details for vessel '%s'", input$access_vessel_info)
      vessel_selection(input$access_vessel_info)
    })
    
    df = NULL
    outp <- outp[,-which(endsWith(colnames(outp), "_CODE"))]
    names(outp) <- gsub("_", " ", names(outp))
    INFO("vessel-list: Renaming vessel list data columns")
    names(outp)[names(outp)=="REGISTRATION NUMBER"] <- "REGISTRATION_NUMBER"
    names(outp)[names(outp)=="HOME PORT LANDING SITE"] <- "HOME_PORT"
    names(outp)[names(outp)=="REG PORT LANDING SITE"] <- "REG_PORT"
    names(outp)[names(outp)=="VESSEL OPERATIONAL STATUS"] <- "OP_STATUS"
    
    INFO("vessel-list: Joining vessel list data and license permits data")
    outp <- outp[,c("ID", "REGISTRATION_NUMBER","NAME","Status", 
                    "VESSEL TYPE","OP_STATUS","VESSEL STAT TYPE",
                    "HOME_PORT","REG_PORT","Details")]
    
    #Adding license information
    ls_permits <- accessVesselLicensePermits(pool,id = NULL)
    INFO("vessel-list: Fetching license permits data with rows '%s'", nrow(ls_permits))
    
    #vessels with no licenses
    not_complete_ls_permits <- ls_permits[is.na(ls_permits$PERMIT_NUMBER),]
    not_complete_ls_permits$Validity <- 'missing license'
    INFO("vessel-list: Vessels with no license permit numbers - %s rows", nrow(not_complete_ls_permits))
    
    #vessels with license
    ls_permits <- ls_permits[!is.na(ls_permits$PERMIT_NUMBER),]

    if(nrow(ls_permits)>0){
      js <- js_select2_filter_provider(ns("vessel_list"))
      
      ls_permits <- dplyr::distinct(ls_permits, PERMIT_NUMBER, .keep_all = TRUE)
      INFO("vessel-list: Vessels with permit numbers - %s rows", nrow(ls_permits))
      
      INFO("vessel-list: Computing valid and expired license permits")
      ls_permits <- get_vessel_license_status(ls_permits)
      INFO("vessel-list: Vessels with permit numbers after computing valid/expired license permits - %s rows", nrow(ls_permits))
      
      #merge vessels with license + no license
      ls_permits <- rbind(ls_permits, not_complete_ls_permits)
      INFO("vessel-list: All vessels before display - %s rows", nrow(ls_permits))
      ls_permits$REGISTRATION_NUMBER = NULL
      
      INFO("vessel-list: Joining vessels with license permits and those without license permits")
      df <- left_join(outp, ls_permits, by="ID")
      df <- dplyr::distinct(df, ID, .keep_all = TRUE)
      df$Validity[is.na(df$Validity)] <- 'missing license'
      df$HOME_PORT[is.na(df$HOME_PORT)] <- 'unknown'
      df$REG_PORT[is.na(df$REG_PORT)] <- 'unknown'
      
      df <- df[,c("REGISTRATION_NUMBER","NAME","Status", 
                  "VESSEL TYPE","OP_STATUS","VESSEL STAT TYPE",
                  "HOME_PORT","REG_PORT","Validity","Details")]
      
      df$HOME_PORT <- as.character(df$HOME_PORT)
      df$REG_PORT <- as.character(df$REG_PORT)
      df$OP_STATUS <- as.character(df$OP_STATUS)
      df$NAME[tolower(df$NAME)=="name to be completed"] <- paste(icon("warning", style='color:darkorange;'),span("NAME TO BE COMPLETED",style='color:darkorange;'))
      df$HOME_PORT[tolower(df$HOME_PORT)=="unknown"] <- paste(icon("warning", style = 'color:darkorange;'),span("Unknown",style='color:darkorange;'))
      df$REG_PORT[tolower(df$REG_PORT)=="unknown"] <- paste(icon("warning", style = 'color:darkorange;'),span("Unknown",style='color:darkorange;'))
      df$OP_STATUS[tolower(df$OP_STATUS)=="unknown"] <- paste(icon("warning",style = 'color:darkorange;'),span("Unknown",style='color:darkorange;'))
      df$Validity[tolower(df$Validity)=="valid"] <- paste(icon("check",style = 'color:green;'),span("Valid",style='color:green;'))
      df$Validity[tolower(df$Validity)=="expired"] <- paste(icon("remove",style = 'color:red;'),span("Expired",style='color:red;'))
      df$Validity[tolower(df$Validity)=="missing license"] <- paste(icon("remove",style = 'color:red;'),span("Missing license",style='color:red;'))
      
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
    max_factor_index = 9
    df[,1:max_factor_index] <- lapply(df[,1:max_factor_index],as.factor)
    
    names(df) <- c(i18n("VESSEL_LIST_TABLE_COLNAME_REGNUMBER"),i18n("VESSEL_LIST_TABLE_COLNAME_NAME"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_VESSELSTATUS"),i18n("VESSEL_LIST_TABLE_COLNAME_VESSELTYPE"),i18n("VESSEL_LIST_TABLE_COLNAME_OPSTATUS"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_VESSELSTATTYPE"),i18n("VESSEL_LIST_TABLE_COLNAME_HOMEPORT"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_REGPORT"),i18n("VESSEL_LIST_TABLE_COLNAME_LICENSESTATUS"),
                   i18n("VESSEL_LIST_TABLE_COLNAME_DETAILS"))
    
    
    INFO("vessel-list: Rendering the vessel list data to the table")
    output$vessel_list <- DT::renderDT(
      df,
      selection = 'none',
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
          list(targets=max_factor_index,searchable = FALSE, sortable = FALSE)
        ),
        exportOptions = list(
          modifiers = list(page = "all", selected = TRUE)
        ),
        language = list(url = i18n("TABLE_LANGUAGE")),
        pageLength = 10,
        initComplete = js
      )
      
    )
    
    #Control the selection of the UI (vessel-list or vessel-info with the use of
    #the parent.session$userData$record_selection)
    observeEvent(vessel_selection(),{
      if(is.null(vessel_selection())){
        DEBUG("Selection is NULL, we display the vessel table")
        parent.session$userData$record_selection = NULL
        isolate({ updateTabItems(parent.session, "calipseo-tabs", "vessel_list") })
      }else{
        DEBUG("Selection is not NULL, we display the vessel record")
        parent.session$userData$record_selection = vessel_selection()
        isolate({ updateTabItems(parent.session, "calipseo-tabs", "vessel_info") })
      }
    })
    
    MODULE_END_TIME <- Sys.time()
    MODULE_TIME <- MODULE_END_TIME - MODULE_START_TIME
    INFO("vessel-list: END")
    DEBUG_MODULE_PROCESSING_TIME("Vessel-list", MODULE_START_TIME, MODULE_END_TIME)
    
    
  }) 
}