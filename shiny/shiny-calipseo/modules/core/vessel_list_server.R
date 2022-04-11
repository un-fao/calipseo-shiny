#vessel_list_server
vessel_list_server <- function(input, output, session, pool) {
  
  output$vessel_list_info <- renderText({
    session$userData$page("vessel-list")
    updatePageUrl("vessel-list", session)
    text <- paste0("<h2>", i18n("VESSEL_LIST_TITLE")," <small>", i18n("VESSEL_LIST_SUBTITLE"),"</small></h2><hr>")
    text
  })
  
  outp <- accessVessels(pool)
  ls_permits <- accessVesselLicensePermit(pool,registrationNumber = NULL)
  
  
  #TODO add buttons
  outp$Details <- sapply(outp$REGISTRATION_NUMBER, function(x){ 
    outhtml <- sprintf("<a href=\"./?page=vessel-info&registrationNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
    return(outhtml)
  })
  
  
  not_complete_ls_permits <- ls_permits[is.na(ls_permits$PERMIT_NUMBER),]
  
  not_complete_ls_permits$Validity <- 'missing license'
  
  ls_permits <- ls_permits[!is.na(ls_permits$PERMIT_NUMBER),]
  
  ls_permits <- dplyr::distinct(ls_permits, PERMIT_NUMBER,.keep_all = TRUE)
  
  
  ls_permits$Valid_to_date <- as.Date(ls_permits$Valid_to_date)
  
  valid_to_date <- ls_permits$Valid_to_date
  
  ls_permits$Validity <- NA
  
  for (i in 1:length(valid_to_date)) {
    validity_status <- Sys.Date()-valid_to_date[i]
    
    if(validity_status<0){
      ls_permits$Validity[i] <- 'valid'
    }else{
      
      ls_permits$Validity[i] <- 'expired'
    }
  }
  
  ls_permits <- rbind(ls_permits,not_complete_ls_permits)
  
  
  df <- reactiveValues(data = outp)
  
  df <- df$data[,-which(endsWith(colnames(df$data), "_CODE"))]
  names(df) <- gsub("_", " ", names(df))
  names(df)[names(df)=="REGISTRATION NUMBER"] <- "REGISTRATION_NUMBER"
  names(df)[names(df)=="HOME PORT LANDING SITE"] <- "HOME_PORT"
  names(df)[names(df)=="REG PORT LANDING SITE"] <- "REG_PORT"
  names(df)[names(df)=="VESSEL OPERATIONAL STATUS"] <- "OP_STATUS"
  
  df <- df[,c(1,2,3,4,5,6,7,16)]
  df <- left_join(df,ls_permits,by="REGISTRATION_NUMBER")
  df <- dplyr::distinct(df,REGISTRATION_NUMBER,.keep_all = TRUE)
  df$Validity[is.na(df$Validity)] <- 'missing license'
  df$HOME_PORT[is.na(df$HOME_PORT)] <- 'unknown'
  df$REG_PORT[is.na(df$REG_PORT)] <- 'unknown'
  
  df <- df[,c(1,2,3,4,5,6,7,15,8)]
  
  
  df$HOME_PORT <- as.character(df$HOME_PORT)
  df$REG_PORT <- as.character(df$REG_PORT)
  df$OP_STATUS <- as.character(df$OP_STATUS)
  df$NAME[tolower(df$NAME)=="name to be completed"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:darkorange;')),span("NAME TO BE COMPLETED",style='color:darkorange;'))
  df$HOME_PORT[tolower(df$HOME_PORT)=="unknown"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:darkorange;')),span("Unknown",style='color:darkorange;'))
  df$REG_PORT[tolower(df$REG_PORT)=="unknown"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:darkorange;')),span("Unknown",style='color:darkorange;'))
  df$OP_STATUS[tolower(df$OP_STATUS)=="unknown"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:darkorange;')),span("Unknown",style='color:darkorange;'))
  df$Validity[tolower(df$Validity)=="valid"] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),span("Valid",style='color:green;'))
  df$Validity[tolower(df$Validity)=="expired"] <- paste(as.character(icon("remove",lib = "glyphicon",style = 'color:red;')),span("Expired",style='color:red;'))
  df$Validity[tolower(df$Validity)=="missing license"] <- paste(as.character(icon("remove",lib = "glyphicon",style = 'color:red;')),span("Missing license",style='color:red;'))
  
  #factorize types for access to codelists
  df[,1:8] <- lapply(df[,1:8],as.factor)
  
  
  names(df) <- c(i18n("VESSEL_LIST_TABLE_COLNAME_1"),i18n("VESSEL_LIST_TABLE_COLNAME_2"),
                 i18n("VESSEL_LIST_TABLE_COLNAME_3"),i18n("VESSEL_LIST_TABLE_COLNAME_4"),
                 i18n("VESSEL_LIST_TABLE_COLNAME_5"),i18n("VESSEL_LIST_TABLE_COLNAME_6"),
                 i18n("VESSEL_LIST_TABLE_COLNAME_7"),i18n("VESSEL_LIST_TABLE_COLNAME_8"),
                 i18n("VESSEL_LIST_TABLE_COLNAME_9"))
  
  output$vessel_list <- renderDataTable(
    df,
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
      
      pageLength = 10
    )
    
  )
  
}