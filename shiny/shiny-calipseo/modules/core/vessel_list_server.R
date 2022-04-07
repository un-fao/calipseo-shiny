#vessel_list_server
vessel_list_server <- function(input, output, session, pool) {

  output$vessel_list_info <- renderText({
    session$userData$page("vessel-list")
    updatePageUrl("vessel-list", session)
    text <- paste0("<h2>", i18n("VESSEL_LIST_TITLE")," <small>", i18n("VESSEL_LIST_SUBTITLE"),"</small></h2><hr>")
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
    outhtml <- sprintf("<a href=\"./?page=vessel-info&registrationNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
    return(outhtml)
  })
  
  df <- reactiveValues(data = outp)

  df <- df$data[,-which(endsWith(colnames(df$data), "_CODE"))]
  names(df) <- gsub("_", " ", names(df))
  names(df)[names(df)=="HOME PORT LANDING SITE"] <- "HOME_PORT"
  names(df)[names(df)=="REG PORT LANDING SITE"] <- "REG_PORT"
  names(df)[names(df)=="VESSEL OPERATIONAL STATUS"] <- "OP_STATUS"
  names(df)[names(df)=="Details"] <- "" 
  df <- df[,c(1,2,3,4,5,6,7,16)]
  
  
  df$HOME_PORT <- as.character(df$HOME_PORT)
  df$REG_PORT <- as.character(df$REG_PORT)
  df$OP_STATUS <- as.character(df$OP_STATUS)
  df$NAME[tolower(df$NAME)=="name to be completed"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:darkorange;')),span("NAME TO BE COMPLETED",style='color:darkorange;'))
  df$HOME_PORT[tolower(df$HOME_PORT)=="unknown"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:darkorange;')),span("Unknown",style='color:darkorange;'))
  df$REG_PORT[tolower(df$REG_PORT)=="unknown"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:darkorange;')),span("Unknown",style='color:darkorange;'))
  df$OP_STATUS[tolower(df$OP_STATUS)=="unknown"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:darkorange;')),span("Unknown",style='color:darkorange;'))
  names(df) <- c(i18n("VESSEL_LIST_TABLE_COLNAME_1"),i18n("VESSEL_LIST_TABLE_COLNAME_2"),
                 i18n("VESSEL_LIST_TABLE_COLNAME_3"),i18n("VESSEL_LIST_TABLE_COLNAME_4"),
                 i18n("VESSEL_LIST_TABLE_COLNAME_5"),i18n("VESSEL_LIST_TABLE_COLNAME_6"),
                 i18n("VESSEL_LIST_TABLE_COLNAME_7"),i18n("VESSEL_LIST_TABLE_COLNAME_8"))
  
  output$vessel_list <- renderDataTable(
    df,
    server = FALSE,
    escape = FALSE,
    rownames = FALSE,
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
        list(targets=7,searchable = FALSE, sortable = FALSE)
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      language = list(url = i18n("TABLE_LANGUAGE")),
      
      pageLength = 10
    )
    )
   
}