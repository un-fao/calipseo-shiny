#vessel_list_server
vessel_list_server <- function(input, output, session, pool) {

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
    outhtml <- sprintf("<a href=\"./?page=vessel-info&registrationNumber=%s\" >Details</a>", x)
    return(outhtml)
  })
  
  df <- reactiveValues(data = outp)
  
  
  df <- df$data[,-c(3,4,5,7,9)]
  
  names(df) <- c("REGISTRATION NUMBER", "NAME", "VESSEL TYPE", "VESSEL OPERATIONAL STATUS",
                 "VESSEL STAT TYPE", "HOME PORT/LANDING SITE", "REGISTRATION PORT/LANDING SITE",
                 "DETAILS")
  
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
        list(extend = 'csv', filename =  "vessels", title = NULL, header = TRUE),
        list(extend = 'excel', filename =  "vessels", title = NULL, header = TRUE),
        list(extend = "pdf", title = "List of vessels", header = TRUE, orientation = "landscape")
      ),
      columnDefs = list(list(targets=7,searchable = FALSE)),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      
      pageLength = 5
    )
    )
   
}