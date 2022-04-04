#individual_list_server
individual_list_server <- function(input, output, session, pool) {
  
  output$individual_list_info <- renderText({
    session$userData$page("individual-list")
    updatePageUrl("individual-list", session)
    text <- paste0("<h2>", i18n("INDIVIDUAL_LIST_TITLE")," <small>", i18n("INDIVIDUAL_LIST_SUBTITLE"),"</small></h2><hr>")
    text
  })
  
  #individual list
  ind <- accessIndividualDetails(pool)
  ind$Salutation <- as.factor(ind$Salutation)
  ind$Gender <- as.factor(ind$Gender)
 
  names(ind) <- c(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_1"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_2"),
                 i18n("INDIVIDUAL_LIST_TABLE_COLNAME_3"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_4"),
                 i18n("INDIVIDUAL_LIST_TABLE_COLNAME_5"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_6"))
  
  
  output$individual_list <- renderDataTable(
    ind,
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
        list(extend = 'csv', filename = i18n("INDIVIDUALS") , title = NULL, header = TRUE),
        list(extend = 'excel', filename =  i18n("INDIVIDUALS"), title = NULL, header = TRUE),
        list(extend = "pdf", title = i18n("INDIVIDUAL_LIST_TITLE"), header = TRUE, orientation = "landscape")
      ),
      exportOptions = list(
        modifiers = list(page = "all", selected = TRUE)
      ),
      language = list(url = i18n("TABLE_LANGUAGE")),
      
      pageLength = 10
    )
  )
  
}