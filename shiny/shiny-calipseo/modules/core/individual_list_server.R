#individual_list_server
individual_list_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$individual_list_info <- renderText({
      text <- paste0("<h2>", i18n("INDIVIDUAL_LIST_TITLE")," <small>", i18n("INDIVIDUAL_LIST_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
    ind <- accessIndividualDetails(pool)
    INFO("individual-list server: Fetching individual list data with rows '%s'", nrow(ind))
    
    #individual list
    ind$Salutation <- as.factor(ind$Salutation)
    ind$Gender <- as.factor(ind$Gender)
    
    observe({
      
      if(appConfig[["country_profile"]]$data$CODE=="DM"){
        
        INFO("individual-list server: Country choosen is '%s'",appConfig[["country_profile"]]$data$NAME)
        
        INFO("individual-list server: Indiviadual list filtered by individual roles '%s'", input$filter_individual_roles)
        
        
        ind$Details <- sapply(ind$individualNumber, function(x){
          ind_outhtml <- sprintf("<a href=\"./?page=individual-info&individualNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
          return(ind_outhtml)
        })
        
        
        ind <- ind[,c("Salutation","FIRST_NAME","MIDDLE_NAME","SUFFIX_NAME","NAME","Gender","Details")]
        INFO("individual-list server: Applying the I18n_terms to the individual list columns")
        names(ind) <- c(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_1"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_2"),
                        i18n("INDIVIDUAL_LIST_TABLE_COLNAME_3"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_4"),
                        i18n("INDIVIDUAL_LIST_TABLE_COLNAME_5"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_6"),"")
        
      }else{
        
        INFO("individual-list server: Country choosen is '%s'",appConfig[["country_profile"]]$data$NAME)
        
        ind <- ind[,c("Salutation","FIRST_NAME","MIDDLE_NAME","SUFFIX_NAME","NAME","Gender")]
        
        names(ind) <- c(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_1"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_2"),
                        i18n("INDIVIDUAL_LIST_TABLE_COLNAME_3"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_4"),
                        i18n("INDIVIDUAL_LIST_TABLE_COLNAME_5"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_6"))
      }   
      
      
      NFO("indiviadual-list server: Rendering the individual list data to the table") 
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
      
    })
    
  })
  
}