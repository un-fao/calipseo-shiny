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
    
    
    observe({
      if(appConfig[["country_profile"]]$data$CODE=="DM"){
        
        observeEvent(input$fisher_chck,{if(isTRUE(input$fisher_chck))updateCheckboxInput(session,"nonfisher_chck",value = FALSE)})
        
        fisher_nonfisher_df <- fisher_nonfisher(ind)
        
        
        
        observeEvent({
          
          input$nonfisher_chck
          input$filter_fisher_category_list
          
        },
        {
          if(isFALSE(input$nonfisher_chck)){
            updateCheckboxInput(session,"fisher_chck",value = TRUE)
            category <- i18n("INDIVIDUAL_BREAKDOWN_LABEL_FISHER")
          }else {
            updateCheckboxInput(session,"fisher_chck",value = FALSE)
            category <- i18n("INDIVIDUAL_BREAKDOWN_LABEL_NONFISHER")
            
          }
          
          
          if(category==i18n("INDIVIDUAL_BREAKDOWN_LABEL_FISHER")){
            
            ind <- fisher_nonfisher_df %>% dplyr::filter(Category%in%input$filter_fisher_category_list)
            
            ind$individualNumber <- as.factor(ind$individualNumber)
            ind <- distinct(ind, individualNumber, .keep_all = TRUE)
            
            INFO("individual-list server: Country choosen is '%s'",appConfig[["country_profile"]]$data$NAME)
            
            INFO("individual-list server: Indiviadual list filtered by individual roles '%s'", input$filter_fisher_category_list)
            
            ind$Details <- sapply(ind$individualNumber, function(x){
              ind_outhtml <- sprintf("<a href=\"./?page=individual-info&individualNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
              return(ind_outhtml)
            })
            
            
            
            ind <- ind[,c("Salutation","FIRST_NAME","MIDDLE_NAME","SUFFIX_NAME","NAME","Gender","Details")]
            INFO("individual-list server: Applying the I18n_terms to the individual list columns")
            names(ind) <- c(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_1"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_2"),
                            i18n("INDIVIDUAL_LIST_TABLE_COLNAME_3"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_4"),
                            i18n("INDIVIDUAL_LIST_TABLE_COLNAME_5"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_6"),"")
            
            
            output$individual_list <- renderDT(
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
            
            
          }else{
            ind <- fisher_nonfisher_df %>% dplyr::filter(Category%in%'nonfisher')
            
            ind$individualNumber <- as.factor(ind$individualNumber)
            ind <- distinct(ind, individualNumber, .keep_all = TRUE)
            
            
            ind$Details <- sapply(ind$individualNumber, function(x){
              ind_outhtml <- sprintf("<a href=\"./?page=individual-info&individualNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
              return(ind_outhtml)
            })
            
            
            
            ind <- ind[,c("Salutation","FIRST_NAME","MIDDLE_NAME","SUFFIX_NAME","NAME","Gender","Details")]
            INFO("individual-list server: Applying the I18n_terms to the individual list columns")
            names(ind) <- c(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_1"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_2"),
                            i18n("INDIVIDUAL_LIST_TABLE_COLNAME_3"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_4"),
                            i18n("INDIVIDUAL_LIST_TABLE_COLNAME_5"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_6"),"")
            
            
            output$individual_list <- renderDT(
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
          
        })
        
        
      }else{
        
        ind$individualNumber <- as.factor(ind$individualNumber)
        ind <- distinct(ind, individualNumber, .keep_all = TRUE)
        
        INFO("individual-list server: Country choosen is '%s'",appConfig[["country_profile"]]$data$NAME)
        
        ind <- ind[,c("Salutation","FIRST_NAME","MIDDLE_NAME","SUFFIX_NAME","NAME","Gender")]
        
        INFO("individual-list server: Applying the I18n_terms to the individual list columns")
        names(ind) <- c(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_1"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_2"),
                        i18n("INDIVIDUAL_LIST_TABLE_COLNAME_3"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_4"),
                        i18n("INDIVIDUAL_LIST_TABLE_COLNAME_5"),i18n("INDIVIDUAL_LIST_TABLE_COLNAME_6"))
      }
      
      INFO("indiviadual-list server: Rendering the individual list data to the table") 
      
      output$individual_list <- renderDT(
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