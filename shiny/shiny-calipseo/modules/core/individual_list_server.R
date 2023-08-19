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
    
    
    # ind_info <- accessIndividualInfo(pool)
    # ind_info <- ind_info[names(ind_info)%in%c("ID","Salutations","First_name","Middle_name","Suffix_name","Gender","FisherID","Site")]
    # 
    # print(ind_info)
    # 
    # country_params<-accessCountryParam(pool)
    # 
    # is_fisher_query<-subset(country_params,CODE=="ISFISHER")$TEXT
    # if(length(is_fisher_query)>0){
    #   is_fisher_table<-suppressWarnings(dbGetQuery(pool, is_fisher_query))
    #   names(is_fisher_table)<-c("ID","Type")
    #   is_fisher_table$Type[is_fisher_table$Type==0]<-"Non Fisher"
    #   is_fisher_table$Type[is_fisher_table$Type==1]<-"Fisher"
    #   ind_info<-merge(ind_info,is_fisher_table)
    # }else{
    #   ind_info$Type<-NA
    # }
    # 
    # is_fisher_active_query<-subset(country_params,CODE=="ISFISHERACTIVE")$TEXT
    # if(length(is_fisher_active_query)>0){
    #   is_fisher_active_table<-suppressWarnings(dbGetQuery(pool, is_fisher_active_query))
    #   names(is_fisher_active_table)<-c("ID","Active")
    #   is_fisher_active_table$Active[is_fisher_active_table$Active==0]<-"Fisher Non Active"
    #   is_fisher_active_table$Active[is_fisher_active_table$Active==1]<-"Fisher Active"
    #   is_fisher_active_table$Active[is.na(is_fisher_active_table$Active)]<-"Non Fisher"
    #   ind_info<-merge(ind_info,is_fisher_active_table)
    # }else{
    #   ind_info$Active<-NA
    # }
    # 
    # ind_info<-unique(ind[!names(ind_info)=="ID"])
    # 
    # 
    # 
    # INFO("individual-list server: Fetching individual list data with rows '%s'", nrow(ind_info))
    # 
    # output$individual_list <- renderDT(
    #   ind_info,
    #   server = FALSE,
    #   escape = FALSE,
    #   rownames = FALSE,
    #   extensions = c("Buttons"),
    #   filter = list(position = 'top', clear = FALSE),
    #   
    #   options = list(
    #     autoWidth = FALSE,
    #     dom = 'Bfrtip',
    #     deferRender = TRUE,
    #     scroll = FALSE,
    #     buttons = list(
    #       list(extend = 'copy'),
    #       list(extend = 'csv', filename = i18n("INDIVIDUALS") , title = NULL, header = TRUE),
    #       list(extend = 'excel', filename =  i18n("INDIVIDUALS"), title = NULL, header = TRUE),
    #       list(extend = "pdf", title = i18n("INDIVIDUAL_LIST_TITLE"), header = TRUE, orientation = "landscape")
    #     ),
    #     exportOptions = list(
    #       modifiers = list(page = "all", selected = TRUE)
    #     ),
    #     language = list(url = i18n("TABLE_LANGUAGE")),
    #     
    #     pageLength = 10
    #   )
    # ) 
    
    
    
    observe({
      
      if(appConfig[["country_profile"]]$data$CODE=="DM"){
        
        observeEvent(input$fisher_chck_list,{if(isTRUE(input$fisher_chck_list))updateCheckboxInput(session,"nonfisher_chck_list",value = FALSE)})
        
        
        
        
        
        
        observeEvent({
          
          input$nonfisher_chck_list
          input$filter_fisher_category_list
          
        },
        {
          if(isFALSE(input$nonfisher_chck_list)){
            updateCheckboxInput(session,"fisher_chck_list",value = TRUE)
            category <- i18n("INDIVIDUAL_BREAKDOWN_LABEL_FISHER")
          }else {
            updateCheckboxInput(session,"fisher_chck_list",value = FALSE)
            category <- i18n("INDIVIDUAL_BREAKDOWN_LABEL_NONFISHER")
            
          }
          
          
          if(category==i18n("INDIVIDUAL_BREAKDOWN_LABEL_FISHER")){
            
            
            ind <- filter_category(data = ind,input = input$filter_fisher_category_list,column_indexes = c(10:13))
            
            
            INFO("individual-list server: Country choosen is '%s'",appConfig[["country_profile"]]$data$NAME)
            
            INFO("individual-list server: Indiviadual list filtered by individual roles '%s'", input$filter_fisher_category_list)
            
            ind$Details <- sapply(ind$IndividualNumber, function(x){
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
            
            ind <- ind %>% dplyr::filter(Category%in%'nonfisher')
            
            ind$Details <- sapply(ind$IndividualNumber, function(x){
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