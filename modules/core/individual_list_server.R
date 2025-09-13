#individual_list_server
individual_list_server <- function(id, pool, reloader) {
  
  moduleServer(id, function(input, output, session) {
    
    INFO("individual-list: START")
    MODULE_START_TIME <- Sys.time()
    ns <- session$ns
    
    output$individual_list_info <- renderText({
      text <- paste0("<h2>", i18n("INDIVIDUAL_LIST_TITLE")," <small>", i18n("INDIVIDUAL_LIST_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
    individuals <- accessIndividuals(pool)
    INFO("individual-list: Fetching individual list data - %s rows", nrow(individuals))
     
    individuals <- individuals[names(individuals) %in% c("ID","Salutations","First_name","Middle_name","Suffix_name","Last_name","GenderCode", "Gender","FisherID")]
     
    is_fisher_query<-subset(COUNTRY_PARAMS, CODE == "ISFISHER")$TEXT
    if(length(is_fisher_query) > 0){
      DEBUG("Query is_fisher_table")
      is_fisher_table <- getFromSQL(pool, is_fisher_query)
      names(is_fisher_table) <- c("ID","Type")
      is_fisher_table$Type[is_fisher_table$Type == 0] <- i18n("INDIVIDUAL_LIST_TABLE_COLNAME_TYPE_NONFISHER")
      is_fisher_table$Type[is_fisher_table$Type == 1] <- i18n("INDIVIDUAL_LIST_TABLE_COLNAME_TYPE_FISHER")
      individuals <- merge(individuals, is_fisher_table)
      INFO("individual-list: Fetching individual list after adding ISFISHER - %s rows", nrow(individuals))
    }else{
      individuals$Type <- NA
    }
    
     
    is_fisher_active_query<-subset(COUNTRY_PARAMS, CODE == "ISFISHERACTIVE")$TEXT
    if(length(is_fisher_active_query) > 0){
      DEBUG("Query is_fisher_active_table")
      is_fisher_active_table <- getFromSQL(pool, is_fisher_active_query)
      names(is_fisher_active_table) <- c("ID","Status")
      is_fisher_active_table$Status[is_fisher_active_table$Status == 0] <- "inactive"
      is_fisher_active_table$Status[is_fisher_active_table$Status == 1] <- "active"
      is_fisher_active_table$Status[is.na(is_fisher_active_table$Status)] <- ""
      individuals <- merge(individuals,is_fisher_active_table)
      INFO("individual-list: Fetching individual list after adding ISFISHERACTIVE - %s rows", nrow(individuals))
    }else{
      individuals$Status <- NA
    }
    
    test_start_time = Sys.time()
    individuals <- unique(subset(individuals, select = c(Type,Status,FisherID,Salutations,First_name,Middle_name,Suffix_name,Last_name,GenderCode,Gender,ID)))
    INFO("individual-list server: Fetching individual list after applying unique - %s rows", nrow(individuals))
    test_end_time = Sys.time()
    test_test = test_end_time-test_start_time
    DEBUG("!!!!!!!!!!!!! %s %s", as(test_test, "numeric"), attr(test_test, "units"))
    
    # individuals$Details <- sapply(individuals$ID, function(x){
    #  ind_outhtml <- sprintf("<a href=\"./?page=individual-info&individualNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
    #  return(ind_outhtml)
    # })
              
    individuals <- individuals[names(individuals) != "ID"]
      
    js <- js_select2_filter_provider(ns("individual_list"))
     
    individuals$Status<-ifelse(individuals$Status=="active",paste0(icon("anchor-circle-check",style = 'color:green'),span(paste0(" ",i18n("INDIVIDUAL_LIST_TABLE_COLNAME_STATUS_ACTIVE")),style='color:green;')),
                              ifelse(individuals$Status=="inactive",paste0(icon("anchor-circle-exclamation",style = 'color:orange'),span(paste0(" ", i18n("INDIVIDUAL_LIST_TABLE_COLNAME_STATUS_INACTIVE")),style='color:orange;')),
                                     paste0(icon("ban",style = 'color:gray'),span(paste0(" ",i18n("INDIVIDUAL_LIST_TABLE_COLNAME_STATUS_NOTCONCERNED")),style='color:gray;'))))
      
    individuals$Gender <- sapply(1:nrow(individuals), function(i){
        if(is.null(individuals[i,]$GenderCode)) return(paste0(icon("ban",style = 'color:gray'),span(paste0(" ",i18n("INDIVIDUAL_LIST_TABLE_COLNAME_GENDER_UNDEFINED")),style='color:gray;')))
        switch(individuals[i,]$GenderCode,
          "MALE" = paste0(icon("mars"),span(paste0(" ",individuals[i,]$Gender))),
          "FEMALE" = paste0(icon("venus"),span(paste0(" ",individuals[i,]$Gender)))
        )
    })
    individuals$GenderCode = NULL
    for(i in 1:length(names(individuals))){
      names(individuals)[i] <- i18n(sprintf("INDIVIDUAL_LIST_TABLE_COLNAME_%s",toupper(names(individuals)[i])))
    }

    INFO("individual-list: Rendering the individual list data to the table")
    output$individual_list <- renderDT(
      individuals,
      container = initDTContainer(individuals),
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
        pageLength = 10,
        initComplete = js
      )
    )
    
    MODULE_END_TIME <- Sys.time()
    INFO("individual-list: END")
    DEBUG_MODULE_PROCESSING_TIME("Individual-list", MODULE_START_TIME, MODULE_END_TIME)
    
  })
  
}