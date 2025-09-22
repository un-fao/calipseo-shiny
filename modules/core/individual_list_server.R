#individual_list_server
individual_list_server <- function(id, parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session) {
    
    INFO("individual-list: START")
    MODULE_START_TIME <- Sys.time()
    ns <- session$ns
    
    output$individual_list_info <- renderText({
      text <- paste0("<h3>", i18n("INDIVIDUAL_LIST_TITLE")," – <small>", i18n("INDIVIDUAL_LIST_SUBTITLE"),"</small></h3>")
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
    
    individuals <- unique(subset(individuals, select = c(Type,Status,FisherID,Salutations,First_name,Middle_name,Suffix_name,Last_name,GenderCode,Gender,ID)))
    INFO("individual-list server: Fetching individual list after applying unique - %s rows", nrow(individuals))
    
    
    # individuals$Details <- sapply(individuals$ID, function(x){
    #  ind_outhtml <- sprintf("<a href=\"./?page=individual-info&individualNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
    #  return(ind_outhtml)
    # })
       
    #postprocessing       
    individuals <- individuals[names(individuals) != "ID"]
    
    #post-process status for UI
    if(any(is.na(individuals$Status))) individuals$Status[is.na(individuals$Status)] <- paste0(icon("ban",style = 'color:gray;margin-right:5px;'),span(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_STATUS_NOTCONCERNED"),style='color:gray;'))
    individuals$Status[!is.na(individuals$Status) & individuals$Status == "active"] <- paste0(icon("anchor-circle-check",style = 'color:green;margin-right:5px;'),span(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_STATUS_ACTIVE"),style='color:green;'))
    individuals$Status[!is.na(individuals$Status) & individuals$Status == "inactive"] <- paste0(icon("anchor-circle-exclamation",style = 'color:orange;margin-right:5px;'),span(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_STATUS_INACTIVE"),style='color:orange;'))

    #post-process gender for UI
    if(any(is.na(individuals$GenderCode))) individuals$Gender[is.na(individuals$GenderCode)] <- paste0(icon("ban",style = 'color:gray;margin-right:5px;'),span(i18n("INDIVIDUAL_LIST_TABLE_COLNAME_GENDER_UNDEFINED"),style='color:gray;'))
    individuals$Gender[individuals$GenderCode=="MALE"] <- paste0(icon("mars"),span(paste0(" ", individuals[individuals$GenderCode=="MALE",][1,]$Gender)))
    individuals$Gender[individuals$GenderCode=="FEMALE"] <- paste0(icon("venus"),span(paste0(" ", individuals[individuals$GenderCode=="FEMALE",][1,]$Gender)))
    
    #colnames
    individuals$GenderCode = NULL
    for(i in 1:length(names(individuals))){
      names(individuals)[i] <- i18n(sprintf("INDIVIDUAL_LIST_TABLE_COLNAME_%s",toupper(names(individuals)[i])))
    }
    
    INFO("individual-list: Rendering the individual list data to the table")
    js <- js_select2_filter_provider(ns("individual_list"))
    output$individual_list <- renderDT(
      individuals,
      container = initDTContainer(individuals),
      server = TRUE,
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