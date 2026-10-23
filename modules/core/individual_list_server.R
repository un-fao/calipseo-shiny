#individual_list_server
individual_list_server <- function(id, parent.session, lang = NULL, pool, reloader) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    INFO("individual-list: START")
    MODULE_START_TIME <- Sys.time()
    
    #i18n
    #-----------------------------------------------------------------------------
    i18n_translator <- get_reactive_translator(lang)
    i18n <- function(key){ i18n_translator()$t(key) }
    #-----------------------------------------------------------------------------
    
    processed_dataset <- reactiveVal(NULL)
    
    output$main <- renderUI({
      tagList(
        fluidRow(
          bs4Dash::box(
            title = htmlOutput(ns("individual_list_info")),
            width = 12,
            div(selectInput(ns("ctry_display_cntrl"),label="", choices = appConfig[["country_profile"]]$data$CODE),style='display:none;'),
            conditionalPanel(
              condition = "input.ctry_display_cntrl =='DM'",ns = ns,
              h4(i18n("INDIVIDUAL_TYPE")),
              span(checkboxInput(ns("fisher_chck_list"), label = i18n("INDIVIDUAL_BREAKDOWN_TITLE_FISHER")),checkboxInput(ns("nonfisher_chck_list"), label = i18n("INDIVIDUAL_BREAKDOWN_TITLE_NONFISHER"))),
              conditionalPanel(
                condition = "input.fisher_chck_list == 1",ns = ns,
                selectizeInput(ns("filter_fisher_category_list"),label = i18n("SELECT_FISHER_CATEGORY"),choices = c(i18n("INDIVIDUAL_LIST_LABEL_OWNER"),i18n("INDIVIDUAL_LIST_LABEL_CAPTAIN"),i18n("INDIVIDUAL_LIST_LABEL_HOLDER_FISHING_ID"),i18n("INDIVIDUAL_LIST_LABEL_HOLDER_FISHING_LICENSE")),multiple = FALSE, width = '40%')
              )
            ),
            withSpinner(DT::dataTableOutput(ns("individual_list"))),
            maximizable = TRUE,
            collapsible = FALSE
          )
        )
      )
    })
    
    output$individual_list_info <- renderText({
      text <- paste0("<h3>", i18n("INDIVIDUAL_LIST_TITLE")," – <small>", i18n("INDIVIDUAL_LIST_SUBTITLE"),"</small></h3>")
      text
    })
    
    observe({
      
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
      
      processed_dataset(individuals)
      
    })
    
    INFO("individual-list: Rendering the individual list data to the table")
    js <- js_select2_filter_provider(ns("individual_list"))
    output$individual_list <- renderDT(
      processed_dataset(),
      container = initDTContainer(processed_dataset()),
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