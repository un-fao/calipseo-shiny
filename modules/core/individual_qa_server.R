#individual_qa_server
individual_qa_server <- function(id, parent.session, lang = NULL, pool, reloader) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    INFO("individual-qa: START")
    MODULE_START_TIME <- Sys.time()
    
    #-----------------------------------------------------------------------------
    i18n_translator <- get_reactive_translator(lang)
    i18n <- function(key){ i18n_translator()$t(key) }
    #-----------------------------------------------------------------------------
    
    output$individual_qa_info <- renderText({
      text <- paste0("<h3>", i18n("INDIVIDUAL_QA_TITLE")," – <small>", i18n("INDIVIDUAL_QA_SUBTITLE"),"</small></h3><hr>")
      text
    })

    #functions
    #individual_qa function to handle the QA data display
    individual_qa <- function(data){
        data$ACTION[data$ACTION=='ok'] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),i18n("INDIVIDUAL_QA_STATUS_VALID"))
        data$ACTION[data$ACTION=='to_check'] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')),i18n("INDIVIDUAL_QA_STATUS_TOCHECK"))
        names(data) <- c(i18n("INDIVIDUAL_QA_TABLE_COLNAME_1"),i18n("INDIVIDUAL_QA_TABLE_COLNAME_2"))
        return(data)
    }
      
    #accessor
    ind_qa_DOB <- accessIndividualQaDOB(pool)
    
    
    #individual_qa_dob
    output$individual_dob <- renderDataTable({
      DT::datatable(individual_qa(ind_qa_DOB),
                    escape = FALSE,
                    rownames = FALSE,
                    
                    options = list(
                      searching = FALSE,
                      dom = 't',
                      language = list(url = i18n("TABLE_LANGUAGE"))
                    ))
    }) 
    
    #main UI
    output$main <- renderUI({
      tagList(
        fluidRow(
          column(
            width = 12, style = "margin:12px;",
            htmlOutput(ns("individual_qa_info"))
          )
        ),
        
        bs4Dash::tabsetPanel(
          vertical = TRUE,
          type = "pills",
          tabPanel(
            title = tags$h5(i18n("VERTICALTABPANEL_INDIVIDUAL_QA_DOB")), box_height='70px',
            bs4Dash::box(
              width = 12, height = 480, 
              title = i18n("INDIVIDUAL_QA_TITLE_DOB"),
              DT::dataTableOutput(ns("individual_dob"))
            ),
            sliderWidth =25
          )
        )
      )
    })
    
    MODULE_END_TIME <- Sys.time()
    INFO("individual-qa: END")
    DEBUG_MODULE_PROCESSING_TIME("Individual-qa", MODULE_START_TIME, MODULE_END_TIME)
    
  })
  
}