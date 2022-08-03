#individual_qa_server
individual_qa_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {
    
    output$individual_qa_info <- renderText({
      text <- paste0("<h2>", i18n("INDIVIDUAL_QA_TITLE")," <small>", i18n("INDIVIDUAL_QA_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
    
    individual_qa <- function(data){

        data$ACTION[data$ACTION=='ok'] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),i18n("INDIVIDUAL_QA_STATUS_VALID"))

        data$ACTION[data$ACTION=='to_check'] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')),i18n("INDIVIDUAL_QA_STATUS_TOCHECK"))

        names(data) <- c(i18n("INDIVIDUAL_QA_TABLE_COLNAME_1"),i18n("INDIVIDUAL_QA_TABLE_COLNAME_2"))

        return(data)
      }
      
    
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
    
    
  })
  
}