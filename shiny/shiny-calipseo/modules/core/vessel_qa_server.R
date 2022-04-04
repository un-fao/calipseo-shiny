#vessel_qa_server
vessel_qa_server <- function(input, output, session, pool) {
  
  
  output$vessel_qa_info <- renderText({
    session$userData$page("vessel-qa")
    updatePageUrl("vessel-qa", session)
    text <- paste0("<h2>", i18n("VESSEL_QA_TITLE")," <small>", i18n("VESSEL_QA_SUBTITLE"),"</small></h2><hr>")
    text
  })
  
  #vessel_qa_data
  vessel_qa_names_df <- accessVesselQaNames(pool)
  vessel_qa_homeport_df <- accessVesselQaHomeport(pool)
  vessel_qa_regport_df <- accessVesselQaRegport(pool)
  
  
    vessel_qa <- function(data){
      
      data$ACTION[data$ACTION=='ok'] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),'Valid')
      
      data$ACTION[data$ACTION=='to_check'] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')),'To check')
      
      names(data) <- c(i18n("VESSEL_QA_TABLE_COLNAME_1"),i18n("VESSEL_QA_TABLE_COLNAME_2"))
      
      
      out <- DT::datatable(data,
                    escape = FALSE,
                    rownames = FALSE,
                    
                    options = list(
                      searching = FALSE,
                      dom = 't',
                      language = list(url = i18n("TABLE_LANGUAGE"))
                    ))
      
      return(out)
    }
  
   
  #vessel_qa_names
  output$vessel_names <- renderDataTable({
    
    vessel_qa(vessel_qa_names_df)
  }) 
  
  
  #vessel_qa_homeport
  output$vessel_homeport <- renderDataTable({
    
    vessel_qa(vessel_qa_homeport_df)
   
  }) 
  
  #vessel_qa_regport
  output$vessel_regport <- renderDataTable({
    
    vessel_qa(vessel_qa_regport_df)
    
  
  
  })
  
}