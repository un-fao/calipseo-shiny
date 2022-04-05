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
  vessel_qa_characteristics_df <- accessVesselQaCharacteristics(pool)
  
  vessel_qa_characteristics_df <- reshape(vessel_qa_characteristics_df, direction = 'wide', timevar = 'CHARA',idvar = 'ACTION')
  
  names(vessel_qa_characteristics_df) <- gsub("COUNT.", "", names(vessel_qa_characteristics_df), fixed = TRUE)
  
  
  vessel_qa <- function(data){
    
    if(ncol(data)>2){
      
      data$ACTION[data$ACTION=='ok'] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),i18n("VESSEL_QA_STATUS_CHAR_VALID"))
      
      data$ACTION[data$ACTION=='to_check'] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')),i18n("VESSEL_QA_STATUS_CHAR_TOCHECK"))
      
      
      names(data) <- c(i18n("VESSEL_QA_TABLE_CHAR_COLNAME_1"),i18n("VESSEL_QA_TABLE_CHAR_COLNAME_2"),
                       i18n("VESSEL_QA_TABLE_CHAR_COLNAME_3"),i18n("VESSEL_QA_TABLE_CHAR_COLNAME_4"),
                       i18n("VESSEL_QA_TABLE_CHAR_COLNAME_5"),i18n("VESSEL_QA_TABLE_CHAR_COLNAME_6"))
    }else{
      
      data$ACTION[data$ACTION=='ok'] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')),i18n("VESSEL_QA_STATUS_VALID"))
      
      data$ACTION[data$ACTION=='to_check'] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')),i18n("VESSEL_QA_STATUS_TOCHECK"))
      
      names(data) <- c(i18n("VESSEL_QA_TABLE_COLNAME_1"),i18n("VESSEL_QA_TABLE_COLNAME_2")) 
    }
    
    
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
  
  #vessel_qa_characteristics
  output$vessel_characteristics <- renderDataTable({
    
    vessel_qa(vessel_qa_characteristics_df)
    
  })
  
}