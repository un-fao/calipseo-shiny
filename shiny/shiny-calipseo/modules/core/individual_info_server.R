#individual_info_server
individual_info_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {
    
    output$individual_header <- renderText({
      text <- paste0("<a href=\"./?page=individual-list\" style=\"float:right;font-weight:bold;margin-right:10px;\">","<< ",i18n("BACK_TO_LIST_OF_INDIVIDUALS"),"</a>")
      text
    })
    
    
    observe({

      individualId <- NULL
      #inherit individual Id
      query <- parseQueryString(session$clientData$url_search)
      if(length(query)>0){
        if(!is.null(query[["individualNumber"]])) {
          cat(sprintf("Selecting individual Id number '%s'\n", query[["individualNumber"]]))
          individualId <- query[["individualNumber"]]
          INFO("individual-info server: Displaying info on individual number '%s'", individualId)
        }
      }
      
      
      if(!is.null(individualId)){
        
        individual <- accessIndividual(pool,individualId)
        
         for (i in 1:ncol(individual))individual[,i] <- as.character(individual[,i])
         individual[is.na(individual)] = ""

        if(nrow(individual)>0){
          INFO("individual-info server: Getting the full individual names")
          individual$FULL_NAME <- sapply(1:nrow(individual), function(i){
            ind <- individual[i,]
            individual_fullname <- ind$FIRST_NAME
            if(length(ind$MIDDLE_NAME)>0) individual_fullname <- paste(individual_fullname, ind$MIDDLE_NAME)
            if(length(ind$LAST_NAME)>0) individual_fullname <- paste(individual_fullname, ind$LAST_NAME)
            return(individual_fullname)
          })
        }
        
        
        #individual full name
        output$individual_name <- renderUI({tags$b(paste0(i18n("INDIVIDUAL_INFO_TITLE")))})
        
        
        #general individual description
        output$individual_description <- renderUI({
          
          individual <- Age_comp(individual, Prep = FALSE)
          
          for (i in 1:ncol(individual)){
            individual[,i] <- as.character(individual[,i])
            if(i>5)individual[,i][individual[,i]== ""] <- "-"
          }
          
          
          tags$ul(style = "margin-top:10px;",
                  tags$li(paste0(i18n("INDIVIDUAL_NAME"),": "), tags$b(individual$Salutation,individual$FULL_NAME)),
                  tags$li(paste0(i18n("INDIVIDUAL_AGE"),": "), tags$b(individual$Age)),
                  tags$li(paste0(i18n("INDIVIDUAL_GENDER"),": "), tags$b(individual$Gender)),
                  tags$li(paste0(i18n("INDIVIDUAL_EDUCATIONAL_LEVEL"),": "), tags$b(individual$EDULEVEL))
                  
          )
        })
        
        
        output$individual_picture <- renderUI({
          
          individual_picture_html <- HTML(createPlaceholderImage("individual"))
          
          individual_picture_html
          
        })
        
        
        
      }
      
      
    })
    
  })
}