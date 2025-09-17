#individual_info_server
individual_info_server <- function(id, parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session) {
    
    INFO("individual-info: START")
    MODULE_START_TIME <- Sys.time()
    
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
          cat(sprintf("Selecting individual number '%s'\n", query[["individualNumber"]]))
          individualId <- query[["individualNumber"]]
          INFO("individual-info server: Displaying info on individual number '%s'", individualId)
        }
      }
      
      
      if(!is.null(individualId)){
        
        individual <- accessIndividual(pool, individualNumber = individualId)
        INFO("individual-info server: Fetching individual info data with rows '%s'", nrow(individual))
        
        fishing_roles <- individual$FSH_CODE
        
        individual <- individual[1,]
        
        
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
        INFO("individual-info server: Computing age of individuals from date of birth")  
          individual <- Age_comp(individual, Prep = FALSE)
          
          for (i in 1:ncol(individual)){
            individual[,i] <- as.character(individual[,i])
            if(i>5 & i!=15)individual[,i][individual[,i]== ""] <- "-"}
          ifelse(individual$Age=="","",individual$Age <- sprintf('(%s y/o)',individual$Age))#paste0("'Age: ",individual$Age,"'"))
  
        INFO("indiviadual-info server: Rendering the individual info data for the ID")     
          tags$ul(style = "margin-top:10px;",
                  tags$li(paste0(i18n("INDIVIDUAL_NAME"),": "), tags$b(individual$Salutation,individual$FULL_NAME)),
                  tags$li(paste0(i18n("INDIVIDUAL_DOB"),": "), tags$b(individual$DOB),individual$Age),
                  tags$li(paste0(i18n("INDIVIDUAL_GENDER"),": "), tags$b(individual$Gender)),
                  tags$li(paste0(i18n("INDIVIDUAL_EDUCATIONAL_LEVEL"),": "), tags$b(individual$EDULEVEL)),
                  tags$li(paste0(i18n("INDIVIDUAL_ENT_DOC_TYPE"),": "), tags$b(individual$ENT_DOC_NAME)),
                  tags$li(paste0(i18n("INDIVIDUAL_ENT_DOC_NUMBER"),": "), tags$b(individual$ENT_DOC_NUMBER))
                  
          )
        })

        output$individual_picture <- renderUI({
          
          INFO("individual-info server: Returning Placeholder image for individual")
          individual_picture_html <- HTML(createPlaceholderImage("individual"))
          
          individual_picture_html
          
        })
        
        ind_roles <- accessIndividuals(pool)[,c("FSH_CODE","FSH_ROLE")]
        
        INFO("individual-info server: Fetching entire individual info data and their fishing roles with rows '%s'", nrow(ind_roles))

        ind_roles <- unique(ind_roles[!is.na(ind_roles$FSH_CODE),])
       

        ind_roles$status <- NA
        
        if(!is.na(fishing_roles[1])){

        for (i in 1:length(fishing_roles)) ind_roles[ind_roles$FSH_CODE==fishing_roles[i],"status"] <- paste(as.character(icon("ok",lib = "glyphicon",style = 'color:green;')))
        
       
        ind_roles[is.na(ind_roles$status),"status"] <- paste(as.character(icon("remove",lib = "glyphicon",style = 'color:red;')))
        
        }else{
          
          ind_roles[is.na(ind_roles$status),"status"] <- paste(as.character(icon("alert",lib = "glyphicon",style = 'color:orange;')))
          
        }

        ind_roles <- ind_roles[,c("FSH_ROLE","status")]
      INFO("individual-info server: Applying the I18n_terms to the individual info columns")
        names(ind_roles) <- c(i18n("INDIVIDUAL_ROLE_TABLE_COLNAME_1"),i18n("INDIVIDUAL_ROLE_TABLE_COLNAME_2"))
        
      INFO("indiviadual-info server: Rendering the individual info fishing role data to the table") 
        
        output$individual_roles <- renderDataTable({
          
          DT::datatable(ind_roles,
                        escape = FALSE,
                        rownames = FALSE,
                        
                        options = list(
                          searching = FALSE,
                          dom = 't',
                          pageLength = 18,
                          language = list(url = i18n("TABLE_LANGUAGE"))
                        ))
          
        })
        
        
        
      }
      
      MODULE_END_TIME <- Sys.time()
      INFO("individual-info: END")
      DEBUG_MODULE_PROCESSING_TIME("Individual-info", MODULE_START_TIME, MODULE_END_TIME)
      
    })
    
  })
}