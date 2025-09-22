#individual_overview_server
individual_overview_server <- function(id, parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session) {  
    
    INFO("individual-overview: START")
    MODULE_START_TIME <- Sys.time()
    
    ns <- session$ns
    
    output$individual_overview_info <- renderText({
      paste0("<h3>", i18n("INDIVIDUAL_OVERVIEW_TITLE")," <small>", i18n("INDIVIDUAL_OVERVIEW_SUBTITLE"), "</h3><hr>")
      
    })
    
    ind_info <- accessIndividualInfo(pool)
    ind_info <- ind_info[!names(ind_info) %in% c("First_name","Middle_name","Suffix_name","Salutations","FisherID")]
    
    is_fisher_query<-subset(COUNTRY_PARAMS,CODE=="ISFISHER")$TEXT
    if(length(is_fisher_query) > 0){
      DEBUG("Query 'is_fisher_table'")
      is_fisher_table <- getFromSQL(pool, is_fisher_query)
      names(is_fisher_table) <- c("ID","Type")
      is_fisher_table$Type[is_fisher_table$Type==0] <- i18n("INDIVIDUAL_TYPE_NONFISHER")
      is_fisher_table$Type[is_fisher_table$Type==1] <- i18n("INDIVIDUAL_TYPE_FISHER")
      ind_info<-merge(ind_info,is_fisher_table)
    }else{
      ind_info$Type<-NA
    }
    
    is_fisher_active_query<-subset(COUNTRY_PARAMS,CODE=="ISFISHERACTIVE")$TEXT
    if(length(is_fisher_active_query) > 0){
      DEBUG("Query 'is_fisher_active_table'")
      is_fisher_active_table <- getFromSQL(pool, is_fisher_active_query)
      names(is_fisher_active_table) <- c("ID","Status")
      is_fisher_active_table$Status[is_fisher_active_table$Status==0] <- i18n("INDIVIDUAL_FISHER_STATUS_INACTIVE")
      is_fisher_active_table$Status[is_fisher_active_table$Status==1] <- i18n("INDIVIDUAL_FISHER_STATUS_ACTIVE")
      is_fisher_active_table$Status[is.na(is_fisher_active_table$Status)] <- i18n("INDIVIDUAL_TYPE_NONFISHER")
      ind_info<-merge(ind_info, is_fisher_active_table)
    }else{
      ind_info$Status<-NA
    }
    
    total_nb<-length(unique(ind_info$ID))
    non_fisher_nb<-length(unique(subset(ind_info, Type==i18n("INDIVIDUAL_TYPE_NONFISHER"))$ID))
    fisher_nb<-length(unique(subset(ind_info, Type==i18n("INDIVIDUAL_TYPE_FISHER"))$ID))
    fisher_active_nb<-length(unique(subset(ind_info, Status==i18n("INDIVIDUAL_FISHER_STATUS_ACTIVE"))$ID))
    license_nb<-nrow(unique(subset(ind_info, !is.na(License), select=c(ID, License))))
    license_active_nb<-nrow(unique(subset(ind_info, !is.na(License), select=c(ID, License))))
    
    
    #UI for individual indicators
    output$indicators<-renderUI({
      fluidRow(
        infoBox(i18n("INFOBOX_TITLE_TOTAL"),total_nb , icon = icon("user"), fill = TRUE,color="navy",width = 3),
        infoBox(i18n("INFOBOX_TITLE_NON_FISHER"),non_fisher_nb, icon = icon("user"), fill = TRUE,color="fuchsia",width = 3),
        infoBox(i18n("INFOBOX_TITLE_FISHER"), fisher_nb, icon = icon("fish"), fill = TRUE,color="lightblue",width = 3),
        infoBox(i18n("INFOBOX_TITLE_FISHER_ACTIVE"),fisher_active_nb, icon = icon("circle-check"), fill = TRUE,color="primary",width = 3)
      )
      # fluidRow
      #   infoBox(i18n("INFOBOX_TITLE_LICENSE"),license_nb, icon = icon("id-card"), fill = TRUE,color="purple",width = 6),
      #   infoBox(i18n("INFOBOX_TITLE_LICENSE_ACTIVE"),license_active_nb, icon = icon("circle-check"), fill = TRUE,color="green",width = 6)
      # )
    })
    
    colVariables<-c()
    if(!all(is.na(ind_info$Type))){
      colVariables<-c(colVariables,c("Type" = i18n("INDIVIDUAL_PROPERTY_TYPE")))
      ind_info$Type[is.na(ind_info$Type)] <- i18n("INDIVIDUAL_UNKNOWN_VALUE")
    }
    if(!all(is.na(ind_info$Status))){
      colVariables<-c(colVariables,c("Status" = i18n("INDIVIDUAL_PROPERTY_STATUS")))
      ind_info$Status[is.na(ind_info$Status)] <- i18n("INDIVIDUAL_UNKNOWN_VALUE")
    }
    if(!all(is.na(ind_info$Gender))){
      colVariables<-c(colVariables,c("Gender"=i18n("INDIVIDUAL_PROPERTY_GENDER")))
      ind_info$Gender[is.na(ind_info$Gender)] <- i18n("INDIVIDUAL_UNKNOWN_VALUE")
    }
    if(!all(is.na(ind_info$Education))){
      colVariables<-c(colVariables,c("Education" = i18n("INDIVIDUAL_PROPERTY_EDUCATION")))
      ind_info$Education[is.na(ind_info$Education)] <- i18n("INDIVIDUAL_UNKNOWN_VALUE")
    }
    if(!all(is.na(ind_info$Worktime))){
      colVariables<-c(colVariables,c("Worktime" = i18n("INDIVIDUAL_PROPERTY_WORKTIME")))
      ind_info$Worktime[is.na(ind_info$Worktime)] <- i18n("INDIVIDUAL_UNKNOWN_VALUE")
    }

    if(!all(is.na(ind_info$License))){
      colVariables<-c(colVariables,c("License"=i18n("INDIVIDUAL_PROPERTY_LICENSE")))
      ind_info$License[is.na(ind_info$License)] <- i18n("INDIVIDUAL_UNKNOWN_VALUE")
    }
    if(!all(is.na(ind_info$Role))){
      colVariables<-c(colVariables,c("Role" = i18n("INDIVIDUAL_PROPERTY_ROLE")))
      ind_info$Role[is.na(ind_info$Role)] <- i18n("INDIVIDUAL_UNKNOWN_VALUE")
    }
    if(!all(is.na(ind_info$Site))){
      colVariables<-c(colVariables,c("Site" = i18n("INDIVIDUAL_PROPERTY_SITE")))
      ind_info$Site[is.na(ind_info$Site)] <- i18n("INDIVIDUAL_UNKNOWN_VALUE")
    }
     
    #pyramid data
    pyramid_data <- ind_info %>%
        filter(!is.na(DOB)) %>%
         mutate(Age = round(time_length(interval(DOB, Sys.Date()),"years"),0)) %>%
         select(-DOB,-Regdate,-GenderCode) %>%
        arrange(ID,License,Role,Site) %>%
        group_by(ID) %>%
        mutate(License=paste0(unique(License),collapse = "+"),
               Role=paste0(unique(Role),collapse = "+"),
               Site=paste0(unique(Site),collapse = "+")) %>%
        ungroup() %>%
        distinct() %>%
         filter(Age>0)
    py_colVariables<-setNames(names(colVariables),colVariables)
    
    #pyramid chart server
    pyramid_chart_server(
      id = "py", 
      df = pyramid_data,
      colAge = "Age",
      colGender = c("Gender"="Gender"),
      colVariables = py_colVariables[py_colVariables!="Gender"],
      mode = "plot+table"
    )
     
    #sunburst data
    sunburst_data <- ind_info %>%
       select(-DOB,-Regdate,-GenderCode) %>%
       arrange(ID,License,Role,Site) %>%
       group_by(ID) %>%
       mutate(License=paste0(unique(License),collapse = "+"),
              Role=paste0(unique(Role),collapse = "+"),
              Site=paste0(unique(Site),collapse = "+")) %>%
       ungroup() %>%
       distinct() %>%
       mutate(value = 1)

    #sunburst chart server
    sunburst_chart_server(
      id = "sb", 
      df = sunburst_data,
      colVariables = colVariables,
      colValue = "value",
      mode = "plot+table"
    )
    
    #pretty table server 
    pretty_table_server(
      id = "pt", 
      df = sunburst_data,
      colVariables = colVariables,
      colValue = "value"
    )
    
     
    MODULE_END_TIME <- Sys.time()
    INFO("individual-overview: END")
    DEBUG_MODULE_PROCESSING_TIME("Individual-overview", MODULE_START_TIME, MODULE_END_TIME)
     
  }
  
  )}