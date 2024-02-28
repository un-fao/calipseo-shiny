#individual_list_server
individual_list_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$individual_list_info <- renderText({
      text <- paste0("<h2>", i18n("INDIVIDUAL_LIST_TITLE")," <small>", i18n("INDIVIDUAL_LIST_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
    
    ind <- accessIndividualDetails(pool)
  
    
    INFO("individual-list server: Fetching individual list data with rows '%s'", nrow(ind))
    
    
     ind_info <- accessIndividualInfo(pool)
     ind_info <- ind_info[names(ind_info)%in%c("ID","Salutations","First_name","Middle_name","Suffix_name","Last_name","GenderCode", "Gender","FisherID","Site")]
     
     country_params<-accessCountryParam(pool)
     
     is_fisher_query<-subset(country_params,CODE=="ISFISHER")$TEXT
     if(length(is_fisher_query)>0){
       is_fisher_table<-suppressWarnings(dbGetQuery(pool, is_fisher_query))
       names(is_fisher_table)<-c("ID","Type")
       is_fisher_table$Type[is_fisher_table$Type==0]<- i18n("INDIVIDUAL_LIST_TABLE_COLNAME_TYPE_NONFISHER")
       is_fisher_table$Type[is_fisher_table$Type==1]<- i18n("INDIVIDUAL_LIST_TABLE_COLNAME_TYPE_FISHER")
       ind_info<-merge(ind_info,is_fisher_table)
     }else{
       ind_info$Type<-NA
     }
     
     is_fisher_active_query<-subset(country_params,CODE=="ISFISHERACTIVE")$TEXT
     if(length(is_fisher_active_query)>0){
       is_fisher_active_table<-suppressWarnings(dbGetQuery(pool, is_fisher_active_query))
       names(is_fisher_active_table)<-c("ID","Status")
       is_fisher_active_table$Status[is_fisher_active_table$Status==0]<-"inactive"
       is_fisher_active_table$Status[is_fisher_active_table$Status==1]<-"active"
       is_fisher_active_table$Status[is.na(is_fisher_active_table$Status)]<-""
       ind_info<-merge(ind_info,is_fisher_active_table)
     }else{
       ind_info$Status<-NA
     }
     
     ind_info<-unique(subset(ind_info,select=c(Type,Status,FisherID,Salutations,First_name,Middle_name,Suffix_name,Last_name,GenderCode,Gender,Site,ID)))
     
     # ind_info$Details <- sapply(ind_info$ID, function(x){
     #  ind_outhtml <- sprintf("<a href=\"./?page=individual-info&individualNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
     #  return(ind_outhtml)
     # })
              
      ind_info<-ind_info[names(ind_info)!="ID"]
      
      js <- js_select2_filter_provider(ns("individual_list"))
     
      ind_info$Status<-ifelse(ind_info$Status=="active",paste0(icon("anchor-circle-check",style = 'color:green'),span(paste0(" ",i18n("INDIVIDUAL_LIST_TABLE_COLNAME_STATUS_ACTIVE")),style='color:green;')),
                              ifelse(ind_info$Status=="inactive",paste0(icon("anchor-circle-exclamation",style = 'color:orange'),span(paste0(" ", i18n("INDIVIDUAL_LIST_TABLE_COLNAME_STATUS_INACTIVE")),style='color:orange;')),
                                     paste0(icon("ban",style = 'color:gray'),span(paste0(" ",i18n("INDIVIDUAL_LIST_TABLE_COLNAME_STATUS_NOTCONCERNED")),style='color:gray;'))))
      
      ind_info$Gender<-sapply(1:nrow(ind_info), function(i){
        if(is.null(ind_info[i,]$GenderCode)) return(paste0(icon("ban",style = 'color:gray'),span(paste0(" ",i18n("INDIVIDUAL_LIST_TABLE_COLNAME_GENDER_UNDEFINED")),style='color:gray;')))
        switch(ind_info[i,]$GenderCode,
          "MALE" = paste0(icon("mars"),span(paste0(" ",ind_info[i,]$Gender))),
          "FEMALE" = paste0(icon("venus"),span(paste0(" ",ind_info[i,]$Gender)))
        )
      })
      ind_info$GenderCode = NULL
      for(i in 1:length(names(ind_info))){
        names(ind_info)[i] <- i18n(sprintf("INDIVIDUAL_LIST_TABLE_COLNAME_%s",toupper(names(ind_info)[i])))
      }

    INFO("individual-list server: Fetching individual list data with rows '%s'", nrow(ind_info))

    output$individual_list <- renderDT(
      ind_info,
      container = initDTContainer(ind_info),
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
    
  })
  
}