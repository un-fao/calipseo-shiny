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
     ind_info <- ind_info[names(ind_info)%in%c("ID","Salutations","First_name","Middle_name","Suffix_name","Last_name","Gender","FisherID","Site")]
     
     country_params<-accessCountryParam(pool)
     
     is_fisher_query<-subset(country_params,CODE=="ISFISHER")$TEXT
     if(length(is_fisher_query)>0){
       is_fisher_table<-suppressWarnings(dbGetQuery(pool, is_fisher_query))
       names(is_fisher_table)<-c("ID","Type")
       is_fisher_table$Type[is_fisher_table$Type==0]<-"Non Fisher"
       is_fisher_table$Type[is_fisher_table$Type==1]<-"Fisher"
       ind_info<-merge(ind_info,is_fisher_table)
     }else{
       ind_info$Type<-NA
     }
     
     is_fisher_active_query<-subset(country_params,CODE=="ISFISHERACTIVE")$TEXT
     if(length(is_fisher_active_query)>0){
       is_fisher_active_table<-suppressWarnings(dbGetQuery(pool, is_fisher_active_query))
       names(is_fisher_active_table)<-c("ID","Status")
       is_fisher_active_table$Status[is_fisher_active_table$Status==0]<-"Inactive"
       is_fisher_active_table$Status[is_fisher_active_table$Status==1]<-"Active"
       is_fisher_active_table$Status[is.na(is_fisher_active_table$Status)]<-""
       ind_info<-merge(ind_info,is_fisher_active_table)
     }else{
       ind_info$Active<-NA
     }
     
     ind_info<-unique(subset(ind_info,select=c(Type,Status,FisherID,Salutations,First_name,Middle_name,Suffix_name,Last_name,Gender,Site,ID)))
     ind_info<-ind_info[!sapply(ind_info, function(x) all(x == ""|is.na(x)))]
     
     # ind_info$Details <- sapply(ind_info$ID, function(x){
     #  ind_outhtml <- sprintf("<a href=\"./?page=individual-info&individualNumber=%s\" style=\"font-weight:bold;\">Details</a>", x)
     #  return(ind_outhtml)
     # })
              
      ind_info<-ind_info[names(ind_info)!="ID"]
      
      for(i in 1:length(names(ind_info))){
        names(ind_info)[i] <- i18n(sprintf("INDIVIDUAL_LIST_TABLE_COLNAME_%s",toupper(names(ind_info)[i])))
      }
     
      
      js <- js_select2_filter_provider(ns("individual_list"))
     
      ind_info$Status<-ifelse(ind_info$Status=="Active",paste0(icon("anchor-circle-check",style = 'color:green'),span(" Active",style='color:green;')),
                              ifelse(ind_info$Status=="Inactive",paste0(icon("anchor-circle-exclamation",style = 'color:orange'),span(" Inactive",style='color:orange;')),
                                     ""))
      ind_info$Gender<-ifelse(ind_info$Gender=="Male",paste0(icon("mars"),span(" Male")),
                              ifelse(ind_info$Gender=="Female",paste0(icon("venus"),span(" Female")),
                                     ind$Gender))

      
     
     print("START TEST PRINT")
     print(ind_info)
     print("END TEST PRINT")



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