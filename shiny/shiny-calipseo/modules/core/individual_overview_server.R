#individual_overview_server
individual_overview_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {  
    
    ns <- session$ns
    
    output$individual_overview_info <- renderText({
      paste0("<h2>", i18n("INDIVIDUAL_OVERVIEW_TITLE")," <small>", i18n("INDIVIDUAL_OVERVIEW_SUBTITLE"))
      
    })
    
    ind_info <- accessIndividualInfo(pool)
    ind_info <- ind_info[!names(ind_info)%in%c("First_name","Middle_name","Suffix_name","Salutations","FisherID")]
    
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
      names(is_fisher_active_table)<-c("ID","Active")
      is_fisher_active_table$Active[is_fisher_active_table$Active==0]<-"Fisher Non Active"
      is_fisher_active_table$Active[is_fisher_active_table$Active==1]<-"Fisher Active"
      is_fisher_active_table$Active[is.na(is_fisher_active_table$Active)]<-"Non Fisher"
      ind_info<-merge(ind_info,is_fisher_active_table)
    }else{
      ind_info$Active<-NA
    }
    
    total_nb<-length(unique(ind_info$ID))
    non_fisher_nb<-length(unique(subset(ind_info,Type=="Non Fisher")$ID))
    fisher_nb<-length(unique(subset(ind_info,Type=="Fisher")$ID))
    fisher_active_nb<-length(unique(subset(ind_info,Active=="Fisher Active")$ID))
    license_nb<-nrow(unique(subset(ind_info,!is.na(License),select=c(ID,License))))
    license_active_nb<-nrow(unique(subset(ind_info,!is.na(License),select=c(ID,License))))
    
    
    output$indicators<-renderUI({
                div(
                  column(12,
                         infoBox(i18n("INFOBOX_TITLE_TOTAL"),total_nb , icon = icon("user"), fill = TRUE,color="blue",width = 3),
                         infoBox(i18n("INFOBOX_TITLE_NON_FISHER"),non_fisher_nb, icon = icon("user"), fill = TRUE,color="yellow",width = 3),
                         infoBox(i18n("INFOBOX_TITLE_FISHER"), fisher_nb, icon = icon("fish"), fill = TRUE,color="aqua",width = 3),
                         infoBox(i18n("INFOBOX_TITLE_FISHER_ACTIVE"),fisher_active_nb, icon = icon("circle-check"), fill = TRUE,color="green",width = 3)
                         
                  )
                  # column(12,
                  #        infoBox(i18n("INFOBOX_TITLE_LICENSE"),license_nb, icon = icon("id-card"), fill = TRUE,color="purple",width = 6),
                  #        infoBox(i18n("INFOBOX_TITLE_LICENSE_ACTIVE"),license_active_nb, icon = icon("circle-check"), fill = TRUE,color="green",width = 6)
                  # )
                )
    })
    
    colVariables<-c()
    
    if(!all(is.na(ind_info$Type))){
      colVariables<-c(colVariables,c("Type"="Type"))
      ind_info$Type[is.na(ind_info$Type)] <- "Unknown"
    }
    
    if(!all(is.na(ind_info$Active))){
      colVariables<-c(colVariables,c("Active"="Active"))
      ind_info$Active[is.na(ind_info$Active)] <- "Unknown"
    }
    
    if(!all(is.na(ind_info$Gender))){
      colVariables<-c(colVariables,c("Gender"="Gender"))
      ind_info$Gender[is.na(ind_info$Gender)] <- "Unknown"
    }
    
    if(!all(is.na(ind_info$Education))){
      colVariables<-c(colVariables,c("Education"="Education"))
      ind_info$Education[is.na(ind_info$Education)] <- "Unknown"
    }

    if(!all(is.na(ind_info$Worktime))){
      colVariables<-c(colVariables,c("Worktime"="Worktime"))
      ind_info$Worktime[is.na(ind_info$Worktime)] <- "Unknown"
    }

    if(!all(is.na(ind_info$License))){
      colVariables<-c(colVariables,c("License"="License"))
      ind_info$License[is.na(ind_info$License)] <- "Unknown"
    }

    if(!all(is.na(ind_info$Role))){
      colVariables<-c(colVariables,c("Role"="Role"))
      ind_info$Role[is.na(ind_info$Role)] <- "Unknown"
    }
     
   if(!all(is.na(ind_info$Site))){
      colVariables<-c(colVariables,c("Site"="Site"))
      ind_info$Site[is.na(ind_info$Site)] <- "Unknown"
   }
    

     
      pyramid_data<-ind_info%>%
        filter(!is.na(DOB))%>%
         mutate(Age=round(time_length(interval(DOB,Sys.Date()),"years"),0))%>%
         select(-DOB,-Regdate)%>%
        arrange(ID,Role,Site)%>%
        group_by(ID) %>%
        mutate(Role=paste0(unique(Role),collapse = "+"),
               Site=paste0(unique(Site),collapse = "+"))%>%
        ungroup()%>%
        distinct()%>%
         filter(Age>0)
    
      print(head(pyramid_data))
    print(unique(pyramid_data$Role))
      
    py_colVariables<-setNames(names(colVariables),colVariables)
     pyramid_chart_server("py", df=pyramid_data,colAge="Age",colGender=c("Gender"="Gender"),colVariables=py_colVariables[py_colVariables!="Gender"],mode="plot+table")
     
     sunburst_data<-ind_info%>%
       select(-DOB,-Regdate)%>%
       arrange(ID,Role,Site)%>%
       group_by(ID) %>%
       mutate(Role=paste0(unique(Role),collapse = "+"),
              Site=paste0(unique(Site),collapse = "+"))%>%
       ungroup()%>%
       distinct()%>%
       mutate(value=1)

     sunburst_chart_server("sb", df=sunburst_data,colVariables=colVariables,colValue="value",mode="plot+table")
     
     pretty_table_server("pt", df=sunburst_data,colVariables=colVariables,colValue="value")
    
  }
  
  )}