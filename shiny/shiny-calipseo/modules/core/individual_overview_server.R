#individual_overview_server
individual_overview_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {  
    
    ns <- session$ns
    
    output$individual_overview_info <- renderText({
      paste0("<h2>", i18n("INDIVIDUAL_OVERVIEW_TITLE")," <small>", i18n("INDIVIDUAL_OVERVIEW_SUBTITLE"))
      
    })
    
    ind_info <- accessIndividualInfo(pool)
    
    total_nb<-length(unique(ind_info$ID))
    non_fisher_nb<-0
    fisher_nb<-total_nb
    fisher_active_nb<-fisher_nb
    license_nb<-nrow(unique(subset(ind_info,!is.na(License),select=c(ID,License))))
    license_active_nb<-nrow(unique(subset(ind_info,!is.na(License),select=c(ID,License))))
    
    
    output$indicators<-renderUI({
                div(
                  column(12,
                         infoBox(i18n("INFOBOX_TITLE_TOTAL"),total_nb , icon = icon("user"), fill = TRUE,color="blue",width = 6),
                         infoBox(i18n("INFOBOX_TITLE_NON_FISHER"),non_fisher_nb, icon = icon("user"), fill = TRUE,color="yellow",width = 6)
                         
                  ),
                  column(12,
                         infoBox(i18n("INFOBOX_TITLE_FISHER"), fisher_nb, icon = icon("fish"), fill = TRUE,color="aqua",width = 6),
                         infoBox(i18n("INFOBOX_TITLE_FISHER_ACTIVE"),fisher_active_nb, icon = icon("circle-check"), fill = TRUE,color="green",width = 6)
                         
                  ),
                  column(12,
                         infoBox(i18n("INFOBOX_TITLE_LICENSE"),license_nb, icon = icon("id-card"), fill = TRUE,color="purple",width = 6),
                         infoBox(i18n("INFOBOX_TITLE_LICENSE_ACTIVE"),license_active_nb, icon = icon("circle-check"), fill = TRUE,color="green",width = 6)
                  )
                )
    })
    
    colVariables<-c()
    if(!all(is.na(ind_info$Edulevel))){
      colVariables<-c(colVariables,c("Education"="Education"))
      ind_info$Edulevel[is.na(ind_info$Edulevel)] <- "Unknown"
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

     pyramid_chart_server("py", df=pyramid_data,colAge="Age",colGender=c("Gender"="Gender"),colVariables=colVariables,mode="plot+table")

     sunVariables<-c(c("Gender"="Gender"),colVariables)
     
     sunburst_data<-ind_info%>%
       select(-DOB,-Regdate)%>%
       arrange(ID,Role,Site)%>%
       group_by(ID) %>%
       mutate(Role=paste0(unique(Role),collapse = "+"),
              Site=paste0(unique(Site),collapse = "+"))%>%
       ungroup()%>%
       distinct()%>%
       mutate(value=1)

     sunburst_chart_server("sb", df=sunburst_data,colVariables=sunVariables,colValue="value",mode="plot+table")
     
     pretty_table_server("pt", df=sunburst_data,colVariables=sunVariables,colValue="value")
     
  }
  
  )}