#individual_overview_server
individual_overview_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {  
    
    ns <- session$ns
    
    ind_info <- accessIndividualInfo(pool)

    # individual_profile <- function(dash_title = NULL,dash_icon = NULL, total_number = NULL,
    #                                number_males = NULL, number_females = NULL,
    #                                min_age = NULL, median_age = NULL, max_age = NULL,
    #                                main_edu_level = NULL){
    #   
    #   div(class = 'col-md-6',
    #       box(width = 12,title = dash_title,
    #           div(class = 'row',CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_TOTAL_NUMBER"),
    #                                             value = total_number, icon = dash_icon,infobox_extra_style = 'min-height:55px',
    #                                             infobox_icon_extra_style = 'height: 55px; line-height: 55px;',width = 12)),
    #           div(class = 'row',div(class = 'col-md-6',
    #                                 CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_NUMBER_OF_MALES"),
    #                                                 value = number_males,icon = icon('mars'),content_margin_left = 0,
    #                                                 width = 12,style_title = 'text-align:center;',infobox_icon_extra_style = 'height: 55px; line-height: 55px;',
    #                                                 style_value = 'text-align:center;',infobox_extra_style = 'min-height:55px')),
    #               div(class = 'col-md-6', CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_NUMBER_OF_FEMALES"),
    #                                                       value = number_females, icon = icon('venus'),content_margin_left = 0,
    #                                                       width = 12,style_title = 'text-align:center;',infobox_icon_extra_style = 'height: 55px; line-height: 55px;',
    #                                                       style_value = 'text-align:center;',infobox_extra_style = 'min-height:55px'))),
    #           div(class = 'row',div(class = 'col-md-4',
    #                                 CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_MIN_AGE"),
    #                                                 value = min_age, Use_icon = FALSE,content_margin_left = 0,
    #                                                 width = 12,infobox_extra_style = 'min-height:55px',
    #                                                 style_title = "font-size:90%;text-align:center;",
    #                                                 style_value = "font-weight:700px;text-align:center")),
    #               div(class = 'col-md-4', CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_MEDIAN_AGE"),
    #                                                       value = median_age,  Use_icon = FALSE,content_margin_left = 0,
    #                                                       width = 12,infobox_extra_style = 'min-height:55px',
    #                                                       style_title = "font-size:90%;text-align:center;",
    #                                                       style_value = "font-weight:700px;text-align:center")),
    #               div(class = 'col-md-4', CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_MAX_AGE"),
    #                                                       value = max_age, Use_icon = FALSE,content_margin_left = 0,
    #                                                       width = 12,infobox_extra_style = 'min-height:55px',
    #                                                       style_title = "font-size:90%;text-align:center;",
    #                                                       style_value = "font-weight:700px;text-align:center"))),
    #           div(class = 'row',CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_MEAN_EDU_LEVEL"),
    #                                             value = main_edu_level,icon = icon('user-graduate'),content_margin_left = 0,style_title = 'text-align:center;',
    #                                             style_value = 'text-align:center;' ,infobox_extra_style = 'min-height:55px',
    #                                             infobox_icon_extra_style = 'height: 55px; line-height: 55px;',width = 12))
    #           
    #       ))
    #   
    #   
    # }
    # 
    # 
    # 
     output$individual_overview_info <- renderText({
       paste0("<h2>", i18n("INDIVIDUAL_OVERVIEW_TITLE")," <small>", i18n("INDIVIDUAL_OVERVIEW_SUBTITLE"))
       
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