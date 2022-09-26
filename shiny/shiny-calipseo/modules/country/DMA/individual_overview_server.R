#individual_overview_server
individual_overview_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {  
    
    ind_overview <- accessIndividualOverview(pool)
    
    main_category <- function(data, column_index, column_name, gender_filter){
      
      data <- data[!is.na(data[,column_index]),]
      data <- as.data.frame(table(data[,column_index]))
      names(data) <-  c(paste(column_name),"Count")
      data$Per <- round(prop.table(data[,2])*100,digits = 1)
      if(column_name!='Gender'){
        dat <- data[order(data[, "Count"], decreasing = TRUE),c(1,3)]
        dat <- sprintf(paste(dat[,1],"(%s%s)"), dat[,2],"%")
        dat <- head(dat,1)
      }else{
        
        data$Per <- sprintf('(%s%s)',data$Per,'%')
        dat <- data[order(data[, "Count"], decreasing = TRUE),c(1,3)]
        dat <- dat[dat$Gender==gender_filter,]}
      return(dat)
    }
    
    fs_df <- fisher_nonfisher(ind_overview)
    non_fisher <- fs_df[fs_df$Category =='nonfisher',]
    fisher <- fs_df[fs_df$Category =='fisher',]
    Owner <- fs_df[fs_df$Category ==i18n("INDIVIDUAL_OVERVIEW_LABEL_OWNER"),]
    Captain <- fs_df[fs_df$Category ==i18n("INDIVIDUAL_OVERVIEW_LABEL_CAPTAIN"),]
    Fisher_ID <- fs_df[fs_df$Category == i18n("INDIVIDUAL_OVERVIEW_LABEL_HOLDER_FISHER_ID"),]
    Fisher_license <- fs_df[fs_df$Category == i18n("INDIVIDUAL_OVERVIEW_LABEL_HOLDER_FISHING_LICENSE"),]
    
    fisher_male <- fisher[fisher$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),]
    fisher_female <- fisher[fisher$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),]
    
    non_fisher_male <- non_fisher[non_fisher$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),]
    non_fisher_female <- non_fisher[non_fisher$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),]
    non_fisher_unspecified <- non_fisher[non_fisher$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"),]
    
    
    main_edu_owner <- main_category(Owner,column_index = 4, column_name = 'Edulevel')
    main_edu_captain <- main_category(Captain,column_index = 4, column_name = 'Edulevel')
    main_edu_fisher_id <- main_category(Fisher_ID,column_index = 4, column_name = 'Edulevel')
    main_edu_fishing_ls <- main_category(Fisher_license,column_index = 4, column_name = 'Edulevel')
    main_edu_fisher <- main_category(fisher,column_index = 4, column_name = 'Edulevel')
    main_edu_non_fisher <- main_category(non_fisher,column_index = 4, column_name = 'Edulevel')
    owner_per_male <- main_category(Owner,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"))$Per
    owner_per_female <- main_category(Owner,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"))$Per
    owner_per_unspecified <- main_category(Owner,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"))$Per
    captain_per_male <- main_category(Captain,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"))$Per
    captain_per_female <- main_category(Captain,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"))$Per
    captain_per_unspecified <- main_category(Captain,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"))$Per
    fisher_id_per_male <- main_category(Fisher_ID,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"))$Per
    fisher_id_per_female <- main_category(Fisher_ID,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"))$Per
    fisher_id_per_unspecified <- main_category(Fisher_ID,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"))$Per
    fishing_ls_per_male <- main_category(Fisher_license,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"))$Per
    fishing_ls_per_female <- main_category(Fisher_license,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"))$Per
    fishing_ls_per_unspecified <- main_category(Fisher_license,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"))$Per
    nonfisher_per_male <- main_category(non_fisher,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"))$Per
    nonfisher_per_female <- main_category(non_fisher,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"))$Per
    nonfisher_per_unspecified <- main_category(non_fisher,column_name = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"), column_index = 2, gender_filter = i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"))$Per
    
    
    age_fisher <- Age_comp(fisher[,c('Gender','DOB')], Prep = TRUE)
    age_non_fisher <- Age_comp(non_fisher[,c('Gender','DOB')], Prep = TRUE)
    age_owner <- Age_comp(Owner[,c('Gender','DOB')], Prep = TRUE)
    age_captain <- Age_comp(Captain[,c('Gender','DOB')], Prep = TRUE)
    age_fishing_ls <- Age_comp(Fisher_license[,c('Gender','DOB')], Prep = TRUE)
    age_fisher_id <- Age_comp(Fisher_ID[,c('Gender','DOB')], Prep = TRUE)
    
    
    Category_fishery_df <- data.frame(
      Category = c(i18n("INDIVIDUAL_OVERVIEW_LABEL_OWNER"), i18n("INDIVIDUAL_OVERVIEW_LABEL_CAPTAIN"), i18n("INDIVIDUAL_OVERVIEW_LABEL_HOLDER_FISHER_ID"), i18n("INDIVIDUAL_OVERVIEW_LABEL_HOLDER_FISHING_LICENSE"), i18n("INDIVIDUAL_OVERVIEW_LABEL_NON_FISHER")),
      Total_number = c(nrow(Owner), nrow(Captain), nrow(Fisher_ID), nrow(Fisher_license),nrow(non_fisher)),
      Number_male = c(paste(nrow(Owner[Owner$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),]),owner_per_male),paste(nrow(Captain[Captain$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),]),captain_per_male),paste(nrow(Fisher_ID[Fisher_ID$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),]),fisher_id_per_male),paste(nrow(Fisher_license[Fisher_license$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),]),fishing_ls_per_male),paste(nrow(non_fisher_male),nonfisher_per_male)),
      Number_female = c(paste(nrow(Owner[Owner$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),]),owner_per_female),paste(nrow(Captain[Captain$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),]),captain_per_female),paste(nrow(Fisher_ID[Fisher_ID$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),]),fisher_id_per_female),paste(nrow(Fisher_license[Fisher_license$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),]),fishing_ls_per_female),paste(nrow(non_fisher_male),nonfisher_per_female)),
      Number_gender_unknown = c(paste(nrow(Owner[Owner$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"),]),owner_per_unspecified),paste(nrow(Captain[Captain$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"),]),captain_per_unspecified),paste(nrow(Fisher_ID[Fisher_ID$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"),]),fisher_id_per_unspecified),paste(nrow(Fisher_license[Fisher_license$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_UNSPECIFIED"),]),fishing_ls_per_unspecified),paste(nrow(non_fisher_male),nonfisher_per_unspecified)),
      min_age = c(min(age_owner$Age),min(age_captain$Age),min(age_fisher_id$Age),min(age_fishing_ls$Age),min(age_non_fisher$Age)),
      max_age = c(max(age_owner$Age),max(age_captain$Age),max(age_fisher_id$Age),max(age_fishing_ls$Age),max(age_non_fisher$Age)),
      median_age = c(median(age_owner$Age),median(age_captain$Age),median(age_fisher_id$Age),median(age_fishing_ls$Age),median(age_non_fisher$Age)),
      main_edu =c(main_edu_owner,main_edu_captain,main_edu_fisher_id,main_edu_fishing_ls,main_edu_non_fisher)
    )
    
    
    
    individual_profile <- function(dash_title = NULL,dash_icon = NULL, total_number = NULL, 
                                   number_males = NULL, number_females = NULL, 
                                   min_age = NULL, median_age = NULL, max_age = NULL, 
                                   main_edu_level = NULL){
      
      div(class = 'col-md-6',
          box(width = 12,title = dash_title,
              div(class = 'row',CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_TOTAL_NUMBER"), 
                                                value = total_number, icon = dash_icon,infobox_extra_style = 'min-height:55px',
                                                infobox_icon_extra_style = 'height: 55px; line-height: 55px;',width = 12)),
              div(class = 'row',div(class = 'col-md-6', 
                                    CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_NUMBER_OF_MALES"), 
                                                    value = number_males,icon = icon('mars'),content_margin_left = 0,
                                                    width = 12,style_title = 'text-align:center;',infobox_icon_extra_style = 'height: 55px; line-height: 55px;',
                                                    style_value = 'text-align:center;',infobox_extra_style = 'min-height:55px')),
                  div(class = 'col-md-6', CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_NUMBER_OF_FEMALES"), 
                                                          value = number_females, icon = icon('venus'),content_margin_left = 0,
                                                          width = 12,style_title = 'text-align:center;',infobox_icon_extra_style = 'height: 55px; line-height: 55px;',
                                                          style_value = 'text-align:center;',infobox_extra_style = 'min-height:55px'))),
              div(class = 'row',div(class = 'col-md-4', 
                                    CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_MIN_AGE"), 
                                                    value = min_age, Use_icon = FALSE,content_margin_left = 0,
                                                    width = 12,infobox_extra_style = 'min-height:55px',
                                                    style_title = "font-size:90%;text-align:center;",
                                                    style_value = "font-weight:700px;text-align:center")),
                  div(class = 'col-md-4', CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_MEDIAN_AGE"),
                                                          value = median_age,  Use_icon = FALSE,content_margin_left = 0,
                                                          width = 12,infobox_extra_style = 'min-height:55px',
                                                          style_title = "font-size:90%;text-align:center;",
                                                          style_value = "font-weight:700px;text-align:center")),
                  div(class = 'col-md-4', CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_MAX_AGE"), 
                                                          value = max_age, Use_icon = FALSE,content_margin_left = 0,
                                                          width = 12,infobox_extra_style = 'min-height:55px',
                                                          style_title = "font-size:90%;text-align:center;",
                                                          style_value = "font-weight:700px;text-align:center"))),
              div(class = 'row',CalipseoInfoBox(title = i18n("INFOBOX_TITLE_INDIVIDUAL_OVERVIEW_MEAN_EDU_LEVEL"), 
                                                value = main_edu_level,icon = icon('user-graduate'),content_margin_left = 0,style_title = 'text-align:center;',
                                                style_value = 'text-align:center;' ,infobox_extra_style = 'min-height:55px',
                                                infobox_icon_extra_style = 'height: 55px; line-height: 55px;',width = 12))
              
          ))
      
      
    }
    
    
    
    output$individual_overview_info <- renderText({
      paste0("<h2>", i18n("INDIVIDUAL_OVERVIEW_TITLE")," <small>", i18n("INDIVIDUAL_OVERVIEW_SUBTITLE"))
      
    })
    
    
    output$descrip_title <- renderText({
      
      paste0(span(i18n("INDIVIDUAL_OVERVIEW_TITLE_INDIVIDUAL_PROFIL"),style='font-size: 18px;margin: 0;'))
      
    })
    
    
    output$fisher_dash <- renderUI({
      
      individual_profile(dash_title = i18n("INDIVIDUAL_OVERVIEW_TITLE_FISHER"),dash_icon = icon('fish'), total_number = nrow(fisher),number_males = nrow(fisher_male),
                         number_females = nrow(fisher_female), min_age = min(age_fisher$Age), median_age = median(age_fisher$Age), 
                         max_age = max(age_fisher$Age), main_edu_level = main_edu_fisher)#sprintf(paste(edu_fisher$Edulevel,"(%s%s)"), edu_fisher$Per,"%"))
      
    })
    
    
    output$nonfisher_dash <- renderUI({
      
      individual_profile(dash_title = i18n("INDIVIDUAL_OVERVIEW_TITLE_NONFISHER"),dash_icon = icon('user'), total_number = nrow(non_fisher),number_males = nrow(non_fisher_male),
                         number_females = nrow(non_fisher_female), min_age = min(age_non_fisher$Age), median_age = median(age_non_fisher$Age), 
                         max_age = max(age_non_fisher$Age), main_edu_level = main_edu_non_fisher)#sprintf(paste(edu_non_fisher$Edulevel,"(%s%s)"), edu_non_fisher$Per,"%"))
      
    })
    
    
    output$category_fishery_dt <- renderDataTable({
      
      names(Category_fishery_df) <- c(i18n("INDIVIDUAL_OVERVIEW_COLNAME_1"),i18n("INDIVIDUAL_OVERVIEW_COLNAME_2"),i18n("INDIVIDUAL_OVERVIEW_COLNAME_3"),
                                      i18n("INDIVIDUAL_OVERVIEW_COLNAME_4"),i18n("INDIVIDUAL_OVERVIEW_COLNAME_5"),i18n("INDIVIDUAL_OVERVIEW_COLNAME_6"),
                                      i18n("INDIVIDUAL_OVERVIEW_COLNAME_7"),i18n("INDIVIDUAL_OVERVIEW_COLNAME_8"),i18n("INDIVIDUAL_OVERVIEW_COLNAME_9"))
      
      datatable(Category_fishery_df,
                rownames = FALSE,
                extensions = c("Buttons"),
                filter = list(position = 'top', clear = FALSE),
                options = list(
                  autoWidth = TRUE,
                  dom = 'Bfrtip',
                  deferRender = TRUE,
                  scroll = FALSE,
                  buttons = list(
                    list(extend = 'copy'),
                    list(extend = 'csv', filename =  i18n("INDIVIDUAL_OVERVIEW_DATA_EXPORT_FILENAME"), title = NULL, header = TRUE),
                    list(extend = 'excel', filename =  i18n("INDIVIDUAL_OVERVIEW_DATA_EXPORT_FILENAME"), title = NULL, header = TRUE),
                    list(extend = "pdf", filename = i18n("INDIVIDUAL_OVERVIEW_DATA_EXPORT_FILENAME"),
                         title = i18n("INDIVIDUAL_OVERVIEW_DATA_EXPORT_FILENAME"), header = TRUE)
                  ),
                  exportOptions = list(
                    modifiers = list(page = "all", selected = TRUE)
                  ),
                  language = list(url = i18n("TABLE_LANGUAGE"))
                ))
      
      
    })
    
    
    
    output$fisher_age_gender <- renderPlotly({plot_df(fisher, fill = i18n("INDIVIDUAL_OVERVIEW_LABEL_EDULEVEL"))})
    
    output$non_fisher_age_gender <- renderPlotly({plot_df(non_fisher, fill = i18n("INDIVIDUAL_OVERVIEW_LABEL_EDULEVEL"))})
    
    
    
  }
  
  )}