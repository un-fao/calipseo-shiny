#individual_breakdown_server
individual_breakdown_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) { 
    
    fisher_nonfisher_df <- fisher_nonfisher(accessIndividualOverview(pool))
    
    fisher_nonfisher_df$Category <- as.factor(fisher_nonfisher_df$Category)
    
    non_fisher <- fisher_nonfisher_df[fisher_nonfisher_df$Category =='nonfisher',]
    
    
    
    output$fisher_breakdown_info <- renderText({
      text <- paste0("<h2>", i18n("INDIVIDUAL_BREAKDOWN_TITLE")," <small>", i18n("INDIVIDUAL_BREAKDOWN_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
    
    observe({
      
      observeEvent(input$fisher_chck,{if(isTRUE(input$fisher_chck))updateCheckboxInput(session,"nonfisher_chck",value = FALSE)})
      observeEvent(input$fisher_chck_edu,{if(isTRUE(input$fisher_chck_edu))updateCheckboxInput(session,"nonfisher_chck_edu",value = FALSE)})
      
      
      cat_selected <- function(data, column_index = NULL){
        
        cate <- data[!is.na(data[,column_index]),]
        
        cate <- cate %>% count(cate[,column_index])
        
        names(cate) <- c(i18n("INDIVIDUAL_LABEL_NAME"), i18n("INDIVIDUAL_LABEL_COUNT"))
        
        return(cate)
      }
      
      
      output$title_box_gender <- renderUI({
        if(isTRUE(input$fisher_chck)){
          
          sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PIECHART_GENDER"),i18n("INDIVIDUAL_BREAKDOWN_TITLE_FISHER"),input$filter_fisher_category,appConfig$country_profile$data$NAME)
        }else{
          title <- sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PIECHART_GENDER"),i18n("INDIVIDUAL_BREAKDOWN_TITLE_NONFISHER"),"",appConfig$country_profile$data$NAME) 
          
          title <- gsub("\\[|\\]", "", title)
          
          title
        }
        
      })
      
      
      output$title_box_pyramid <- renderUI({
        if(isTRUE(input$fisher_chck)){
          
          sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PYRAMID"),i18n("INDIVIDUAL_BREAKDOWN_TITLE_FISHER"),input$filter_fisher_category,appConfig$country_profile$data$NAME)
        }else{
          title <- sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PYRAMID"),i18n("INDIVIDUAL_BREAKDOWN_TITLE_NONFISHER"),"",appConfig$country_profile$data$NAME) 
          
          title <- gsub("\\[|\\]", "", title)
          
          title
        }
        
      })
      
      
      
      observeEvent({
        
        input$nonfisher_chck
        input$filter_fisher_category
        
      },
      {
        if(isFALSE(input$nonfisher_chck)){
          updateCheckboxInput(session,"fisher_chck",value = TRUE)
          category <- i18n("INDIVIDUAL_BREAKDOWN_LABEL_FISHER")
        }else {
          updateCheckboxInput(session,"fisher_chck",value = FALSE)
          category <- i18n("INDIVIDUAL_BREAKDOWN_LABEL_NONFISHER")
          
        }
        
        
        if(category==i18n("INDIVIDUAL_BREAKDOWN_LABEL_FISHER")){
          
          dat <- fisher_nonfisher_df %>% dplyr::filter(Category%in%input$filter_fisher_category)
          gender_col <- cat_selected(dat,column_index = 2)
          
          #fisher breakdown by gender (pie chart)
          output$fisher_gender <- renderPlotly({
            
            plot_ly(gender_col, labels = ~Name, values = ~Count, type = 'pie', sort = FALSE, direction = "clockwise") %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            
            
          })
          
          
          output$fisher_age_gender <- renderPlotly({plot_df(dat, fill = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"))})
          
          
        }else{
          
          #non fisher
          gender_col <- cat_selected(non_fisher,column_index = 2)
          
          #fisher breakdown by gender (pie chart)
          output$fisher_gender <- renderPlotly({
            
            plot_ly(gender_col, labels = ~Name, values = ~Count, type = 'pie', sort = FALSE, direction = "clockwise") %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            
          })
          
          output$fisher_age_gender <- renderPlotly({plot_df(non_fisher, fill = i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"))})
          
        }
        
        
      })
      
      
      
      
      
      
      observeEvent({
        
        input$nonfisher_chck_edu
        input$filter_fisher_category_edu
        
      },
      {
        if(isFALSE(input$nonfisher_chck_edu)){
          updateCheckboxInput(session,"fisher_chck_edu",value = TRUE)
          category <- i18n("INDIVIDUAL_BREAKDOWN_LABEL_FISHER")
        }else {
          updateCheckboxInput(session,"fisher_chck_edu",value = FALSE)
          category <- i18n("INDIVIDUAL_BREAKDOWN_LABEL_NONFISHER")
          
        }
        
        
        if(category==i18n("INDIVIDUAL_BREAKDOWN_LABEL_FISHER")){
          
          fisher_df_edu <- fisher_nonfisher_df %>% dplyr::filter(Category%in%input$filter_fisher_category_edu)
          edu_col <- cat_selected(fisher_df_edu,column_index = 4)
          
          
          #individual breakdown by education level (pie chart)
          output$individual_edulevel <- renderPlotly({
            
            plot_ly(edu_col, labels = ~Name, values = ~Count, type = 'pie', sort = FALSE, direction = "clockwise") %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     legend = list(x = 0.35, y = -0.5))
            
          })
          
          
          
          #individual breakdown by education level Male (pie chart)
          output$individual_edulevel_male <- renderPlotly({
            
            individual_breakdown_edulevel_male <- fisher_df_edu[fisher_df_edu$Gender==i18n("INDIVIDUAL_LABEL_MALE"),]
            individual_breakdown_edulevel_male <- cat_selected(individual_breakdown_edulevel_male,column_index = 4)
            
            plot_ly(individual_breakdown_edulevel_male, labels = ~Name, values = ~Count, type = 'pie', sort = FALSE, direction = "clockwise") %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     legend = list(x = 0.35, y = -0.5))
            
          })
          
          
          
          #individual breakdown by education level Female (pie chart)
          output$individual_edulevel_female <- renderPlotly({
            
            individual_breakdown_edulevel_female <- fisher_df_edu[fisher_df_edu$Gender==i18n("INDIVIDUAL_LABEL_FEMALE"),]
            individual_breakdown_edulevel_female <- cat_selected(individual_breakdown_edulevel_female,column_index = 4)
            
            plot_ly(individual_breakdown_edulevel_female, labels = ~Name, values = ~Count, type = 'pie', sort = FALSE, direction = "clockwise") %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     legend = list(x = 0.35, y = -0.5))
            
          })
          
        }else{
          
          
          #individual breakdown by education level (pie chart)
          output$individual_edulevel <- renderPlotly({
            non_fisher_edu_col <- cat_selected(non_fisher,column_index = 4)
            
            plot_ly(non_fisher_edu_col, labels = ~Name, values = ~Count, type = 'pie', sort = FALSE, direction = "clockwise") %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     legend = list(x = 0.35, y = -0.5))
            
          })
          
          
          #individual breakdown by education level Male (pie chart)
          output$individual_edulevel_male <- renderPlotly({
            individual_breakdown_edulevel_male <- non_fisher[non_fisher$Gender==i18n("INDIVIDUAL_LABEL_MALE"),]
            individual_breakdown_edulevel_male <- cat_selected(individual_breakdown_edulevel_male,column_index = 4)
            
            plot_ly(individual_breakdown_edulevel_male, labels = ~Name, values = ~Count, type = 'pie', sort = FALSE, direction = "clockwise") %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     legend = list(x = 0.35, y = -0.5))
            
          })
          
          
          
          #individual breakdown by education level Female (pie chart)
          output$individual_edulevel_female <- renderPlotly({
            individual_breakdown_edulevel_female <- non_fisher[non_fisher$Gender==i18n("INDIVIDUAL_LABEL_FEMALE"),]
            individual_breakdown_edulevel_female <- cat_selected(individual_breakdown_edulevel_female,column_index = 4)
            
            plot_ly(individual_breakdown_edulevel_female, labels = ~Name, values = ~Count, type = 'pie', sort = FALSE, direction = "clockwise") %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     legend = list(x = 0.35, y = -0.5))
            
          })
          
          
          
          
          
          
          
        }
        
        
      })
      
      
      
      
      
      
      
      
      
      
      
      
    })
    
    
    
    
    
  }) 
}

