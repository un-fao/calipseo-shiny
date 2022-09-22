#fisher_breakdown_server
fisher_breakdown_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) { 
    
    ind_overview <- accessIndividualOverview(pool)
    
    fisher <- ind_overview[ind_overview$Category=='fisher',]
    fisher$individualNumber <- as.factor(fisher$individualNumber)
    fisher <- distinct(fisher, individualNumber, .keep_all = TRUE)
   
    
    non_fisher <- ind_overview[ind_overview$Category =='nonfisher',]
    non_fisher$individualNumber <- as.factor(non_fisher$individualNumber)
    non_fisher <- dplyr::filter(non_fisher,!individualNumber%in%fisher$individualNumber)
    non_fisher <- distinct(non_fisher, individualNumber, .keep_all = TRUE)
   
    
    Owner <<- Category_fishery(fisher, code = 'OWN', category_name = i18n("FISHER_LABEL_OWNER"))
    Captain <- Category_fishery(fisher, code = 'CAP', category_name = i18n("FISHER_LABEL_CAPTAIN"))
    Fisher_ID <- Category_fishery(fisher, code = 'FIS', category_name = i18n("FISHER_LABEL_HOLDER_FISHING_ID"))
    Fisher_ID <- Fisher_ID[Fisher_ID$FSH_CODE!='CAP' & Fisher_ID$FSH_CODE!='OWN',]
    Fisher_license <- Category_fishery(fisher, category_name = i18n("FISHER_LABEL_HOLDER_FISHING_LICENSE"))
    Fisher_license <- Fisher_license[Fisher_license$FSH_CODE!='CAP' & Fisher_license$FSH_CODE!='OWN' & Fisher_license$FSH_CODE!='FIS',]
    Fisher_license <<- Fisher_license[!is.na(Fisher_license$Category),]
    
    
    
    output$fisher_breakdown_info <- renderText({
      text <- paste0("<h2>", i18n("FISHER_BREAKDOWN_TITLE")," <small>", i18n("FISHER_BREAKDOWN_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
  
    observe({
    
      observeEvent(input$fisher_chck,{if(isTRUE(input$fisher_chck))updateCheckboxInput(session,"nonfisher_chck",value = FALSE)})
      
   
      cat_selected <- function(data, column_index = NULL){
        
        cate <- data[!is.na(data[,column_index]),]
        
        cate <- cate %>% count(cate[,column_index])
        
        names(cate) <- c(i18n("FISHER_LABEL_NAME"), i18n("FISHER_LABEL_COUNT"))
        
        return(cate)
      }
      
      
      fisher_cate_name <- switch(input$filter_fisher_category,
                    "OWN"= i18n("FISHER_LABEL_OWNER"),
                    "CAP"= i18n("FISHER_LABEL_CAPTAIN"),
                    "FIS_ID"= i18n("FISHER_LABEL_HOLDER_FISHING_ID"),
                    "FIS_LS" = i18n("FISHER_LABEL_HOLDER_FISHING_LICENSE"))
      
      
      output$title_box_gender <- renderUI({
        if(isTRUE(input$fisher_chck)){
         
          sprintf(i18n("BREAKDOWN_FISHER_TITLE_PIECHART_GENDER"),i18n("FISHER_BREAKDOWN_TITLE_FISHER"),fisher_cate_name,appConfig$country_profile$data$NAME)
        }else{
          title <- sprintf(i18n("BREAKDOWN_FISHER_TITLE_PIECHART_GENDER"),i18n("FISHER_BREAKDOWN_TITLE_NONFISHER"),"",appConfig$country_profile$data$NAME) 
          
          title <- gsub("\\[|\\]", "", title)
          
          title
        }
       
      })
      
      
      output$title_box_pyramid <- renderUI({
        if(isTRUE(input$fisher_chck)){
          
          sprintf(i18n("BREAKDOWN_FISHER_TITLE_PYRAMID"),i18n("FISHER_BREAKDOWN_TITLE_FISHER"),fisher_cate_name,appConfig$country_profile$data$NAME)
        }else{
          title <- sprintf(i18n("BREAKDOWN_FISHER_TITLE_PYRAMID"),i18n("FISHER_BREAKDOWN_TITLE_NONFISHER"),"",appConfig$country_profile$data$NAME) 
          
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
          category <- i18n("FISHER_BREAKDOWN_LABEL_FISHER")
        }else {
          updateCheckboxInput(session,"fisher_chck",value = FALSE)
          category <- i18n("FISHER_BREAKDOWN_LABEL_NONFISHER")
          
        }
        
        
        if(category==i18n("FISHER_BREAKDOWN_LABEL_FISHER")){
          
          #Captain
          if(input$filter_fisher_category=="CAP"){
            
           
           gender_col <- cat_selected(Captain,column_index = 2)
           
           dat <- Captain
           
            
          #fisher ID 
          }else if(input$filter_fisher_category=="FIS_ID"){
            
           gender_col <- cat_selected(Fisher_ID,column_index = 2)
           
           dat <- Fisher_ID
          
          #Owner  
          }else if(input$filter_fisher_category=="OWN"){
            
          gender_col <- cat_selected(Owner,column_index = 2)
          
          dat <- Owner
          
           
          #fishing license 
          }else if(input$filter_fisher_category=="FIS_LS"){
            
          gender_col <- cat_selected(Fisher_license,column_index = 2)
          
          dat <- Fisher_license
          
            
          }
          

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
      
     
      
    })
    
 
   

    
  }) 
}

