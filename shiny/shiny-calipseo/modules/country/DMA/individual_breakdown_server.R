#individual_breakdown_server
individual_breakdown_server <- function(id, pool) {
  
  moduleServer(id, function(input, output, session) {  
    
    output$individual_breakdown_info <- renderText({
      text <- paste0("<h2>", i18n("INDIVIDUAL_BREAKDOWN_TITLE")," <small>", i18n("INDIVIDUAL_BREAKDOWN_SUBTITLE"),"</small></h2><hr>")
      text
    })
    
    
    
    #individual breakdown by gender (pie chart)
    output$individual_gender <- renderPlotly({
      individual_breakdown = accessIndividualCountByGender(pool)
      
      plot_ly(individual_breakdown, labels = ~NAME, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    
    #individual breakdown by age and gender (pyramid)
    age_gender <- accessIndividualDetails(pool)[,c("Gender","DOB")]
    
    age_gender <- Age_comp(age_gender, Prep = TRUE)
    
    pyramid_df <- function(data, subset = NULL){
      
      data <- data[,-2]
      
      subset <- data[which(data$Gender == subset),names(data) %in% c("Gender","Age")]
      
      subset$Age <- as.factor(subset$Age)
      
      Pop_subset <- as.data.frame(table(subset$Age, dnn = c("Age")))
      
      df <- unique(left_join(Pop_subset,subset,  by = "Age"))
      
      return(df)
      
    }
    
    
    
    output$individual_age_gender <- renderPlotly({
      
      
      Male <- pyramid_df(age_gender, subset = 'Male')
      Female <- pyramid_df(age_gender, subset = 'Female')
      
      
      pyramid_df <- rbind(Female,Male)
      
      pyramid_df$Age <- as.numeric(as.character(pyramid_df$Age))
      
      
      pyramid_df %>% 
        mutate(number = ifelse(Gender == "Male", yes = -Freq, no = Freq))%>%
        mutate(abs_num = abs(number)) %>%
        plot_ly(x= ~number, y=~Age, color=~Gender,text = ~abs_num,colors = c('orange', '#1f77b4'),
                hovertemplate = paste(i18n("AGE_LABEL"),": %{y:}<br>",i18n("INDIVIDUALS_LABEL"),": %{text}")) %>% 
        add_bars(orientation = 'h') %>%
        layout(bargap = 0.1, barmode = 'overlay',
               xaxis = list(title = i18n("NUMBER_OF_INDIVIDUALS_TITLE"),tickmode = 'array', tickvals = c(-1000,-100,-90,-80,-70,-60,-50,-40,-30,-20, -10, -5, 0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000),
                            ticktext = c('1000','100','90','80','70','60','50','40','30','20', '10', '5', '0', '5', '10', '20', '30', '40', '50', '60', '70', '80', '90','100','1000')),
               yaxis = list(title = i18n("AGE_LABEL")))
      
    })

    male_df <- pyramid_df(age_gender, subset = 'Male')
    male_df$Age <- as.numeric(male_df$Age)
    female_df <- pyramid_df(age_gender, subset = 'Female')
    female_df$Age <- as.numeric(female_df$Age)
    
    
    num_males <- nrow(male_df)
    num_females <- nrow(female_df)
    total_all <- num_males+num_females
    age_sum_male <- sum(male_df[,'Age'], na.rm = TRUE)
    age_sum_female <- sum(female_df[,'Age'], na.rm = TRUE)
    age_sum_all <- age_sum_male+age_sum_female
    mean_age_male <- age_sum_male/num_males
    mean_age_female <- age_sum_female/num_females
    mean_age_all <- age_sum_all/total_all
    
    
    output$total_mean_age <- renderText({
      
      sprintf(paste0(i18n("MEAN_AGE_TOTAL"),": %s"), round(mean_age_all, digits = 3))
      
    })
    
    
    output$male_mean_age <- renderText({
      
      sprintf(paste0(i18n("MEAN_AGE_MALE"),": %s"), round(mean_age_male, digits = 3))
      
    })
    
    
    output$female_mean_age <- renderText({
      
      sprintf(paste0(i18n("MEAN_AGE_FEMALE"),": %s"), round(mean_age_female, digits = 3))
      
    })
    
    
    
    #individual breakdown by education level (pie chart)
    output$individual_edulevel <- renderPlotly({
      individual_breakdown_edulevel = accessIndividualCountByEdulevel(pool)
      
      plot_ly(individual_breakdown_edulevel, labels = ~NAME, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    
    #individual breakdown by education level (pie chart)
    output$individual_edulevel <- renderPlotly({
      individual_breakdown_edulevel = accessIndividualCountByEdulevel(pool, gender_id = 'All')
      
      plot_ly(individual_breakdown_edulevel, labels = ~NAME, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(x = 0.35, y = -0.5))
      
    })
    
    
    
    #individual breakdown by education level Male (pie chart)
    output$individual_edulevel_male <- renderPlotly({
      individual_breakdown_edulevel_male = accessIndividualCountByEdulevel(pool, gender_id = 1)
      
      plot_ly(individual_breakdown_edulevel_male, labels = ~NAME, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(x = 0.35, y = -0.5))
      
    })
    
    
    
    #individual breakdown by education level Female (pie chart)
    output$individual_edulevel_female <- renderPlotly({
      individual_breakdown_edulevel_female = accessIndividualCountByEdulevel(pool, gender_id = 2)
      
      plot_ly(individual_breakdown_edulevel_female, labels = ~NAME, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(x = 0.35, y = -0.5))
      
    })
    
    
    
    
  }) 
}

