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
    output$individual_age_gender <- renderPlotly({
      
      age_gender <- accessIndividualDetails(pool)[,c("Gender","DOB")]
      
      age_gender <- age_gender[(age_gender$Gender=='Male' | age_gender$Gender=='Female') & !is.na(age_gender$DOB),]
      
      age_gender$DOB <- as.Date(age_gender$DOB)
      
      age_gender$Age = round(as.numeric(difftime(Sys.Date(),age_gender$DOB, units = "weeks"))/52.25, digits = 0)
      
      
      pyramid_df <- function(data, subset = NULL){
        
        data <- data[,-2]
        
        subset <- data[which(data$Gender == subset),names(data) %in% c("Gender","Age")]
        
        subset$Age <- as.factor(subset$Age)
        
        Pop_subset <- as.data.frame(table(subset$Age, dnn = c("Age")))
        
        df <- unique(left_join(Pop_subset,subset,  by = "Age"))
        
        return(df)
        
      }
      
      Male <- pyramid_df(age_gender, subset = 'Male')
      Female <- pyramid_df(age_gender, subset = 'Female')
      
      
      pyramid_df <- rbind(Female,Male)
      
      pyramid_df$Age <- as.numeric(as.character(pyramid_df$Age))
      
      
      pyramid_df %>% 
        mutate(number = ifelse(Gender == "Male", yes = -Freq, no = Freq))%>%
        mutate(abs_num = abs(number)) %>%
        plot_ly(x= ~number, y=~Age, color=~Gender,text = ~abs_num,colors = c('orange', '#1f77b4'),
                hovertemplate = paste("Age : %{y:}<br>","Individual(s) : %{text}")) %>% 
        add_bars(orientation = 'h') %>%
        layout(bargap = 0.1, barmode = 'overlay',
               xaxis = list(title = 'Number of individuals',tickmode = 'array', tickvals = c(-1000,-100,-90,-80,-70,-60,-50,-40,-30,-20, -10, -5, 0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000),
                            ticktext = c('1000','100','90','80','70','60','50','40','30','20', '10', '5', '0', '5', '10', '20', '30', '40', '50', '60', '70', '80', '90','100','1000')))
      
      
      
    })
    
    
    
    #individual breakdown by education level (pie chart)
    output$individual_edulevel <- renderPlotly({
      individual_breakdown_edulevel = accessIndividualCountByEdulevel(pool)
      
      plot_ly(individual_breakdown_edulevel, labels = ~NAME, values = ~COUNT, type = 'pie', sort = FALSE, direction = "clockwise") %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    
    
    
  }) 
}

