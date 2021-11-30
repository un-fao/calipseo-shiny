line_chart_server <- function(id, df, colTarget, colValue,colText=colTarget,xlab="Time",ylab="Quantity(tons)", rank=FALSE, nbToShow=5,rankLabel="Display x most catched:") {
#  line_chart_server <- function(input, output, session, df, target, value, rank=FALSE, nbToShow=5) {  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$rank_params<-renderUI({
      
      if(isTRUE(rank)){
        max_nb<-length(unique(df[[colTarget]]))
        tagList(
          numericInput(ns("number"), rankLabel, value = if(max_nb<=5){max_nb}else{5}, min = 0, max = max_nb),
          selectInput(ns("rank_method"),"Rank method :",choices=c("Total catch over the period"="sum","Last year total catch"="last_year","Annual catch average"="year_avg"))
        )
      }else{
        NULL
      }
      
    })
    
    observeEvent(input$stat,{
      output$additional<-renderUI({
        if(input$stat=="mean"){
          checkboxInput(ns("withsd"),"standard deviation", value = FALSE)
        }else if(input$stat=="median"){
          checkboxInput(ns("withquartile"),"quartiles", value = FALSE)
        }else{
          NULL
        }
      })
    })
    
    observeEvent(c(input$stat,input$granu,input$number),{
      
      output$plot<-renderPlotly({
        
        df<-df%>%
          rename(setNames(colTarget,"target"))%>%
          rename(setNames(colValue,"value"))
        
        if(colTarget==colText){
          df<-df%>%
            mutate(text=target)
        }else{
          df<-df%>%
            rename(setNames(colText,"text"))
        }
                          
        if(isTRUE(rank)){
          req(input$number)
          if(input$rank_method=="sum"){
            ranked <- df %>%
            group_by(target) %>% 
            summarise(total = sum(value))%>%
            mutate(rank = rank(-total)) %>%
            filter(rank <=as.numeric(input$number)) %>%
            arrange(rank)%>%
            pull(target)
          }	
                            
          if(input$rank_method=="year_avg"){
            ranked <- df %>%
            group_by(year,target) %>% 
            summarise(total = sum(value))%>%
            group_by(target)%>%
            summarise(avg = mean(total))%>%
            mutate(rank = rank(-avg)) %>%
            filter(rank <=as.numeric(input$number)) %>%
            arrange(rank)%>%
            pull(target)
          }
                              
          if(input$rank_method=="last_year"){
            ranked <- df %>%
            filter(year==max(year))%>%
            group_by(target) %>% 
            summarise(total = sum(value))%>%
            mutate(rank = rank(-total)) %>%
            filter(rank <=as.numeric(input$number)) %>%
            arrange(rank)%>%
            pull(target)
          }
                            
          df<-df%>%
            filter(target%in%ranked)
        }
                        
        p<-df%>%
          mutate(date = as.character(format(as.Date(date),format = input$granu)))%>%
          mutate(quantity=value/1000)%>%
          group_by(date,target,text,trip_id)%>%
          summarise(sum_by_trip = sum(value))%>%
          group_by(date,target,text)%>%
          summarise(
            sum = sum(sum_by_trip, na.rm = TRUE),
            mean = mean(sum_by_trip, na.rm = TRUE),
            min = min(sum_by_trip, na.rm = TRUE),
            max = max(sum_by_trip, na.rm = TRUE),
            sd = sd(sum_by_trip, na.rm = TRUE),
            q1 = quantile(sum_by_trip, probs = 0.25, na.rm = TRUE, names = FALSE),
            median = median(sum_by_trip, na.rm = TRUE),
            q3 = quantile(sum_by_trip, probs = 0.75, na.rm = TRUE, names = FALSE)
          )%>%
          mutate(target=as.factor(target))%>%
          mutate(sd = ifelse(is.na(sd), 0, sd))%>%
          ungroup()%>%
          plot_ly(
            x = ~date
          )
                          
          if(isTRUE(input$withquartile)&input$stat=="median"){
            p<-p%>%add_boxplot(x = ~date,color= ~target,type = "box", q1=~ q1, median=~ median,q3=~ q3, mean=~ mean,lowerfence=~ min,upperfence=~ max)
          }else{
            p<-p%>%    
             add_lines(y =~ get(input$stat),color= ~target,line = list(simplyfy = F),text = ~sprintf("%s[%s]: %s tons",text,date,round(get(input$stat),2)))
          }
                          
          if(isTRUE(input$withsd)&input$stat=="mean"){
            p<-p%>%add_ribbons(color= ~target,
                               ymin = ~ get(input$stat)-sd,
                               ymax = ~ get(input$stat)+sd,
                               showlegend=F,
                               opacity = 0.3,
                               line = list(dash="dash"))
          }
                          
          p%>%layout(
            showlegend=T,
            hovermode ='closest',
            xaxis = list(
              titlefont = list(size = 10), 
              tickfont = list(size = 10),
              title = xlab,
              zeroline = F
            ),
            yaxis = list(
              titlefont = list(size = 10), 
              tickfont = list(size = 10),
              title = ylab,
              zeroline = F
            )
          )
      })
    })  
        
        
  })
      
}