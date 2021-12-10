#' @name line_chart_server
#' @aliases line_chart_server
#' @title line_chart_server
#' @description \code{line_chart_server} Server part of line_chart module
#'
#' @usage line_chart_server(id, df, colDate, colTarget, colValue, colText, xlab, ylab, rank, nbToShow, rankLabel)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param df dataframe 
#' @param label label use to target column
#' @param colDate column name of date variable 
#' @param colTarget column name of variable of interest 
#' @param colValue column name of value
#' @param colText column name of variable use to marker label
#' @param xlab character string to specify x label name
#' @param ylab character string to specify y label name
#' @param rank boolean argument, if TRUE slider include capacity to filter target value by rank level of value
#' @param nbToShow numeric, only use if rank=TRUE, indicate number of ranked value to display
#' @param rankLabel character string to specify rank label name
#' @param plotType type of maine trace type : 'line' or 'bar'
#' @param mode indicate mode to display result, 4 modes available ,'plot','table','plot+table','table+plot'
#'    

line_chart_server <- function(id, df,colDate, colTarget,label=colTarget, colValue,colText=colTarget,xlab="Time",ylab="Quantity (tons)", rank=FALSE, nbToShow=5,rankLabel="Display x most caught:",plotType="line",mode="plot") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_formated<-reactiveVal(NULL)
    data_ready<-reactiveVal(FALSE)
    
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
          #checkboxInput(ns("withsd"),"standard deviation", value = FALSE)
          selectInput(ns("witherror"),"Project variation :",choices=c("None"="none","Standard Deviation (SD)"="sd",
                                                                      "Standard Error (SE)"="se",
                                                                      "Confidence Interval (CI) - quantile method"="ci-q",
                                                                      "Confidence Interval (CI) - normal distribution "="ci-n",
                                                                      "Confidence Interval (CI) - t distribution"="ci-t"
                                                                      ))
        }else if(input$stat=="median"){
          checkboxInput(ns("withquartile"),"quartiles", value = FALSE)
        }else{
          NULL
        }
      })
    })
    
    data_formating<-eventReactive(c(input$stat,input$granu,input$number),{
        df<-df%>%
          rename(setNames(colDate,"date"))%>%
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
              mutate(year = as.character(format(as.Date(date),format = '%Y')))%>%
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
              mutate(year = as.character(format(as.Date(date),format = '%Y')))%>%
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
        
        df<-df%>%
          mutate(date = as.character(format(as.Date(date),format = input$granu)))%>%
          mutate(value=value/1000)%>%
          group_by(date,target,text,trip_id)%>%
          summarise(sum_by_trip = sum(value))%>%
          group_by(date,target,text)%>%
          summarise(
            n = length(unique(trip_id)),
            sum = round(sum(sum_by_trip, na.rm = TRUE),2),
            mean = round(mean(sum_by_trip, na.rm = TRUE),2),
            median = round(median(sum_by_trip, na.rm = TRUE),2),
            min = round(min(sum_by_trip, na.rm = TRUE),2),
            max = round(max(sum_by_trip, na.rm = TRUE),2),
            sd = round(sd(sum_by_trip, na.rm = TRUE),2),
            se = round(sd/sqrt(n),2),
            q1 = round(quantile(sum_by_trip, probs = 0.25, na.rm = TRUE, names = FALSE),2),
            q3 = round(quantile(sum_by_trip, probs = 0.75, na.rm = TRUE, names = FALSE),2),
            ci_norm_coef = round(qnorm(.975)*se,2),
            ci_stud_coef = round(qt(.975, df = n - 1) * se,2),
            q025=round(quantile(sum_by_trip, probs = 0.025, na.rm = TRUE, names = FALSE),2),
            q975=round(quantile(sum_by_trip, probs = 0.975, na.rm = TRUE, names = FALSE),2)
          )%>%
          mutate(target=as.factor(target))%>%
          mutate(sd = ifelse(is.na(sd), 0.00, sd),
                 se = ifelse(is.na(se), 0.00, se),
                 ci_norm_coef = ifelse(is.na(ci_norm_coef), 0.00, ci_norm_coef),
                 ci_stud_coef = ifelse(is.na(ci_stud_coef), 0.00, ci_stud_coef)
                 )%>%
          ungroup()
        
        data_formated(df)
        data_ready(TRUE)
      }
    )
    
    #observeEvent(c(input$stat,input$granu,input$number),{
      output$plot<-renderPlotly({
        data_formating()
        
        if(isTRUE(data_ready())){
        
          p<-data_formated()%>%plot_ly(
            x = ~date
            
          )
                          
          if(isTRUE(input$withquartile)&input$stat=="median"){
            p<-p%>%add_boxplot(x = ~date,color= ~target,type = "box", q1=~ q1, median=~ median,q3=~ q3, mean=~ mean,lowerfence=~ min,upperfence=~ max)
          }else{
            if(plotType=="line"){
              p<-p%>%    
               add_trace(type="scatter",mode="lines+markers",y =~ get(input$stat),color= ~target,line = list(simplyfy = F),legendgroup = ~target,text = ~sprintf("%s[%s]: %s tons",text,date,round(get(input$stat),2)))
            }else{
              p<-p%>%    
                add_bars(y =~ get(input$stat),color= ~target,line = list(simplyfy = F),legendgroup = ~target,text = ~sprintf("%s[%s]: %s tons",text,date,round(get(input$stat),2))) 
            }
          }
                          
          if(!is.null(input$witherror)){
            if(input$witherror!="none"&input$stat=="mean"){

              p<-p%>%add_ribbons(color= ~target,
                                 ymin = ~ switch(input$witherror,
                                   "sd"= {get(input$stat)-sd},
                                   "se"= {get(input$stat)-se},
                                   "ci-n"={get(input$stat)-ci_norm_coef},
                                   "ci-t"={get(input$stat)-ci_stud_coef},
                                   "ci-q"={q025}),
                                 ymax = ~ switch(input$witherror,
                                   "sd"= {get(input$stat)+sd},
                                   "se"= {get(input$stat)+se},
                                   "ci-n"={get(input$stat)+ci_norm_coef},
                                   "ci-t"={get(input$stat)+ci_stud_coef},
                                   "ci-q"={q975}),
                                 showlegend=F,
                                 legendgroup = ~target,
                                 opacity = 0.3,
                                 line = list(dash="dash"))
            }
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
        }
      })
#  })
  
  output$table<-DT::renderDT(server = FALSE, {
    
    data_formating()
    
    if(isTRUE(data_ready())){
      
      granu<-switch(input$granu,"%Y"="Year",
                    "%Y-%m"="Month",
                    "%Y-%U"="Week")
      
      DT::datatable(
        data_formated()%>%
          select(-text)%>%
          rename(!!granu:=date)%>%
                   rename(!!label:=target)%>%
        mutate(!!label:=as.factor(!!sym(label))),
        extensions = c("Buttons"),
        escape = FALSE,
        filter = list(position = 'top',clear =FALSE),
        options = list(
          dom = 'Bfrtip',
          scrollX=TRUE,
          pageLength=5,
          orientation ='landscape',
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  sprintf("%s_%s_statistics",label,granu), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  sprintf("%s_%s_statistics",label,granu), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',orientation = 'landscape',filename = sprintf("%s_%s_statistics",label,granu), 
            title = sprintf("Statistics by %s - %s", label,granu), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          )
        )
      )
    }
    
    })
  
  output$result<-renderUI({
      switch(mode,
        'plot+table'={
          tabsetPanel(
            tabPanel("Plot",plotlyOutput(ns("plot"))%>%withSpinner(type = 4)),
            tabPanel('Statistics',DTOutput(ns("table"))%>%withSpinner(type = 4))
          )
        },
        'table+plot'={
          tabsetPanel(
            tabPanel('Statistics',DTOutput(ns("table"))%>%withSpinner(type = 4)),
            tabPanel("Plot",plotlyOutput(ns("plot"))%>%withSpinner(type = 4))
          )
        },
        'plot'={
          plotlyOutput(ns("plot"))%>%withSpinner(type = 4)
        },
        'table'={
          DTOutput(ns("table"))%>%withSpinner(type = 4)
        }
      )
  })
  
  })  
}
