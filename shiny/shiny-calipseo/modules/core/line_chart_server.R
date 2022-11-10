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
#' @param valueUnit value unit
#' @param rank boolean argument, if TRUE slider include capacity to filter target value by rank level of value
#' @param nbToShow numeric, only use if rank=TRUE, indicate number of ranked value to display
#' @param rankLabel character string to specify rank label name
#' @param plotType type of maine trace type : 'line' or 'bar'
#' @param mode indicate mode to display result, 4 modes available ,'plot','table','plot+table','table+plot'
#'    

line_chart_server <- function(id, df,colDate, colTarget,label=colTarget, colValue,colText=colTarget,xlab=i18n("PLOT_XLAB"),ylab=i18n("PLOT_YLAB"),valueUnit=i18n("CATCH_UNIT"), rank=FALSE, nbToShow=5,rankLabel=paste0(i18n("RANK_LABEL"),":"),plotType="line",mode="plot") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
   
    data_formated<-reactiveVal(NULL)
    data_ready<-reactiveVal(FALSE)

    getDateFormat <- function(granu){
      if(granu==i18n("YEARLY")){
        "%Y"
      }else if(granu==i18n("MONTHLY")){
        "%Y-%m"
      }else if(granu==i18n("WEEKLY")){
        "%Y-%U"
      }
    }
    
    getStatOutput <- function(stat){
      if(stat==i18n("TOTAL")){
        "sum"
      }else if(stat==i18n("AVERAGE")){
        "mean"
      }else if(stat==i18n("MEDIAN")){
        "median"
      }
    }
    
    rank_method_choices <- c(i18n("TOTAL_CATCH_OVER_THE_PERIOD"),i18n("LAST_YEAR_TOTAL_CATCH"),i18n("ANNUAL_CATCH_AVERAGE"))
    witherror_choices <- c(i18n("NONE"),i18n("STANDARD_DEVIATION"),i18n("STANDARD_ERROR"),
                           i18n("CONFIDENCE_INTERVAL_QUANTILE_METHOD"),
                           i18n("CONFIDENCE_INTERVAL_NORMAL_DISTRIBUTION"),
                           i18n("CONFIDENCE_INTERVAL_T_DISTRIBUTION"))
    
    output$rank_params<-renderUI({
      
      if(isTRUE(rank)){
        max_nb<-length(unique(df[[colTarget]]))
        tagList(
          numericInput(ns("number"), rankLabel, value = if(max_nb<=5){max_nb}else{5}, min = 0, max = max_nb),
          selectInput(ns("rank_method"),i18n("RANK_METHOD"),choices=rank_method_choices)
        )
      }else{
        NULL
      }
      
    })
    
    observeEvent(input$stat,{
      output$additional<-renderUI({
        stat_output <- getStatOutput(input$stat)
        if(stat_output=="mean"){
          selectInput(ns("witherror"),paste0(i18n("PROJECTION_VARIATION")," :"),choices= witherror_choices)
          
        }else if(stat_output=="median"){
          checkboxInput(ns("withquartile"),i18n("QUARTILES"), value = FALSE)
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
          
          rank_method_output <- if(input$rank_method==i18n("TOTAL_CATCH_OVER_THE_PERIOD")){
            "sum"
          }else if(input$rank_method==i18n("LAST_YEAR_TOTAL_CATCH")){
            "last_year"
          }else if(input$rank_method==i18n("ANNUAL_CATCH_AVERAGE")){
            "year_avg"
          }
          
          
          if(rank_method_output=="sum"){
            ranked <- df %>%
              group_by(target) %>% 
              summarise(total = sum(value))%>%
              mutate(rank = rank(-total)) %>%
              filter(rank <=as.numeric(input$number)) %>%
              arrange(rank)%>%
              pull(target)
          }	
          
          if(rank_method_output=="year_avg"){
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
          
          if(rank_method_output=="last_year"){
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
          mutate(date = as.character(format(as.Date(date),format = getDateFormat(input$granu))))%>%
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
          
          stat_output <- getStatOutput(input$stat)
                          
          if(isTRUE(input$withquartile) & stat_output=="median"){
            p<-p%>%add_boxplot(x = ~date,color= ~target,type = "box", q1=~ q1, median=~ median,q3=~ q3, mean=~ mean,lowerfence=~ min,upperfence=~ max)
          }else{
            if(plotType=="line"){
              p<-p%>%    
               add_trace(type="scatter",mode="lines+markers",y =~ get(stat_output),color= ~target,line = list(simplyfy = F),legendgroup = ~target,text = ~sprintf(paste("%s[%s]: %s",valueUnit),text,date,round(get(stat_output),2)))
            }else{
              p<-p%>%    
                add_bars(y =~ get(stat_output),color= ~target,line = list(simplyfy = F),legendgroup = ~target,text = ~sprintf(paste("%s[%s]: %s",valueUnit),text,date,round(get(stat_output),2))) 
            }
          }
                          
          if(!is.null(input$witherror)){
            
            witherror_output <- if(input$witherror==i18n("NONE")){
              "none"
            }else if(input$witherror==i18n("STANDARD_DEVIATION")){
              "sd"
            }else if(input$witherror==i18n("STANDARD_ERROR")){
              "se"
            }else if(input$witherror==i18n("CONFIDENCE_INTERVAL_QUANTILE_METHOD")){
              "ci-q"
            }else if(input$witherror==i18n("CONFIDENCE_INTERVAL_NORMAL_DISTRIBUTION")){
              "ci-n"
            }else if(input$witherror==i18n("CONFIDENCE_INTERVAL_T_DISTRIBUTION")){
              "ci-t"
            }
            
            if(witherror_output!="none"&stat_output=="mean"){

              p<-p%>%add_ribbons(color= ~target,
                                 ymin = ~ switch(witherror_output,
                                   "sd"= {get(stat_output)-sd},
                                   "se"= {get(stat_output)-se},
                                   "ci-n"={get(stat_output)-ci_norm_coef},
                                   "ci-t"={get(stat_output)-ci_stud_coef},
                                   "ci-q"={q025}),
                                 ymax = ~ switch(witherror_output,
                                   "sd"= {get(stat_output)+sd},
                                   "se"= {get(stat_output)+se},
                                   "ci-n"={get(stat_output)+ci_norm_coef},
                                   "ci-t"={get(stat_output)+ci_stud_coef},
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
     
      granu<-switch(getDateFormat(input$granu),"%Y"=i18n("GRANU_LABEL_YEAR"),
                    "%Y-%m"=i18n("GRANU_LABEL_MONTH"),
                    "%Y-%U"=i18n("GRANU_LABEL_WEEK"))
      
      df <- data_formated()%>%
        select(-text)%>%
        rename(!!granu:=date)%>%
        rename(!!label:=target)%>%
        mutate(!!label:=as.factor(!!sym(label)))
      
      colnames(df)[4] <- i18n("STATISTIC_TABLE_COLNAME_4")
      colnames(df)[5] <- i18n("STATISTIC_TABLE_COLNAME_5")
      colnames(df)[6] <- i18n("STATISTIC_TABLE_COLNAME_6")
      
     
      DT::datatable(
        df,
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
            list(extend = 'csv', filename =  sprintf(i18n("STATISTIC_DATA_EXPORT_FILENAME"),label,granu), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  sprintf(i18n("STATISTIC_DATA_EXPORT_FILENAME"),label,granu), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',orientation = 'landscape',filename = sprintf(i18n("STATISTIC_DATA_EXPORT_FILENAME"),label,granu), 
            title = sprintf(i18n("STATISTIC_PDF_TITLE"), label,granu), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          ),
          language = list(url = i18n("STATISTIC_TABLE_LANGUAGE"))
        )
      )
    }
    
    })
  
  output$result<-renderUI({
      switch(mode,
        'plot+table'={
          tabsetPanel(
            tabPanel(i18n("TABPANEL_PLOT"),plotlyOutput(ns("plot"))%>%withSpinner(type = 4)),
            tabPanel(i18n("TABPANEL_STATISTIC"),DTOutput(ns("table"))%>%withSpinner(type = 4))
          )
        },
        'table+plot'={
          tabsetPanel(
            tabPanel(i18n("TABPANEL_STATISTIC"),DTOutput(ns("table"))%>%withSpinner(type = 4)),
            tabPanel(i18n("TABPANEL_PLOT"),plotlyOutput(ns("plot"))%>%withSpinner(type = 4))
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
