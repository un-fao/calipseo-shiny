#' @name artfish_line_chart_server
#' @aliases artfish_line_chart_server
#' @title artfish_line_chart_server
#' @description \code{artfish_line_chart_server} Server part of artfish_line_chart module
#'
#' @usage artfish_line_chart_server(id, df, colDate, colTarget, colValue, colText, xlab, ylab, rank, nbToShow, rankLabel)
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

artfish_line_chart_server <- function(id, df,colDate, colTarget,label=colTarget, colValue,colText=colTarget,xlab="Time",ylab="",levels=c("Global"="global","Detail"="detail"),stat="sum", rank=FALSE, nbToShow=5,rankLabel="Display x most caught:",plotType="line",mode="plot") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_formated<-reactiveVal(NULL)
    data_ready<-reactiveVal(FALSE)
    
    output$levels_selector<-renderUI({
      if(!is.null(levels)){
          selectInput(ns("levels"),"Level :",choices=levels)
      }
    })
    
    output$rank_params<-renderUI({
      req(!is.null(input$levels))
      
      if(isTRUE(rank)&input$levels=="detail"){
        max_nb<-length(unique(df[[colTarget]]))
        tagList(
          numericInput(ns("number"), rankLabel, value = if(max_nb<=5){max_nb}else{5}, min = 0, max = max_nb),
          selectInput(ns("rank_method"),"Rank method :",choices=c("Total catch over the period"="sum","Last year total catch"="last_year","Annual catch average"="year_avg"))
        )
      }else{
        NULL
      }
      
    })
    
    data_formating<-eventReactive(c(input$levels,input$number),{
  
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
          if(input$levels=="detail"){
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
        }
        
       # req(!is.null(input$levels))
        
        if(input$levels=="detail"){
          df<-df%>%
            mutate(value=value)%>%
            group_by(date,target,text)%>%
            summarise(agg=ifelse(stat=="mean",mean(value,na.rm=T),sum(value,na.rm=T)))%>%
            ungroup()
          
        }else{
          df<-df%>%
            mutate(value=value)%>%
            group_by(date)%>%
            summarise(agg=ifelse(stat=="mean",mean(value,na.rm=T),sum(value,na.rm=T)),text="",target="Global")%>%
            ungroup()
        }

        data_formated(df)
        data_ready(TRUE)
      }
    )
    
  
    output$plot<-renderPlotly({
        data_formating()
        
        if(isTRUE(data_ready())){
        
          p<-data_formated()%>%plot_ly(
            x = ~date
            
          )
          
          if(plotType=="line"){
              p<-p%>%    
               add_trace(type="scatter",mode="lines+markers",y =~ agg,color= ~target,line = list(simplyfy = F),text = ~sprintf("%s[%s]: %s",text,date,round(agg,2)))
            }else{
              p<-p%>%    
                add_bars(y =~ agg,color= ~target,line = list(simplyfy = F),text = ~sprintf("%s[%s]: %s",text,date,round(agg,2))) 
            }
          
                          
        p%>%layout(
            showlegend=ifelse(input$levels=="detail",T,F),
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
    
    output$table<-DT::renderDT(server = FALSE, {
    
    data_formating()
    
    if(isTRUE(data_ready())){
      
      DT::datatable(
        data_formated()%>%
          select(-text)%>%
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
            list(extend = 'csv', filename =  sprintf("%s_%s_statistics",label,input$levels), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  sprintf("%s_%s_statistics",label,input$levels), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',orientation = 'landscape',filename = sprintf("%s_%s_statistics",label,input$levels), 
            title = sprintf("Statistics by %s - %s", label,input$levels), header = TRUE)
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
