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

artfish_line_chart_server <- function(id, df,colDate, colTarget,label=colTarget, colValue,colText=colTarget,xlab=i18n("X_LABEL_TITLE"),ylab="",levels=c(i18n("LEVEL_LABLE_GLOBAL"),i18n("LEVEL_LABLE_DETAIL")),stat="sum", rank=FALSE, nbToShow=5,rankLabel=paste0(i18n("RANK_LABEL"),":"),plotType="line",mode="plot",prefered_colnames = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_formated<-reactiveVal(NULL)
    data_ready<-reactiveVal(FALSE)
    
    levels_output <- reactive({
    levels_output <- if(input$levels==i18n("LEVEL_LABLE_GLOBAL")){
      'global'
    }else if(input$levels==i18n("LEVEL_LABLE_DETAIL")){
      'detail'
    }
    
    return(levels_output)
    })
    
    
    output$rank_params <- renderUI({
      req(!is.null(input$levels))
      
      if(isTRUE(rank)&levels_output()=="detail"){
        max_nb<-length(unique(df[[colTarget]]))
        tagList(
          numericInput(ns("number"), rankLabel, value = if(max_nb<=5){max_nb}else{5}, min = 0, max = max_nb)
          
        )
      }else{
        NULL
      }
      
      
       
      })
    rank_method_output <- reactive({
    
    if(input$rank_method==i18n("RANK_LABEL_TOTAL_CATCH_OVER_THE_PERIOD")){
      'sum'
    }else if(input$rank_method==i18n("RANK_LABEL_LAST_YEAR_TOTAL_CATCH")){
      "last_year"
    }else if(input$rank_method==i18n("RANK_LABEL_ANNUAL_CATCH_AVERAGE")){
      'year_avg'
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
          if(levels_output()=="detail"){
          req(input$number)
          if(rank_method_output()=="sum"){
            ranked <- df %>%
              group_by(target) %>% 
              summarise(total = sum(value))%>%
              mutate(rank = rank(-total)) %>%
              filter(rank <=as.numeric(input$number)) %>%
              arrange(rank)%>%
              pull(target)
          }	
          
          if(rank_method_output()=="year_avg"){
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
          
          if(rank_method_output()=="last_year"){
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
        
        if(levels_output()=="detail"){
          df<-df%>%
            mutate(value=value)%>%
            group_by(date,target,text)%>%
            summarise(agg=ifelse(stat=="mean",mean(value,na.rm=T),sum(value,na.rm=T)))%>%
            ungroup()
          
        }else{
          df<-df%>%
            mutate(value=value)%>%
            group_by(date)%>%
            summarise(agg=ifelse(stat=="mean",mean(value,na.rm=T),sum(value,na.rm=T)),text="",target=i18n("LEVEL_LABLE_GLOBAL"))%>%
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
            showlegend=ifelse(levels_output()=="detail",T,F),
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
    #colnames(data_formating())
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
        colnames = prefered_colnames,
        filter = list(position = 'top',clear =FALSE),
        options = list(
          dom = 'Bfrtip',
          scrollX=TRUE,
          pageLength=5,
          orientation ='landscape',
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  paste0(label,"_",levels_output(),"_",tolower(i18n("LABEL_STATISTICS"))), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  paste0(label,"_",levels_output(),"_",tolower(i18n("LABEL_STATISTICS"))), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',orientation = 'landscape',filename = paste0(label,"_",levels_output(),"_",tolower(i18n("LABEL_STATISTICS"))), 
            title = paste0(label,"_",levels_output(),"_",tolower(i18n("LABEL_STATISTICS"))), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          ),
          language = list(url = i18n("TABLE_LANGUAGE"))
        )
      )
    }
    
    })
  
    output$result<-renderUI({
      switch(mode,
        'plot+table'={
          tabsetPanel(
            tabPanel(i18n("LABEL_PLOT"),plotlyOutput(ns("plot"))%>%withSpinner(type = 4)),
            tabPanel(i18n("LABEL_STATISTICS"),DTOutput(ns("table"))%>%withSpinner(type = 4))
          )
        },
        'table+plot'={
          tabsetPanel(
            tabPanel(i18n("LABEL_STATISTICS"),DTOutput(ns("table"))%>%withSpinner(type = 4)),
            tabPanel(i18n("LABEL_PLOT"),plotlyOutput(ns("plot"))%>%withSpinner(type = 4))
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
