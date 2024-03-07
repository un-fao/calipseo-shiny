#' @name sunburst_chart_server
#' @aliases sunburst_chart_server
#' @title sunburst_chart_server
#' @description \code{sunburst_chart_server} Server part of sunburst_chart module
#'
#' @usage sunburst_chart_server(id,df,colVariables,colValue,mode)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param df dataframe 
#' @param colVariables list of column names available to selection and display
#' @param colValue column name of value
#' @param mode indicate mode to display result, 4 modes available ,'plot','table','plot+table','table+plot'
#'    

sunburst_chart_server <- function(id, df,colVariables=list(),colValue="value",mode="plot") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$select_variable<-renderUI({
      bucket_list(
        header = i18n("BUCKET_HEADER"),
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list(
          text = i18n("AVAILABLE_VARIABLES"),
          labels = if(length(colVariables)>3){colVariables[4:length(colVariables)]}else{NULL},
          input_id = ns("available_variable")
        ),
        add_rank_list(
          text = i18n("SELECTED_VARIABLES"),
          labels = if(length(colVariables)>=3){ colVariables[1:3]}else{ colVariables},
          input_id = ns("selected_variable")
        )
      )
    })
    
    data_formated<-reactiveVal(NULL)
    data_for_table<-reactiveVal(NULL)
    data_ready<-reactiveVal(FALSE)
    
    data_formating<-eventReactive(c(input$selected_variable),{
      
      df<-df%>%
        rename(setNames(colValue,"value"))
      
      if(!is.null(input$selected_variable)){
        
        new_df<-df%>%
          select(input$selected_variable,value)%>%
          group_by_at(input$selected_variable)%>%
          summarise(value=sum(value,na.rm=T))%>%
          ungroup()
      
        data_for_table<-data_for_table(new_df)
        
        new_df<-new_df%>%
          mutate(parent="",root="Total")%>%
          relocate(c(parent,root),.before=everything())
        
        sb_df<-do.call("rbind",lapply(2:(ncol(new_df)-1), function(i){
          
          if(i<=3){
            target_parent<-names(new_df)[i-1]  
          }else{
            target_parent<-names(new_df)[(2):(i-1)]
          }
          
          target_label<-names(new_df)[i]
          target_cols<-names(new_df)[1:i]
          target_cols2<-names(new_df)[2:i]
          
          
          target_df<-new_df[,c(1:i,ncol(new_df))]
          
          target_df<-target_df%>%
            group_by_at(c(target_cols))%>%
            summarise(value=sum(value))%>%
            rowwise()%>%
            #mutate(ids = paste0(!!!syms(target_cols)))%>%
            unite(ids,target_cols2, sep = " - ", remove = FALSE)%>%
            unite(parents,target_parent, sep = " - ", remove = FALSE)%>%
            ungroup()
          
          out<-data.frame(ids=target_df$ids,
                     parents=target_df$parents,
                     labels=target_df[[target_label]],
                     values=target_df$value,
                     stringsAsFactors = FALSE)
            
            return(out)
        
      }))
        
        print(sb_df)
        data_formated(sb_df)
        data_ready(TRUE)
      }
    }
    )
    
    #observeEvent(c(input$stat,input$granu,input$number),{
    output$plot<-renderPlotly({
      data_formating()
      
      if(isTRUE(data_ready())){
        
        p<-data_formated()%>%plot_ly(ids = ~ids, labels = ~labels, parents = ~parents,values= ~values, type = 'sunburst',branchvalues = 'total',maxdepth = length(colVariables)+1,hoverinfo="label+value+percent parent+percent root")

      }
    })
    #  })
    
    output$table<-DT::renderDT(server = FALSE, {
      
      data_formating()
      
      if(isTRUE(data_ready())){
        
        
        DT::datatable(
          data_for_table(),
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
              list(extend = 'csv', filename =  i18n("STATISTIC_DATA_EXPORT_FILENAME"), title = NULL, header = TRUE),
              list(extend = 'excel', filename =  i18n("STATISTIC_DATA_EXPORT_FILENAME"), title = NULL, header = TRUE),
              list(extend = "pdf", pageSize = 'A4',orientation = 'landscape',filename = i18n("STATISTIC_DATA_EXPORT_FILENAME"), 
                   title = i18n("STATISTIC_PDF_TITLE"), header = TRUE)
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
