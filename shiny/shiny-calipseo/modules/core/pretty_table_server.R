#' @name pretty_table_server
#' @aliases pretty_table_server
#' @title pretty_table_server
#' @description \code{pretty_table_server} Server part of pretty_table module
#'
#' @usage pretty_table_server(id,df,colVariables,colValue)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param df dataframe 
#' @param colVariables list of column names available to selection and display
#' @param colValue column name of value
#'    

pretty_table_server <- function(id, df,colVariables=list(),colValue="value") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #Seems not work
    max_2_item_opts <- sortable_options(
      group = list(
        name = "my_shared_group",
        put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 2 child already
        return to.el.children.length < 2;
      }
    ")
      )
    )
    
    
    output$select_variable<-renderUI({
      bucket_list(
        header = i18n("BUCKET_HEADER"),
        group_name = "bucket_list_group",
        orientation = "horizontal",
        options = sortable_options(group = "my_shared_group"),
        add_rank_list(
          text = i18n("AVAILABLE_VARIABLES"),
          labels = colVariables,
          input_id = ns("available_variable")
        ),
        add_rank_list(
          text = i18n("ROW_VARIABLES"),
          labels = NULL,
          input_id = ns("row_variable")
        ),
        add_rank_list(
          text = i18n("COL_VARIABLES"),
          labels = NULL,
          input_id = ns("col_variable"),
          options = max_2_item_opts
        )
      )
    })
    
    output$grandTotal_wrapper<-renderUI({
      checkboxInput(ns("grandTotal"), i18n("TOTAL_LABEL"), value = FALSE)
    })
    
    data_formated<-reactiveVal(NULL)
    data_for_table<-reactiveVal(NULL)
    data_ready<-reactiveVal(FALSE)
    
    data_formating<-eventReactive(c(input$row_variable,input$col_variable,input$grandTotal),{
      
      new_df<-df%>%
        rename(setNames(colValue,"value"))
      
      if(length(input$row_variable)>0){
        col_to_filter<-c(input$row_variable,input$col_variable,"value")
        new_df<-new_df%>%
          filter_at(vars(col_to_filter), any_vars(. == 1))
        
        
        print(head(new_df))
        if(length(input$col_variable)>0){
        new_df<-new_df%>%
          group_by_at(c(input$row_variable,input$col_variable))%>%
          summarise(value=sum(value,na.rm=T))%>%
          ungroup()%>%
          pivot_wider(names_from=input$col_variable,values_from = c(value),names_sort=T,names_vary="slowest",values_fill=list(value=0))%>%
          ungroup()
        }else{
          new_df<-new_df%>%
            group_by_at(input$row_variable)%>%
            summarise(value=sum(value,na.rm=T))%>%
            ungroup()
        }
        
        if(input$grandTotal){
        new_df<-new_df%>%
          rowwise() %>%
          mutate(!!i18n("TOTAL_LABEL"):= sum(c_across((length(input$row_variable)+1):ncol(new_df))))%>%
          ungroup()
        
          new_df<-new_df%>%
            bind_rows(
            new_df%>%
              summarise(across(where(~!is.numeric(.)), ~paste0(i18n("TOTAL_LABEL"))),
                        across(where(is.numeric), ~sum(.))))
        }
        
        new_df<-new_df%>%
          mutate(across(where(~is.character(.)),~as.factor(.)))%>%
          ungroup()
        
        data_for_table<-data_for_table(new_df)
        
        print(new_df)
        data_formated(new_df)
        data_ready(TRUE)
      }
    }
    )
    
    output$table<-DT::renderDT(server = FALSE, {
      
      if(isTRUE(data_ready())){
        
        df_names<-names(data_for_table())
        
        # 
        # sketch = htmltools::withTags(table(
        #   class = 'display',
        #   thead(
        #     tr(
        #       th(rowspan = 2, 'Species'),
        #       th(colspan = 2, 'Sepal'),
        #       th(colspan = 2, 'Petal')
        #     ),
        #     tr(
        #       lapply(rep(c('Length', 'Width'), 2), th)
        #     )
        #   )
        # ))
        # 
        
        DT::datatable(
          data_for_table(),
          extensions = c("Buttons"),
          #container =sketch,
          escape = FALSE,
          rownames = FALSE,
          filter = list(position = 'top',clear =FALSE),
          options = list(
            dom = 'lBfrtip',
            scrollX=TRUE,
            pageLength=nrow(data_for_table()),
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
    
    observeEvent(c(input$row_variable,input$col_variable,input$grandTotal),{
      data_formating()
      output$result<-renderUI({
        if(isFALSE(data_ready())|length(input$row_variable)==0){
          p(style="text-align: center",i18n("EMPTY_TABLE_MESSAGE"))
        }else{
          DTOutput(ns("table"))%>%withSpinner(type = 4)
        }
      })
    })
    
  })  
}
