#' @name pyramid_chart_server
#' @aliases pyramid_chart_server
#' @title pyramid_chart_server
#' @description \code{pyramid_chart_server} Server part of pyramid_chart module
#'
#' @usage pyramid_chart_server(id,df,colAge,colGender,colVariables,mode)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param df dataframe 
#' @param label label use to target column
#' @param colAge column name of age variable 
#' @param colGender column name of gender variable 
#' @param colVariables variables list available for breakdown and fill 
#' @param mode indicate mode to display result, 4 modes available ,'plot','table','plot+table','table+plot'
#'    

pyramid_chart_server <- function(id, df,colAge=NULL,colGender=NULL,colVariables=c(),mode="plot") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_formated<-reactiveVal(NULL)
    data_for_table<-reactiveVal(NULL)
    data_ready<-reactiveVal(FALSE)
    col_av_filter<-reactiveVal(NULL)
    col_av_fill<-reactiveVal(NULL)
    
    observeEvent(input$mode,{
      if(input$mode=="pyramid"){
        col_av_filter<-col_av_filter(colVariables)
        col_av_fill<-col_av_fill(c(colGender,colVariables))
      }else{
        col_av_filter<-col_av_filter(c(colGender,colVariables))
        col_av_fill<-col_av_fill(c(colGender,colVariables))
      }
    })
    
    df<-df%>%
           rename(setNames(colAge,"age"))
    
    
    output$mode_selector<-renderUI({
      if(!is.null(colGender)){
        choices<-setNames(c("pyramid","stacked_bar","percent_bar"),c(i18n("PYRAMID_CHOICE"),i18n("STACKED_CHOICE"),i18n("PERCENT_CHOICE")))
      }else{
        choices<-setNames(c("stacked_bar","percent_bar"),c(i18n("STACKED_CHOICE"),i18n("PERCENT_CHOICE")))
      }
      
      selectInput(ns("mode"), i18n("MODE_SELECTOR_LABEL"), choices=choices,selected=choices[1], multiple=F)
    })
    
    output$age_slider<-renderUI({
      sliderInput(ns("age_range"), i18n("AGE_SELECTOR_LABEL"),
                  min = min(df$age), max = max(df$age),
                  value = c(min(df$age),max(df$age)), step = 1
      )
    })
    
    output$step_selector<-renderUI({
      numericInput(ns("step"), i18n("STEP_SELECTOR_LABEL"), 1, min = 1, max = 100)
    })
    
    
    output$fill_selector<-renderUI({
      choices=col_av_fill()
      print(input$filter_col)
      print(sub)
      print(choices)
      selectInput(ns("fill_col"), i18n("FILL_SELECTOR_LABEL"), choices=choices, multiple=F)
    })
    
    observeEvent(input$fill_col,{
    output$filter_selector<-renderUI({

      lapply(col_av_filter()[col_av_filter()!=input$fill_col], function(i) {
        col<-col_av_filter()[is.element(col_av_filter(),i)]
        values<-unique(df[[col]])
        
        pickerInput(
          inputId = ns(paste0("filter_",i)),
          label = sprintf(i18n("FILTER_SELECTOR_LABEL"),names(col)), 
          choices = values,
          selected = values,
          options = list(
            title = i18n("FILTER_NO_VALUE")),
          multiple=T
        )
      })
      
    })
    })
    
     data_formating<-eventReactive(c(input$age_range,input$step,input$fill_col,input$mode,lapply(col_av_filter(), function(i) {input[[paste0("filter_", i)]]})),{
       new_df<-subset(df,age>=input$age_range[1]&age<=input$age_range[2])
       
       var_list<-sapply(setNames(unname(col_av_filter()),unname(col_av_filter())), function(i) {input[[paste0("filter_", i)]]})
       var_to_filter<-var_list[!var_list %in% list(NULL)]
       var_to_remove<-names(var_list[var_list %in% list(NULL)])
       if(input$mode=="pyramid"){
         group_variables<-unique(c("Gender",input$fill_col,"age_gr"))
       }else{
         group_variables<-unique(c(input$fill_col,"age_gr"))
       }
      
       if(length(var_to_remove)>0){
         new_df<-new_df[,!names(new_df) %in% var_to_remove]
       }
       
       if(length(var_to_filter)>0){
         for(i in 1: length(var_to_filter)){
           new_df<-new_df[new_df[[names(var_to_filter[i])]] %in% var_to_filter[[i]],]
         }
       }
       
       age_range<-c(input$age_range[1]:input$age_range[2])
       
       if(input$mode=="pyramid"){
       new_df<-new_df%>%
         mutate(value=1)%>%
         filter(Gender%in%c("Male","Female"))%>%
         complete(nesting(!!!syms(names(var_to_filter))),age=age_range,Gender=c("Male","Female"),fill=list(value=0))%>%
         mutate(age_gr=cut_width(age, width =input$step,closed = "left",boundary =min(age)))%>%
         group_by_at(group_variables)%>%
         summarise(value=sum(value))%>%
         arrange(Gender)%>%
         ungroup()%>%
         complete(!!!syms(group_variables),fill=list(value=0))
       }else if(input$mode=="stacked_bar"){
         new_df<-new_df%>%
           mutate(value=1)%>%
           complete(!!!syms(names(var_to_filter)),age=c(min(age):max(age)),fill=list(value=0))%>%
           mutate(age_gr=cut_width(age, width =input$step,closed = "left",boundary =min(age)))%>%
           group_by_at(group_variables)%>%
           summarise(value=sum(value))%>%
           ungroup()%>%
           complete(!!!syms(group_variables),fill=list(value=0))
       }else{
         new_df<-new_df%>%
           mutate(value=1)%>%
           complete(!!!syms(names(var_to_filter)),age=c(min(age):max(age)),fill=list(value=0))%>%
           mutate(age_gr=cut_width(age, width =input$step,closed = "left",boundary =min(age)))%>%
           group_by_at(group_variables)%>%
           summarise(value=sum(value))%>%
           ungroup()%>%
           group_by(age_gr)%>%
           mutate(tot=sum(value))%>%
           mutate(value=value/tot*100)%>%
           ungroup()%>%
           select(-tot)%>%
           complete(!!!syms(group_variables),fill=list(value=0))
       }
       
        print(input$age_range)
        data_for_table<-data_for_table(new_df)
        

         data_formated(new_df)
         data_ready(TRUE)

     }
     )
    
    #observeEvent(c(input$stat,input$granu,input$number),{
    output$plot<-renderPlotly({
       data_formating()
       
       if(isTRUE(data_ready())){
          
         if(input$mode=="pyramid"){
           
           print(summary(data_formated()))
           
          p<-data_formated() %>% 
            mutate(value = ifelse(Gender == "Male",  -value, value)) %>%
            mutate(abs_value = abs(value))%>%
            plot_ly() %>% 
            add_trace(x= ~value, y=~age_gr, color=~get(input$fill_col),type='bar',orientation = 'h', hoverinfo = 'text', text = ~abs_value) %>%
            layout(bargap = 0.1, barmode = 'relative',
                   yaxis = list(title = i18n("PYRAMID_Y_LABEL"),autotypenumbers = 'strict',tickfont=list(size=10)),
                   
                   xaxis =  list(title = i18n("PYRAMID_X_LABEL"),tickmode = 'array', tickvals = c(-rev(seq(50,max(abs(data_formated()%>%filter(Gender=="Male")%>%pull(value))),50)),0,50),
                                 ticktext = c(as.character(rev(seq(50,max(abs(data_formated()%>%filter(Gender=="Male")%>%pull(value))),50)),0,50)))
            )
         }else if(input$mode=="stacked_bar"){
           p<-data_formated()%>%plot_ly(
             x = ~age_gr,
             y = ~value,
             color = ~get(input$fill_col),
             type = "bar"
           ) %>% 
             layout(barmode = "stack",
                    yaxis = list(title = i18n("STACKED_Y_LABEL")),
                    xaxis = list(title = i18n("STACKED_X_LABEL"))
             )
         }else{
           p<-data_formated()%>%plot_ly(
             x = ~age_gr,
             y = ~value,
             color = ~get(input$fill_col),
             type = "bar"
           ) %>% 
             layout(barmode = "stack",
                    yaxis = list(title = i18n("PERCENT_Y_LABEL")),
                    xaxis = list(title = i18n("PERCENT_X_LABEL"))
             )
         }
          
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
