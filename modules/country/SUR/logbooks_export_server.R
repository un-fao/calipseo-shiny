#logbooks_export_server
logbooks_export_server <- function(id, parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    output$year_wrapper<-renderUI({
      years<-accessAvailableYears(pool)$year
      selectizeInput(ns("year"),
                     label="Year",
                     multiple = F,
                     choices = sort(years,T),
                     selected=max(as.integer(years)),
                     options = list(
                       placeholder = "Select a year"
                     )
      )
    })
    
    output$aggregate_wrapper<-renderUI({
      selectizeInput(ns("aggregate"),
                     label="Aggregate",
                     multiple = F,
                     choices = c("By month"="month","By quarter"="quarter"),
                     selected= "month",
                     options = list(
                       placeholder = "Select a temporal aggregation"
                     )
      )
    })
    
    output$format_wrapper<-renderUI({
      selectizeInput(ns("format"),
                     label="Format",
                     multiple = F,
                     choices = c("Generic"="generic","Simplified"="simplified"),
                     selected= "simplified",
                     options = list(
                       placeholder = "Select a format"
                     )
      )
    })
    
    output$task_wrapper<-renderUI({
      selectizeInput(ns("task"),
                     label="Task",
                     multiple = F,
                     choices = c("Task I.2-Nominal catches"="task_1.2"),
                     selected= "task_1.2",
                     options = list(
                       placeholder = "Select a task"
                     )
      )
    })
    
    output$btn_wrapper<-renderUI({
      downloadButton(ns("run"), 'Download')
    })
    
    
     output$run <- downloadHandler(
       filename = function() {
         sprintf('WECAFC_SUR_%s_%s_%s_%s.csv',input$year,input$task,input$format,input$aggregate)
       },
       content = function(file) {
         switch(input$task,
          "task_1.2"={
             data<-accessLogBooksWecafc(pool,year=input$year)
             data<-data%>%
              rowwise()%>%
                mutate(period=ifelse(input$aggregate=="month",sprintf('%02d',as.integer(period)),ifelse(as.integer(period)%in%c(1:3),"Q1",
                                                                                                       ifelse(as.integer(period)%in%c(4:6),"Q2",
                                                                                                              ifelse(as.integer(period)%in%c(7:9),"Q3","Q4")))))%>%
               group_by(flagstate,year,period,geographic_identifier,species,catch_unit)%>%
               summarise(catch_retained=sum(ifelse(catch_unit=="Kilogram"&!is.null(catch_retained),catch_retained/1000,catch_retained),na.rm=T),
                         catch_discarded=sum(ifelse(catch_unit=="Kilogram"&!is.null(catch_discarded),catch_discarded/1000,catch_discarded),na.rm=T),
                         catch_nominal=sum(ifelse(catch_unit=="Kilogram"&!is.null(catch_nominal),catch_nominal/1000,catch_nominal),na.rm=T))%>%
               ungroup()%>%
               mutate(catch_unit=ifelse(catch_unit=="Kilogram","t",catch_unit))%>%
               select(flagstate,year,period,geographic_identifier,species,catch_retained,catch_discarded,catch_nominal,catch_unit)
             
             if(input$format=="generic"){
               data<-data%>%
                 pivot_longer(c(catch_retained,catch_discarded,catch_nominal),names_to="measurement_type",values_to="measurement_value")%>%
                 mutate(measurement_type=gsub("^.*?_","",measurement_type))%>%
                 rename(measurement_unit=catch_unit)%>%
               mutate(measurement="catch")%>%
               mutate(date=paste0(year,"-",period))%>%
               mutate(time_start=dateformating(date,"start"),
                      time_end=dateformating(date,"end"),
                      time=paste0(time_start,"/",time_end))%>%
               select(c(flagstate,
                        time,
                        time_start,
                        time_end, 
                        geographic_identifier,
                        species,
                        measurement,
                        measurement_type,
                        measurement_value,
                        measurement_unit))
             }
         }
         )
         
         
         write.csv(data, file,row.names = F)
       }
     )
    
  })
}
