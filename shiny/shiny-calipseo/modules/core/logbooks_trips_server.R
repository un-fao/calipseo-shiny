#logbooks_trips_server
logbooks_trips_server <- function(input, output, session, pool){
  ns<-session$ns
  
  data_formated<-reactiveVal(NULL)
  data_ready<-reactiveVal(FALSE)
  
  
  output$urlPage<-renderUI({
    session$userData$page("logbooks-trips")
    updatePageUrl("logbooks-trips", session)
  })
  
  trips <- accessLogBooksTrips(pool)  
  trips$date_from <- as.Date(trips$date_from, format = "%Y-%m-%d")
  trips$date_to <- as.Date(trips$date_to, format = "%Y-%m-%d")
  
  trips<-trips%>%
    arrange(vesselname,date_from)%>%
    group_by(vesselname)%>%
    mutate(y=cur_group_id())
  
  output$from<-renderUI({
    years<-sort(as.integer(unique(format(as.Date(unique(c(trips$date_from,trips$date_to))),format = '%Y'))))
    shinyWidgets::pickerInput(ns("period_from"),"Period from ",choices=years,selected = min(years),width = 'fit')
    })
  
  output$to<-renderUI({
    years<-sort(as.integer(unique(format(as.Date(unique(c(trips$date_from,trips$date_to))),format = '%Y'))))
    shinyWidgets::pickerInput(ns("period_to"),"to ",choices=years[as.integer(years)>=as.integer(input$period_from)],selected=max(years),width = 'fit')
  })
  
  # output$test<-renderUI({
  #   years<-sort(as.integer(unique(format(as.Date(unique(c(trips$date_from,trips$date_to))),format = '%Y'))))
  #   dateRangeInput(ns("daterange"), "Period:",
  #                  start  = min(years),
  #                  end    = max(years),
  #                  min    = min(years),
  #                  max    = max(years),
  #                  format = "yyyy",
  #                  language ='en',
  #                  startview = 'year',
  #                  separator = " to ")
  # })
  
  output$selector<-renderUI({
    years<-unique(format(as.Date(unique(c(trips$date_from,trips$date_to))),format = '%Y'))
    tagList(
    fluidRow(
      column(width=6,uiOutput(ns("from"))),
      column(width=6,uiOutput(ns("to")))
    ),
   # uiOutput(ns("test")),
    shinyWidgets::pickerInput(ns("vesseltype"),"Vessel type : ",choices=c("All",unique(trips$vesseltype)),width = 'fit'),
    shinyWidgets::pickerInput(ns("nbByPage"),"Vessels by page : ",choices=c("All",5,10,15,20),selected=10,width = 'fit'),
    actionButton(ns("go"),"Run selection")
    )
  })
    
  output$page_selector<-renderUI({
    if(!is.null(input$nbByPage))if(input$nbByPage!='All'){
      print(max(trips$y))
    x<-unique(c(seq(0,max(trips$y),by=as.integer(input$nbByPage)),max(trips$y)))
    shinyWidgets::pickerInput(ns("page"),"Vessels ",choices=unlist(lapply(seq(0:(length(x)-2)),function(y,i){paste0(y[i]+1,'-',y[i+1])},y=x)),width = 'fit')}else{
    shinyWidgets::pickerInput(ns("page"),"Vessels ",choices=paste0(1,'-',max(trips$y)),width = 'fit')}
  }
    )
  
  data_formating<-eventReactive(input$go,{
    formated<-subset(trips,date_to>=as.Date(sprintf('%s-01-01',input$period_from),format="%Y-%m-%d")&date_from<=as.Date(sprintf('%s-12-31',input$period_to),format="%Y-%m-%d"))

    if(!is.null(input$vesseltype))if(input$vesseltype!='All'){
      formated<-subset(formated,vesseltype==input$vesseltype)
    }
    data_formated(formated)
    data_ready(TRUE)
    })

  output$gantt<-renderPlotly({
    data_formating()
    
    if(isTRUE(data_ready())){
      print(head)
      trips<-data_formated()
      if(!is.null(input$page)){
      trips<-subset(trips,y%in%seq(as.integer(unlist(strsplit(input$page,"-"))[1]),as.integer(unlist(strsplit(input$page,"-"))[2]),by=1))
      }
      print(head(trips))
   fig <- plot_ly(source="gantt")

    for(i in 1:nrow(trips)){
      fig <- add_trace(fig,
                       x = c(trips$date_from[i], trips$date_to[i]),
                       y = trips$y[i],
                       key=trips$trip_id[i],
                      type='scatter',
                       mode = "lines",
                       line = list(color = '#0288D1',width = 10),
                       showlegend = F,
                       hoverinfo = "text",
                       text = trips$vesselname[i]
      )
    }
   
   fig <- layout(fig,
                 height= 100+15*length(unique(trips$y)),
                # height= 100,
                 xaxis = list(dtick = "M1", tickformat = '%m-%Y',showgrid = T, tickfont = list(color = "#333333")),
                 yaxis = list(showgrid = F, tickfont = list(color = "#333333"),
                              tickmode = "array", tickvals = unique(trips$y), ticktext = unique(trips$vesselname),
                              domain = c(0, 1))
   
   )%>%event_register('plotly_click') 
    }
  })
  
  observe({
    
    event.data = plotly::event_data("plotly_click", source = "gantt")
    
    if(is.null(event.data)) {
    } else {
      
      trip<-accessFishingTripDetails(pool,event.data$key)
      
      output$table <- DT::renderDT(server = FALSE, {
        
        species<-trip%>%
          mutate(Species= sprintf("%s [<b>%s</b>]<br><em>%s</em> ",species_desc,species_asfis,species_sci))%>%
          mutate(Percent=quantity/sum(quantity))%>%
          arrange(-quantity)%>%
          mutate(Quantity= sprintf("%s %s",quantity,tolower(quantity_unit)))%>%
          select(Species,Quantity,Percent)
        
        DT::datatable(
          species,
          escape = FALSE,
          rownames = FALSE,
          options = list(
            pageLength = 10, dom = 'tip')
        )  %>%
          formatPercentage("Percent", 1) %>%
          formatStyle(
          'Percent',
          background = styleColorBar(c(0,1), '#0288D1',-90),
           backgroundSize = '90% 80%',
           backgroundRepeat = 'no-repeat',
          backgroundPosition = 'left'
        )
        
      })
      
      showModal(
        modalDialog(
          fluidPage(
          fluidRow(
            column(12,
            HTML(sprintf("<p style='font-size:16px;'>Trip from <span style='color:#0288D1;'><u>%s</u></span> to <span style='color:#0288D1;'><u>%s</u></span> (<span style='color:#0288D1;'>%s</span> days)</p>",format(as.Date(trip$date_from[1]),format = '%d %B %Y'),format(as.Date(trip$date_to[1]),format = '%d %B %Y'),as.numeric(difftime(trip$date_to[1], trip$date_from[1], units = "days"))),)
            )),
          fluidRow(
            column(6,
            HTML(sprintf("<p style='font-size:16px;'>Vessel type : <span style='color:#0288D1;'><u>%s</u></span>",trip$vesseltype[1]))
            ),
            column(6,
            HTML(sprintf("<p style='font-size:16px;'>Fishing gear : <span style='color:#0288D1;'><u>%s</u></span>",trip$fishing_gear[1]))
            )
          ),
          fluidRow(
            column(6,
            HTML(sprintf("<p style='font-size:16px;'>Fishing zone : <span style='color:#0288D1;'><u>%s</u></span>",trip$fishing_zone[1]))
            ),
            column(6,
            HTML(sprintf("<p style='font-size:16px;'>Landing site : <span style='color:#0288D1;'><u>%s</u></span>",trip$landing_site[1])),
          )
          ),
          fluidRow(
            column(6,
                   HTML(sprintf("<p style='font-size:16px;'>Total quantity caught : <span style='color:#0288D1;'><u>%s kg</u></span>",sum(trip$quantity)))
            ),
            column(6,
                   HTML(sprintf("<p style='font-size:16px;'>Number of species caught : <span style='color:#0288D1;'><u>%s</u></span>",length(unique(trip$species_asfis)))),
            )
          ),
          fluidRow(column(10, align="center",offset=1,DTOutput(ns("table"))%>%withSpinner(type = 4)))
          )
          ,
          title = sprintf("Trip detail for Vessel '%s' [%s]",toupper(trip$vessel_name[1]),trip$reg_number[1]),
          size = 'm',
          easyClose = T,
          footer = NULL
        )
      )
     print(subset(trips,trip_id==event.data$key))
    }
  })
}


