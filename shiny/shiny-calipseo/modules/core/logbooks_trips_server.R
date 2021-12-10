#logbooks_trips_server
logbooks_trips_server <- function(input, output, session, pool){
  ns<-session$ns
  
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
  
  print(head(trips))
  output$gantt<-renderPlotly({
   # Initialize empty plot
   fig <- plot_ly(source="gantt")
  
  # 
    for(i in 1:nrow(trips)){
      fig <- add_trace(fig,
                       x = c(trips$date_from[i], trips$date_to[i]),
                       y = trips$y[i],
                       key=trips$trip_id[i],
                      type='scatter',
                       mode = "lines",
                       line = list(color = 'steelblue',width = 20),
                       showlegend = F,
                       hoverinfo = "text",
                       text = trips$vesselname[i]
      )
    }
   
   fig <- layout(fig,
                 height= 20*max(trips$y),
                # height= 100,
                 xaxis = list(showgrid = T, tickfont = list(color = "#333333")),
                 yaxis = list(showgrid = F, tickfont = list(color = "#333333"),
                              tickmode = "array", tickvals = unique(trips$y), ticktext = unique(trips$vesselname),
                              domain = c(0, 1))
   
   )%>%event_register('plotly_click') 
  })
  
  observe({
    
    event.data = plotly::event_data("plotly_click", source = "gantt")
    
    if(is.null(event.data)) {
    } else {
      
      trip<-accessFishingTripDetails(pool,event.data$key)
      
      output$table <- DT::renderDT(server = FALSE, {
        
        species<-trip%>%
          mutate(Species= sprintf("%s <em>%s</em> [<b>%s</b>]",species_desc,species_sci,species_asfis))%>%
          mutate(Percent=quantity/sum(quantity))%>%
          arrange(-quantity)%>%
          mutate(Quantity= sprintf("%s %s",quantity,tolower(quantity_unit)))%>%
          select(Species,Quantity,Percent)
        
        DT::datatable(
          species,
          escape = FALSE,
          rownames = FALSE,
          options = list(
            pageLength = 10, dom = 't')
        )  %>%
          formatPercentage("Percent", 1) %>%
          formatStyle(
          'Percent',
          background = styleColorBar(c(0,1), 'lightblue',-90),
           backgroundSize = '98% 88%',
           backgroundRepeat = 'no-repeat',
          backgroundPosition = 'left'
        )
        
      })
      
      showModal(
        modalDialog(
          fluidRow(
            p(sprintf('Vessel : %s [%s]',toupper(trip$vessel_name[1]),trip$reg_number[1])),
            p(sprintf("From %s to %s [ %s days]",trip$date_from[1],trip$date_to[1],as.numeric(difftime(trip$date_to[1], trip$date_from[1], units = "days")))),
            p(sprintf('Vessel type : %s Fishing gear : %s',trip$vesseltype[1],trip$fishing_gear[1])),
            p(sprintf('Fishing zone : %s Landing site : %s',trip$fishing_zone[1],trip$landing_site[1])),
          ),
          fluidRow(DTOutput(ns("table"))%>%withSpinner(type = 4))
          ,
          title = 'Trip detail',
          size = 'l',
          easyClose = T,
          footer = NULL
        )
      )
     print(subset(trips,trip_id==event.data$key))
    }
  })
}

