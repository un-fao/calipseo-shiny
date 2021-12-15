#' @name trip_gantt_server
#' @aliases trip_gantt_server
#' @title trip_gantt_server
#' @description \code{trip_gantt_server} Server part of trip_gantt module
#'
#' @usage trip_gantt_server(id, pool,vessel_stat_type=NULL,vesselId=NULL,mode="full")
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param pool database connection 
#' @param vessel_stat_type id number to vessel stat type
#' @param vesselId registration number of vessel
#' @param mode 'full' for completed application with capacity to filter data and indicators; 'light' to minimal format without options
#'    

trip_gantt_server <- function(id, pool,vessel_stat_type=NULL,vesselId=NULL,mode="full") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_ready<-reactiveVal(FALSE)
    data_formated<-reactiveVal(NULL)
    trips <- accessFishingTrips(pool,vessel_stat_type,vesselId)  
    trips$date_from <- as.Date(trips$date_from, format = "%Y-%m-%d")
    trips$date_to <- as.Date(trips$date_to, format = "%Y-%m-%d")
    
  output$content<-renderUI({
    
    if(mode=="light"){
    
      box(id='trip-box', width = 12,
          fluidPage(
            fluidRow(column(8,offset=4,p("click on a trip to see more informations"))),
            fluidRow(plotlyOutput(ns("gantt"))%>%withSpinner(type = 4))
          )
      )
    
      }else{
        tagList(
          fluidRow(uiOutput(ns("indicators"))),
          box(
            id='trip-box',
            title="",
            width = 12,
            sidebar = shinydashboardPlus::boxSidebar(
              id=ns("box"),
              width = 25,
              startOpen = T,
              style = 'font-size:14px;',
              uiOutput(ns("selector"))
            ),
            fluidRow(
              column(3,offset=1,uiOutput(ns("page_selector"))),
              column(8,uiOutput(ns("message")))
            ),
            plotlyOutput(ns("gantt"))%>%withSpinner(type = 4)
          )
        )
      }
  })
    
  #Slider Parameters
  
  if(mode!="light"){
    
    output$from<-renderUI({
      years<-sort(as.integer(unique(format(as.Date(unique(c(trips$date_from,trips$date_to))),format = '%Y'))))
      shinyWidgets::pickerInput(ns("period_from"),"Period from ",choices=years,selected = min(years),width = 'fit')
    })
    
    output$to<-renderUI({
      years<-sort(as.integer(unique(format(as.Date(unique(c(trips$date_from,trips$date_to))),format = '%Y'))))
      shinyWidgets::pickerInput(ns("period_to"),"to ",choices=years[as.integer(years)>=as.integer(input$period_from)],selected=max(years),width = 'fit')
    })
    
    output$vesseltype_select<-renderUI({
      vesseltypeList<-unique(subset(trips,date_to>=as.Date(sprintf('%s-01-01',input$period_from),format="%Y-%m-%d")&
                           date_from<=as.Date(sprintf('%s-12-31',input$period_to),format="%Y-%m-%d"))$vesseltype)

      shinyWidgets::pickerInput(ns("vesseltype"),"Vessel type : ",choices=vesseltypeList,selected=NULL,multiple=T,width = 'fit',options = pickerOptions(title = "All"
      ))
     
    })
    
    output$vessel_select<-renderUI({
      vesselList<-subset(trips,date_to>=as.Date(sprintf('%s-01-01',input$period_from),format="%Y-%m-%d")&
                                       date_from<=as.Date(sprintf('%s-12-31',input$period_to),format="%Y-%m-%d"))
      if(!is.null(input$vesseltype)){
        vesselList<-subset(vesselList,vesseltype%in%input$vesseltype)
      }
      vesselList<-unique(vesselList$vesselname)
      shinyWidgets::pickerInput(ns("vesselname"),"Vessels : ",choices=vesselList,selected=NULL,multiple=T,width = 'fit',options = pickerOptions(title = "All"
      ))
    })

    output$nbByPage_select<-renderUI({
      tmp<-subset(trips,date_to>=as.Date(sprintf('%s-01-01',input$period_from),format="%Y-%m-%d")&
                           date_from<=as.Date(sprintf('%s-12-31',input$period_to),format="%Y-%m-%d"))
      if(!is.null(input$vesseltype)){
        tmp<-subset(tmp,vesseltype%in%input$vesseltype)
      }
      if(!is.null(input$vesselname)){
        tmp<-subset(tmp,vesselname%in%input$vesselname)
      }
      nbvessels<-length(unique(tmp$vesselname))
      if(nbvessels>10){
      shinyWidgets::pickerInput(ns("nbByPage"),"Vessels by page : ",choices=c(10,20,30,40,"All"),selected=10,multiple=F,width = 'fit')
      }else{return(NULL)}
    })
    
    output$test<-renderUI({
      dates<-as.Date(unique(trips$date_from,trips$date_to))
      dateRangeInput(ns("daterange"), "Period:",
                     start  = min(dates),
                     end    = max(dates),
                     min    = min(dates),
                     max    = max(dates),
                     format = "yyyy-mm-dd",
                     language ='en',
                     separator = " to ")
    })
    
    output$message<-renderUI({
      if(isTRUE(data_ready())){
        p("click on a trip to see more informations")
      }else{
        p("Select criteria to display corresponding trips")
      }
    })

    output$selector<-renderUI({
      years<-unique(format(as.Date(unique(c(trips$date_from,trips$date_to))),format = '%Y'))
      tagList(
        fluidRow(
          column(width=6,uiOutput(ns("from"))),
          column(width=6,uiOutput(ns("to")))
        ),
        #uiOutput(ns("test")),
        uiOutput(ns("vesseltype_select")),
        uiOutput(ns("vessel_select")),
        uiOutput(ns("nbByPage_select")),
        actionButton(ns("go"),"Run selection")
      )
    })
    
  #Data subsetting
    data_formating<-eventReactive(input$go,{
      
      formated<-subset(trips,date_to>=as.Date(sprintf('%s-01-01',input$period_from),format="%Y-%m-%d")&
                    date_from<=as.Date(sprintf('%s-12-31',input$period_to),format="%Y-%m-%d"))
      if(!is.null(input$vesseltype)){
        formated<-subset(formated,vesseltype%in%input$vesseltype)
      }
      if(!is.null(input$vesselname)){
        formated<-subset(formated,vesselname%in%input$vesselname)
      }
      formated<-formated%>%
        arrange(vesselname,date_from)%>%
        group_by(vesselname)%>%
        mutate(y=cur_group_id())
      
      output$page_selector<-renderUI({
        if(!is.null(input$nbByPage))if(input$nbByPage!='All'){
          x<-unique(c(seq(0,max(formated$y),by=as.integer(input$nbByPage)),max(formated$y)))
          shinyWidgets::pickerInput(ns("page"),"Vessels ",choices=unlist(lapply(seq(0:(length(x)-2)),function(y,i){paste0(y[i]+1,'-',y[i+1])},y=x)),width = 'fit')}else{
          shinyWidgets::pickerInput(ns("page"),"Vessels ",choices=paste0(1,"-",max(formated$y)),width = 'fit')}
      })
      
      output$indicators<-renderUI({
        fluidRow(
          infoBox("Number of vessels",max(formated$y), icon = icon("ship"), fill = TRUE, width = 3),
          infoBox("Number of trips",length(unique(formated$trip_id)), icon = icon("calendar",lib="glyphicon"), fill = TRUE, width = 3)
        )
      })
      
      data_formated(formated)
      data_ready(TRUE)
      
    })
    
  }
    
  #Plot
      
  if(mode=="light"){
    output$gantt<-renderPlotly({
      trips<-trips%>%
      arrange(vesselname,date_from)%>%
      group_by(vesselname)%>%
      mutate(y=cur_group_id())
        
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
                    height= 150,
                    xaxis = list(dtick = "M1", tickformat = '%m-%Y',showgrid = T,tickfont = list(color = "#333333"),
                                 rangeslider = list(visible = F),
                                 rangeselector=list(
                                   buttons=list(
                                     list(count=3, label="3y", step="year", stepmode="backward"),
                                     list(count=2, label="2y", step="year", stepmode="backward"),
                                     list(count=1, label="1y", step="year", stepmode="backward"),
                                     list(step="all")
                                   ))),
                    yaxis = list(showgrid = F, tickfont = list(color = "#333333"),
                                 tickmode = "array", tickvals = unique(trips$y), ticktext = unique(trips$vesselname),
                                 domain = c(0, 1))
                    
      )%>%event_register('plotly_click') 
    })
  }else{
    output$gantt<-renderPlotly({
        data_formating()
        if(isTRUE(data_ready()))if(!is.null(input$page)){
          trips<-subset(data_formated(),y%in%seq(as.integer(unlist(strsplit(input$page,"-"))[1]),as.integer(unlist(strsplit(input$page,"-"))[2]),by=1))
          
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
                        xaxis = list(dtick = "M1", tickformat = '%m-%Y',showgrid = T,tickfont = list(color = "#333333"),
                                     rangeslider = list(visible = F),
                                     rangeselector=list(
                                       buttons=list(
                                         list(count=3, label="3y", step="year", stepmode="backward"),
                                         list(count=2, label="2y", step="year", stepmode="backward"),
                                         list(count=1, label="1y", step="year", stepmode="backward"),
                                         list(step="all")
                                       ))),
                        yaxis = list(showgrid = F, tickfont = list(color = "#333333"),
                                     tickmode = "array", tickvals = unique(trips$y), ticktext = unique(trips$vesselname),
                                     domain = c(0, 1))
                        
          )%>%event_register('plotly_click') 
        }
    })
  }
  
  #Detail page
    observe({
      
      event.data = plotly::event_data("plotly_click", source = "gantt")
      
      if(is.null(event.data)) {
      } else {
        
        trip<-accessFishingTripDetails(pool,event.data$key)
        
        output$table <- DT::renderDT(server = FALSE, {
          
          species<-trip%>%
            mutate(Species= sprintf("%s [<b>%s</b>]<br><em>%s</em> ",species_desc,species_asfis,species_sci))%>%
            group_by(Species,quantity_unit)%>%
            summarise(quantity=sum(quantity))%>%
            ungroup()%>%
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
        
        landing_site<-trip%>%
          select(landing_site,ls_longitude,ls_latitude)%>%
          distinct()%>%
          rename(longitude=ls_longitude)%>%
          rename(latitude=ls_latitude)
        
        output$landing_map <- renderUI({
          if(is.na(landing_site$latitude)|is.na(landing_site$longitude)){
            HTML("<p><em>(landing site position not available)</em></p>")
          }else{
            leafletOutput(ns("map_ls"),width = "90%",height=150)
          }
        })
        
        if(!is.na(landing_site$latitude)&!is.na(landing_site$longitude)){
          output$map_ls <- renderLeaflet({
            leaflet() %>%
              addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%
              addMarkers(data = landing_site, lat = as.numeric(landing_site$latitude), lng = as.numeric(landing_site$longitude))%>%
              setView(lat = as.numeric(landing_site$latitude), lng = as.numeric(landing_site$longitude),zoom=6)
          })
        }
        
        fishing_zone<-trip%>%
          select(fishing_zone,fz_longitude,fz_latitude)%>%
          distinct()%>%
          rename(longitude=fz_longitude)%>%
          rename(latitude=fz_latitude)
        
        output$fishingZone_map <- renderUI({
          if(is.na(fishing_zone$latitude)|is.na(fishing_zone$longitude)){
            HTML("<p><em>(fishing zone position not available)</em></p>")
          }else{
            leafletOutput(ns("map_fz"),width = "90%",height=150)
          }
        })
        
        if(!is.na(fishing_zone$latitude)&!is.na(fishing_zone$longitude)){
          output$map_fz <- renderLeaflet({
            leaflet() %>%
              addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(noWrap = TRUE)) %>%
              addMarkers(data = fishing_zone, lat = as.numeric(fishing_zone$latitude), lng = as.numeric(fishing_zone$longitude))%>%
              setView(lat = as.numeric(fishing_zone$latitude), lng = as.numeric(fishing_zone$longitude),zoom=6)
          })
        }
        
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
                       uiOutput(ns("fishingZone_map"))
                ),
                column(6,
                       uiOutput(ns("landing_map"))
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
  })
}