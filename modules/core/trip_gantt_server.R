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
#' @param withMap boolean, if TRUE display fishing zone and landing site map
#'    

trip_gantt_server <- function(id, pool,vessel_stat_type=NULL,vesselId=NULL,mode="full", withMap=F) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_ready<-reactiveVal(FALSE)
    data_formated<-reactiveVal(NULL)
    trips <- accessFishingTrips(pool,vessel_stat_type,vesselId)  
    trips$date_from <- as.Date(trips$date_from, format = "%Y-%m-%d")
    trips$date_to <- as.Date(trips$date_to, format = "%Y-%m-%d")
    
  output$content<-renderUI({
    
    if(mode=="light"){
      if(nrow(trips)>0){
        box(id='trip-box', width = 12,
            fluidPage(
              fluidRow(column(8,offset=4,p(i18n("DISPLAY_TRIP_MSG")))),
              fluidRow(plotlyOutput(ns("gantt"))%>%withSpinner(type = 4))
            )
        )
      }else{
          box(id='trip-box', width = 12,
              fluidPage(
                fluidRow(column(8,offset=4,p(i18n("NO_TRIP_MSG"))))
              )
          )
        }
    
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
    
    output$daterange<-renderUI({
      dates<-as.Date(unique(trips$date_from,trips$date_to))
      dateRangeInput(ns("period"), paste0(i18n("PERIOD"),":"),
                     start  = min(dates),
                     end    = max(dates),
                     min    = min(dates),
                     max    = max(dates),
                     format = "yyyy-mm-dd",
                     language = appConfig$language,
                     separator = paste0(" ",i18n("DATE_SEPARATOR_TO")," "),
                     width='60%')
    })
    
    output$vesseltype_select<-renderUI({

       vesseltypeList<-unique(subset(trips,date_from>=input$period[1]&date_to<=input$period[2])$vesseltype)
     

      shinyWidgets::pickerInput(ns("vesseltype"),paste(i18n("VESSEL TYPE"),":"),choices=vesseltypeList,selected=NULL,multiple=T,width = 'fit',
                                options = pickerOptions(title = "All"))
     
    })
    
    output$vessel_select<-renderUI({
       vesselList<-subset(trips,date_from>=input$period[1]&date_to<=input$period[2])
      #vesselList<-trips
      if(!is.null(input$vesseltype)){
        vesselList<-subset(vesselList,vesseltype%in%input$vesseltype)
      }
      vesselList<-unique(vesselList$vesselname)
      shinyWidgets::pickerInput(ns("vesselname"),paste(i18n("PICKER_LABEL_VESSELS"),":"),choices=vesselList,selected=NULL,multiple=T,width = 'fit',
                                options = pickerOptions(title = "All",maxOptions = 5,maxOptionsText = i18n("PICKER_LABEL_VESSELS_HINT")))
    })

    output$nbByPage_select<-renderUI({
       tmp<-subset(trips,date_from>=input$period[1]&date_to<=input$period[2])
      #tmp<-trips
      if(!is.null(input$vesseltype)){
        tmp<-subset(tmp,vesseltype%in%input$vesseltype)
      }
      if(!is.null(input$vesselname)){
        tmp<-subset(tmp,vesselname%in%input$vesselname)
      }
      nbvessels<-length(unique(tmp$vesselname))
      if(nbvessels>10){
      shinyWidgets::pickerInput(ns("nbByPage"),paste(i18n("PICKER_LABEL_VESSELS_BY_PAGE"),":"),choices=c(10,20,30,40,"All"),selected=10,multiple=F,width = 'fit')
      }else{return(NULL)}
    })
    
    output$message<-renderUI({
      if(isTRUE(data_ready())){
        p(i18n("DISPLAY_TRIP_MSG"))
      }else{
        p(i18n("SELECT_TRIP_MSG"))
      }
    })

    output$selector<-renderUI({
      tagList(
        uiOutput(ns("daterange")),
        uiOutput(ns("vesseltype_select")),
        uiOutput(ns("vessel_select")),
        uiOutput(ns("nbByPage_select")),
        actionButton(ns("go"),i18n("RUN_SELECTION_LABEL"))
      )
    })
    
  #Data subsetting
    data_formating<-eventReactive(input$go,{
      
       formated<-subset(trips,date_from>=input$period[1]&date_to<=input$period[2])
      #formated<-trips
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
          shinyWidgets::pickerInput(ns("page"),paste0(i18n("VESSELS_LABEL")," "),choices=unlist(lapply(seq(0:(length(x)-2)),function(y,i){paste0(y[i]+1,'-',y[i+1])},y=x)),width = 'fit')}else{
          shinyWidgets::pickerInput(ns("page"),paste0(i18n("VESSELS_LABEL")," "),choices=paste0(1,"-",max(formated$y)),width = 'fit')}
      })
      
      output$indicators<-renderUI({
        fluidRow(
          infoBox(i18n("INFOBOX_TITLE_NUMBER_OF_VESSELS"),max(formated$y), icon = icon("ship"), fill = TRUE, width = 3),
          infoBox(i18n("INFOBOX_TITLE_NUMBER_OF_TRIPS"),length(unique(formated$trip_id)), icon = icon("calendar",lib="glyphicon"), fill = TRUE, width = 3)
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
                                     list(count=3, label=i18n("3_YEAR_LABEL"), step="year", stepmode="backward"),
                                     list(count=2, label=i18n("2_YEAR_LABEL"), step="year", stepmode="backward"),
                                     list(count=1, label=i18n("1_YEAR_LABEL"), step="year", stepmode="backward"),
                                     list(step=i18n("ALL_YEAR_LABEL"))
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
                                         list(count=3, label=i18n("3_YEAR_LABEL"), step="year", stepmode="backward"),
                                         list(count=2, label=i18n("2_YEAR_LABEL"), step="year", stepmode="backward"),
                                         list(count=1, label=i18n("1_YEAR_LABEL"), step="year", stepmode="backward"),
                                         list(step=i18n("ALL_YEAR_LABEL"))
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
            mutate(Quantity= sprintf("%s %s",round(quantity,0),tolower(quantity_unit)))%>%
            select(Species,Quantity,Percent)
          
          names(species) <- c(i18n("TRIP_TABLE_COLNAME_1"),i18n("TRIP_TABLE_COLNAME_2"),
                              i18n("TRIP_TABLE_COLNAME_3"))
          
          DT::datatable(
            species,
            escape = FALSE,
            rownames = FALSE,
            options = list(
              pageLength = 10, dom = 'tip',
              language = list(url = i18n("TABLE_LANGUAGE")))
          )  %>%
            formatPercentage(i18n("TRIP_TABLE_COLNAME_3"), 1) %>%
            formatStyle(
              i18n("TRIP_TABLE_COLNAME_3"),
              background = styleColorBar(c(0,1), '#0288D1',-90),
              backgroundSize = '90% 80%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'left'
            )
          
        })
        
        
        if(isTRUE(withMap)){
        
        landing_site<-trip%>%
          select(landing_site,ls_longitude,ls_latitude)%>%
          distinct()%>%
          rename(longitude=ls_longitude)%>%
          rename(latitude=ls_latitude)
        
        output$landing_map <- renderUI({
          if(is.na(landing_site$latitude)|is.na(landing_site$longitude)){
            HTML(paste("<p><em>(",i18n("LANDING_SITE_COORDINATES_NOT_AVAILABLE"),")</em></p>"))
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
            HTML(paste("<p><em>(",i18n("FISHING_ZONE_COORDINATES_NOT_AVAILABLE"),")</em></p>"))
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
        
        output$map<-renderUI({
          fluidRow(
            column(6,
                   uiOutput(ns("fishingZone_map"))
            ),
            column(6,
                   uiOutput(ns("landing_map"))
            )
          )
          })
        }else{
          output$map<-renderUI({NULL})
        }
        
        showModal(
          modalDialog(
            fluidPage(
              fluidRow(
                column(12,
                       HTML(sprintf(paste("<p style='font-size:16px;'>",i18n("TRIP_FROM"), "<span style='color:#0288D1;'><u>%s</u></span>",i18n("DATE_SEPARATOR_TO"),"<span style='color:#0288D1;'><u>%s</u></span> (<span style='color:#0288D1;'>%s</span>",i18n("DAYS"),")</p>"),format(as.Date(trip$date_from[1]),format = '%d-%m-%Y'),format(as.Date(trip$date_to[1]),format = '%d-%m-%Y'),round(as.numeric(difftime(trip$date_to[1], trip$date_from[1], units = "days")),1)))
                )),
              fluidRow(
                column(6,
                       HTML(sprintf(paste("<p style='font-size:16px;'>",i18n("VESSEL TYPE"),": <span style='color:#0288D1;'><u>%s</u></span>"),trip$vesseltype[1]))
                ),
                column(6,
                       HTML(sprintf(paste("<p style='font-size:16px;'>",i18n("LABEL_FISHING_GEAR"),": <span style='color:#0288D1;'><u>%s</u></span></p>"),if(length(unique(trip$fishing_gear))>1){sprintf("<ul>%s</li></ul>",paste0("<li style='color:#0288D1;>",unique(trip$fishing_gear),collapse = "","</li>"))}else{trip$fishing_gear[1]}))
                       )
              ),
              fluidRow(
                column(6,
                       HTML(sprintf(paste("<p style='font-size:16px;'>",i18n("LABEL_FISHING_ZONE"),": <span style='color:#0288D1;'><u>%s</u></span>"),trip$fishing_zone[1]))
                ),
                column(6,
                       HTML(sprintf(paste("<p style='font-size:16px;'>",i18n("LABEL_LANDING_SITE"),": <span style='color:#0288D1;'><u>%s</u></span>"),trip$landing_site[1]))
                )
              ),
              uiOutput(ns('map')),
              fluidRow(
                valueBox(value=tags$p(i18n("LABEL_QUANTITY_CAUGHT"),style="font-size: 40%"), subtitle=paste(round(sum(trip$quantity),2), trip$quantity_unit[1]), icon = tags$i(class = "fas fa-balance-scale", style="font-size: 30px"), width = 4),
                valueBox(value=tags$p(i18n("LABEL_CONTENT"),style="font-size: 40%"),subtitle=paste(length(unique(trip$species_asfis)),i18n("LABEL_CONTENT_SUBTITLE")), icon = tags$i(class = "fas fa-fish", style="font-size: 30px"), width = 4),
                valueBox(value=tags$p(i18n("LABEL_GLOBAL_CPUE"),style="font-size: 40%"), subtitle=paste(round(sum(trip$quantity)/as.numeric(difftime(trip$date_to[1], trip$date_from[1], units = "days")),0),ifelse(trip$quantity_unit[1]=="pound",
                                                                                                                                                                                                                    i18n("LABEL_GLOBAL_CPUE_SUBTITLE_LB"),
                                                                                                                                                                                                                    ifelse(trip$quantity_unit[1]=="kilogram",
                                                                                                                                                                                                                           i18n("LABEL_GLOBAL_CPUE_SUBTITLE_KG"),paste0(trip$quantity_unit[1],i18n("LABEL_GLOBAL_CPUE_SUBTITLE_OTHER"))))), icon = tags$i(class = "fas fa-chart-line", style="font-size: 30px"),  width = 4)
              ),
              fluidRow(column(10, align="center",offset=1,DTOutput(ns("table"))%>%withSpinner(type = 4)))
            )
            ,
            title = sprintf(paste(i18n("TITLE_TRIP_DETAILS_FOR_VESSEL"),"'%s' [%s]"),toupper(trip$vessel_name[1]),trip$reg_number[1]),
            size = 'm',
            easyClose = T,
            footer = NULL
          )
        )
        
      }
    })
  })
}