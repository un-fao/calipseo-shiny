#observor_reports_server
observor_reports_server <- function(id, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    target_report<-reactiveVal(NULL)
    target_vessel_info<-reactiveVal(NULL)
    target_vessel_equipment<-reactiveVal(NULL)
    
    reports_info <- accessObserverReportsSummary(pool)
    vessel_info <- accessObserverVesselInfo(pool)
    vessel_equipment <- accessObserverVesselEquipment(pool)
    
    
    
    output$report_selector<-renderUI({
      
      reports<-setNames(reports_info$ID, sprintf("[%s] - %s (%s-%s)",reports_info$ID,reports_info$VESSEL_NAME,reports_info$OBSERVATION_PERIOD_START,reports_info$OBSERVATION_PERIOD_END))
      
      selectizeInput(ns("report"),paste0(i18n("SELECT_TITLE_REPORT")," :"),choices=reports,multiple = F,selected=NULL,
                     options = list(
                       placeholder = i18n("SELECT_REPORT_PLACEHOLDER"),
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
    })
    
    observeEvent(input$report,{
      req(input$report)
      target_vessel_info(subset(vessel_info,DT_OBSERVER_REPORT_ID==input$report))
      tmp<-target_vessel_info()
      print("VESSEL INFO")
      print(target_vessel_info())
      target_vessel_equipment(subset(vessel_equipment,DT_OBSERVER_REPORT_VESSEL_INFO_ID== tmp$ID))
      print("VESSEL EQUIPMENT")
      print(target_vessel_equipment())
    })
    
    
    output$table_equipment<-DT::renderDT(server = FALSE, {
    DT::datatable(
      target_vessel_equipment()%>%
        select(-ID,-DT_OBSERVER_REPORT_VESSEL_INFO_ID,-OTHER_DESCRIPTION, -UPDATER_ID, -COMMENT, -CREATED_AT, -UPDATED_AT)%>%
        pivot_longer(
          cols = everything(),       
          names_to = "EQUIPMENT",     
          values_to = "HERE"        
        )%>%
        mutate(HERE = case_when(
          is.na(HERE) ~ '<i class="fas fa-question-circle" style="color:orange"></i>',
          HERE == 1   ~ '<i class="fas fa-check-circle" style="color:green"></i>',
          HERE == 0   ~ '<i class="fas fa-times-circle" style="color:red"></i>'
        )),
      extensions = c("Buttons"),
      escape = FALSE,
      filter = list(position = 'top',clear =FALSE),
      options = list(
        dom = 'Bfrtip',
        scrollX=TRUE,
        pageLength=10,
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
    })
    
    output$radarPlot <- renderPlotly({
      
      categories <- c("Confort", "Security", "Sanitary", "Food", "Weather")
      
      values <- c(1,2,3,4,5)
      
      plot_ly(
        type = 'scatterpolar',
        r = values,
        theta = categories,
        fill = 'toself',
        name = input$report
      ) %>%
        layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0,5))
          ),
          showlegend = TRUE
        )
    })
    
    output$species_infobox <- renderUI({
      req(reports_info)
      fluidRow(
        shiny::tagList(
          CalipseoInfoBox(i18n("INFOBOX_TITLE_TOTLANDING"), "0", icon = icon("ship")),
          CalipseoInfoBox(i18n("INFOBOX_TITLE_TOTDISCARD"), "0", icon = icon("ship")),
          CalipseoInfoBox(i18n("INFOBOX_TITLE_NBSPECIES"), "0", icon = icon("fish")),
        )
      )
    })
    
    
    
    output$map <- renderLeaflet({
      
      # --- Simulate 3 fishing activities in Cameroon waters ---
      set.seed(123)
      fishing_activities <- tibble(
        id = 1:3,
        start_lat = runif(3, 2.5, 5),   # Cameroon offshore latitude
        start_lng = runif(3, 8, 10),    # Cameroon offshore longitude
        end_lat   = start_lat + runif(3, 0.1, 0.3),  # slight move
        end_lng   = start_lng + runif(3, 0.1, 0.3)
      )
      
    
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        setView(lng = 9.3, lat = 4.0, zoom = 7)
      
      
      for (i in 1:nrow(fishing_activities)) {
        m <- m %>%
          addPolylines(
            lng = c(fishing_activities$start_lng[i], fishing_activities$end_lng[i]),
            lat = c(fishing_activities$start_lat[i], fishing_activities$end_lat[i]),
            color = "blue",
            weight = 3,
            opacity = 0.7,
            label = paste("Fishing activity", fishing_activities$id[i]),
            popup = paste0(
              "<b>Activity ID:</b> ", fishing_activities$id[i], "<br/>",
              "<b>Start:</b> (", round(fishing_activities$start_lat[i], 3), ", ", round(fishing_activities$start_lng[i], 3), ")<br/>",
              "<b>End:</b> (", round(fishing_activities$end_lat[i], 3), ", ", round(fishing_activities$end_lng[i], 3), ")"
            )
          ) %>%
          addCircleMarkers(
            lng = fishing_activities$start_lng[i],
            lat = fishing_activities$start_lat[i],
            radius = 5, color = "green", fill = TRUE, fillOpacity = 0.8,
            label = paste("Start of activity", fishing_activities$id[i])
          ) %>%
          addCircleMarkers(
            lng = fishing_activities$end_lng[i],
            lat = fishing_activities$end_lat[i],
            radius = 5, color = "red", fill = TRUE, fillOpacity = 0.8,
            label = paste("End of activity", fishing_activities$id[i])
          )
      }
      
      m
    })
    
    output$map_density <- renderLeaflet({
      
      set.seed(42)
      # --- Simulate fishing activities ---
      fishing_activities <- tibble(
        id = 1:10,
        start_lat = runif(10, 2.5, 5),
        start_lng = runif(10, 8, 10),
        end_lat   = start_lat + runif(10, 0.05, 0.2),
        end_lng   = start_lng + runif(10, 0.05, 0.2),
        species   = sample(c("Sardinella", "Tuna", "Shrimp"), 10, replace = TRUE),
        catch_kg  = round(runif(10, 50, 500), 0)
      )
      
      fishing_activities <- fishing_activities %>%
        mutate(
          centroid_lat = (start_lat + end_lat) / 2,
          centroid_lng = (start_lng + end_lng) / 2
        )
      
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        setView(lng = 9.3, lat = 4.0, zoom = 7)
      
      if (nrow(fishing_activities) > 0) {
        m <- m %>%
          addHeatmap(
            lng = ~centroid_lng, lat = ~centroid_lat,
            intensity = ~catch_kg,
            blur = 20, max = 1, radius = 15,
            data = fishing_activities
          )
      }
      
      m <- m %>%
        addCircleMarkers(
          lng = ~centroid_lng, lat = ~centroid_lat,
          color = "white", fillColor = "blue", fillOpacity = 0.8,
          radius = ~sqrt(catch_kg)/4,
          label = ~paste0("Species: ", species, 
                          "<br>Catch: ", catch_kg, " kg")
        )
      
      m
    })
    
    
    output$vessel_box_wrapper<-renderUI({
      req(input$report)
      my_vessel_info<-target_vessel_info()
      print("DEBUG1")
      print(my_vessel_info)
      tagList(
        fluidRow(
          box(title=HTML("<b>",i18n("BOX_TITLE_OBSERVER"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
              div(
                column(3,
                       p(strong(paste0(i18n("OBSERVER_NAME"),": ")),",")
                )
              )
          )
          ),
        fluidRow(
        box(title=HTML("<b>",i18n("BOX_TITLE_VESSEL"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
            div(
              column(3,
                     p(strong(paste0(i18n("VESSEL_NAME"),": ")),my_vessel_info$VESSEL_NAME[1]),
                     p(strong(paste0(i18n("REGISTRATION_NUMBER"),": ")), my_vessel_info$REGISTRATION_NUMBER[1])
            ),
            column(3,
                   p(strong(paste0(i18n("LOA"),": ")),my_vessel_info$LOA[1]),
                   p(strong(paste0(i18n("DRA"),": ")), my_vessel_info$DRA[1]),
                   p(strong(paste0(i18n("WIDTH"),": ")), my_vessel_info$WITDH[1]),
                   p(strong(paste0(i18n("SPEED"),": ")), my_vessel_info$SPEED[1]),
                   p(strong(paste0(i18n("GT"),": ")), my_vessel_info$GT[1]),
                   p(strong(paste0(i18n("NET_TONNAGE"),": ")), my_vessel_info$NET_TONNAGE[1]),
                   p(strong(paste0(i18n("SPEED"),": ")), my_vessel_info$SPEED[1]),
                   p(strong(paste0(i18n("ENGINE_POWER"),": ")), my_vessel_info$ENGINE_POWER[1])
            ),
        )
      )
      ),
      fluidRow(
        box(title=HTML("<b>",i18n("BOX_TITLE_OWNER"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
            div(
              column(3,
                     p(strong(paste0(i18n("OWNER_NAME"),": ")),my_vessel_info$OWNER_NAME[1])
              )
        )
      )
      ),
      fluidRow(
        box(title=HTML("<b>",i18n("BOX_TITLE_EQUIPMENT"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
            div(
              DTOutput(ns("table_equipment"))
            )
        )
      ),
      fluidRow(
        box(title=HTML("<b>",i18n("BOX_TITLE_QUALITY"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
            div(
              plotlyOutput(ns("radarPlot"))
            )
        )
      ),
      fluidRow(
        box(title=HTML("<b>",i18n("BOX_TITLE_ENVIRONMENT"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
            div(
              p("Waiting data")
            )
        )
      ),
      fluidRow(
        box(title=HTML("<b>",i18n("BOX_TITLE_SPECIES"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
            tagList(
            div(uiOutput(ns("species_infobox"))),
            div(
              p("Waiting data")
            )
            )
        )
      ),
      fluidRow(
        box(title=HTML("<b>",i18n("BOX_TITLE_METRICS"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
            div(
              p("Waiting data")
            )
        )
      ),
      fluidRow(
        box(title=HTML("<b>",i18n("BOX_TITLE_ACTIVITY"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
            div(
              leafletOutput(ns("map"), height = 600)
            )
        )
      ),
      fluidRow(
        box(title=HTML("<b>",i18n("BOX_TITLE_SPECIES_DENSITY"),"</b>"),status = "info", solidHeader = TRUE,collapsible = T,collapsed=F,width = 12,
            div(
              leafletOutput(ns("map_density"), height = 600)
            )
        )
      )
      )
    })
    
  })
}
