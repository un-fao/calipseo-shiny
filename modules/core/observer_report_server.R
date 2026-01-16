#observer_report_server
observer_report_server <- function(id,parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    # Dynamic access by Detail in report-list
    # TO FIX - issue to go back and update report
    # output$report_header <- renderText({
    #   sprintf(
    #     '<a href="#" onclick="Shiny.setInputValue(\'%s\', %d, {priority: \'event\'});" style="margin-left:15px;font-weight:bold;">%s</a>',
    #     ns("back_to_reports"), 1, i18n("BACK_TO_LIST_OF_REPORTS")
    #   )
    # })
    # 
    # observeEvent(input$back_to_reports, {
    #   DEBUG("Click to return to the list of reports")
    #   parent.session$userData$record_selection = NULL
    #   isolate({ bs4Dash::updateTabItems(parent.session, "calipseo-tabs", "report_list") })
    # }, ignoreInit = T)
    # 
    # report_selected <- reactive({
    #   parent.session$userData$record_selection
    # })
    
    #Temporary report access by selector
    output$report_selector<-renderUI({
    reports_list <- accessObserverReportsSummary(pool)
    reports_list <- reports_list$report_id
    reports_list<-setNames(reports_list,paste0(i18n("OBSERVER_REPORT_REPORT_SELECTED")," #",reports_list))

    selectInput(
      inputId = ns("report_selected"),
      label   = i18n("OBSERVER_REPORT_SELECT_REPORT_LABEL"),
      choices = reports_list,
      selected = reports_list[1]
    )
    })
        
    report_selected <- reactive({
      req(input$report_selected)
      input$report_selected
    })
        
    

    
    # --- Utils functions ---
    unknown_tag <- function() {
      tagList(
        icon("exclamation-triangle", style = "color:darkorange;"),
        span(
          i18n("OBSERVER_REPORT_UNKNOWN"),
          style = "color:darkorange; margin-left:4px;"
        )
      )
    }
    
    unit_unknown_tag <- function() {
      tagList(
        span(" (", style = "color:darkorange;"),
        icon("exclamation-triangle", style = "color:darkorange;"),
        span(
          i18n("OBSERVER_REPORT_UNIT_UNKNOWN"),
          style = "color:darkorange; margin-left:4px;"
        ),
        span(")", style = "color:darkorange;")
      )
    }
    
    li_value <- function(label, value) {
      tags$li(
        style = "display:flex; gap:8px;",
        tagList(
          tags$span(
            paste0(i18n(label), ":"),
            style = "min-width:220px; color:#555;"
          ),
          if (is.na(value)){unknown_tag()}else if(trimws(as.character(value))==""){unknown_tag()}else{tags$b(value)}
        )
      )
    }
    
    li_value_unit <- function(label, value, unit) {
      tags$li(
        style = "display:flex; gap:8px;",
        tagList(
          tags$span(
            paste0(i18n(label), ": "),
            style = "min-width:220px; color:#555;"
          ),
          if (is.na(value)) {
            unknown_tag()
          }else if(trimws(as.character(value))==""){
            unknown_tag()
          }else {
            tags$b(
              tagList(
                value,
                if (is.na(unit)) {
                  unit_unknown_tag()
                }else if(trimws(as.character(unit))==""){
                  unit_unknown_tag()
                } else {
                  paste0(" ", unit)
                }
              )
            )
          }
        )
      )
    }
    
    li_bool <- function(label, value) {
      tags$li(
        style = "display:flex; gap:8px;",
        tagList(
          tags$span(
            paste0(i18n(label), ": "),
            style = "min-width:220px; color:#555;"
          ),
          if (is.na(value)) {
            unknown_tag()
          }else if(trimws(as.character(value))==""){
            unknown_tag()
          } else if (value == 1) {
            icon("check", style = "color:green;")
          } else {
            icon("times", style = "color:red;")
          }
        )
      )
    }
    
    ### --- Data preparation ---
    reports_info <- reactive({
      df <- accessObserverReportsSummary(pool,report_id=report_selected())
    
    df$embarkation_start <- as.POSIXct(as.character(df$embarkation_start))
    attr(df$embarkation_start, "tzone") <- appConfig$country_profile$timezone
    df$embarkation_end <- as.POSIXct(as.character(df$embarkation_end))
    attr(df$embarkation_end, "tzone") <- appConfig$country_profile$timezone
    df$observation_start <- as.POSIXct(as.character(df$observation_start))
    attr(df$observation_start, "tzone") <- appConfig$country_profile$timezone
    df$observation_end <- as.POSIXct(as.character(df$observation_end))
    attr(df$observation_end, "tzone") <- appConfig$country_profile$timezone
    
    df$trip_start <- as.POSIXct(as.character(df$trip_start))
    attr(df$trip_start, "tzone") <- appConfig$country_profile$timezone
    df$trip_end <- as.POSIXct(as.character(df$trip_end))
    attr(df$trip_end, "tzone") <- appConfig$country_profile$timezone
    
    df
    })
    
    report_vessel<-reactive({
      accessObserverVesselsDetails(pool,report_id=report_selected())
    })
    
    report_activities<-reactive({
      
      df<-accessObserverTripsDetails(pool,report_id=report_selected())
    
    df$DATE_FROM <- as.POSIXct(as.character(df$DATE_FROM))
    attr(df$DATE_FROM, "tzone") <- appConfig$country_profile$timezone
    df$DATE_TO <- as.POSIXct(as.character(df$DATE_TO))
    attr(df$DATE_TO, "tzone") <- appConfig$country_profile$timezone
    
    df
    })
    
    report_activities_sum<-reactive({
        report_activities()%>%
      group_by(REPORT_ID, TRIP_ID, ACTIVITY_ID, DATE_FROM, DATE_TO, LONGITUDE, LATITUDE, OBSERVER_PRESENT, WIRED_CABLE, IS_DAYTIME)%>%
      summarise(LANDED_QUANTITY = sum(LANDED_QUANTITY,na.rm=T),
                DISCARD_QUANTITY = sum(DISCARD_QUANTITY, na.rm=T),
                SP_IS_ETP = ifelse(any(1 %in% SP_IS_ETP),1,0))%>%
      ungroup()
    })    
    
    tot_landed<-reactive({report_activities()%>%
      filter(LANDED_QUANTITY>0)%>%
      summarise(SP=length(unique(SP_CODE)),
                QUANTITY=sum(LANDED_QUANTITY,n.rm=T),
                UNIT = unique(LANDED_QUANTITY_UNIT)
      )%>%
      ungroup()
    })
    
    tot_discard<-reactive({report_activities()%>%
      filter(DISCARD_QUANTITY>0)%>%
      summarise(SP=length(unique(SP_CODE)),
                QUANTITY=sum(DISCARD_QUANTITY,n.rm=T),
                UNIT = unique(DISCARD_QUANTITY_UNIT)
      )%>%
      ungroup()
    })
    
    tot_sp<-reactive({report_activities()%>%
      select(SP_CODE,SP_IS_ETP)%>%
      distinct()%>%
      summarise(SP=length(unique(SP_CODE)),
                ETP=sum(SP_IS_ETP,na.rm=T))%>%
      ungroup()
    })
    
    tot_act<-reactive({report_activities()%>%
      select(ACTIVITY_ID,OBSERVER_PRESENT)%>%
      distinct()%>%
      summarise(ACTIVITY=length(unique(ACTIVITY_ID)),
                OBSERVED=sum(OBSERVER_PRESENT,na.rm=T))%>%
      ungroup()
    })
    
    report_logbook<-reactive({
      accessObserverReportsHasLogbook(pool,report_id=report_selected())
    })
    
    # --- Decription Tab ---
    #Description Tab - timeline
    output$observer_timeline <- renderPlotly({
      
      y_label <- i18n("OBSERVER_REPORT_DESCRIPTION_OBSERVER_LABEL")
      
      p <- plotly::plot_ly()
        
        #Embarkation period
      if(!is.na(reports_info()$embarkation_start))if(!is.na(reports_info()$embarkation_end))if(reports_info()$embarkation_start<=reports_info()$embarkation_end){
       p <- p %>% 
         add_segments(
          x = reports_info()$embarkation_start,
          xend = reports_info()$embarkation_end,
          y = y_label,
          yend = y_label,
          line = list(width = 12, color = "rgba(70,130,180,0.6)"),
          name = i18n("OBSERVER_REPORT_DESCRIPTION_EMBARKATION_LABEL"),
          hovertext = paste(
            i18n("OBSERVER_REPORT_DESCRIPTION_FROM"), reports_info()$embarkation_start,
            "<br>",
            i18n("OBSERVER_REPORT_DESCRIPTION_TO"), reports_info()$embarkation_end
          ),
          hoverinfo = "text"
        )
    }
       
      #Trip period
      if(!is.na(reports_info()$trip_start))if(!is.na(reports_info()$trip_end))if(reports_info()$trip_start<=reports_info()$trip_end){
        p <- p %>%
          add_segments(
            x = reports_info()$trip_start,
            xend = reports_info()$trip_end,
            y = y_label,
            yend = y_label,
            line = list(width = 14, color = "rgba(80,220,100,0.8)"),
            name = i18n("OBSERVER_REPORT_DESCRIPTION_TRIP_LABEL"),
            hovertext = paste(
              i18n("OBSERVER_REPORT_DESCRIPTION_FROM"), reports_info()$trip_start,
              "<br>",
              i18n("OBSERVER_REPORT_DESCRIPTION_TO"), reports_info()$trip_end
            ),
            hoverinfo = "text"
          )
      }
        
        #Observation period
         if(!is.na(reports_info()$observation_start))if(!is.na(reports_info()$observation_end))if(reports_info()$observation_start<=reports_info()$observation_end){
        p <- p %>%
          add_segments(
          x = reports_info()$trip_start,
          xend = reports_info()$trip_end,
          y = y_label,
          yend = y_label,
          line = list(width = 5, color = "rgba(255,140,0,0.9)"),
          name = i18n("OBSERVER_REPORT_DESCRIPTION_OBSERVATION_LABEL"),
          hovertext = paste(
            i18n("OBSERVER_REPORT_DESCRIPTION_FROM"), reports_info()$observation_start,
            "<br>",
            i18n("OBSERVER_REPORT_DESCRIPTION_TO"), reports_info()$observation_end
          ),
          hoverinfo = "text"
        )
         }
        
        p <- p %>%
          layout(
          xaxis = list(title = NULL),
          yaxis = list(title = "", showticklabels = FALSE),
          showlegend = TRUE,
          margin = list(l = 20, r = 20, t = 20, b = 40),
          hovermode = "closest"
        )
      
      p
    })
    
    #Description Tab content wrapper
    output$description_content <-renderUI({
      tagList(
        
        tags$h4(i18n("OBSERVER_REPORT_DESCRIPTION_TITLE")),
        
        tags$b(i18n("OBSERVER_REPORT_DESCRIPTION_OBSERVER")),
        tags$div(style="margin-bottom:10px;",
                 li_value("OBSERVER_REPORT_DESCRIPTION_OBSERVER_NAME", reports_info()$observer_name)
        ),
        
        fluidRow(
          column(4,
                 tags$b(i18n("OBSERVER_REPORT_DESCRIPTION_EMBARKATION")),
                 li_value("OBSERVER_REPORT_DESCRIPTION_EMBARKATION_DATE", reports_info()$embarkation_start),
                 li_value("OBSERVER_REPORT_DESCRIPTION_EMBARKATION_PORT", reports_info()$embarkation_port)
          ),
          column(4,
                 tags$b(i18n("OBSERVER_REPORT_DESCRIPTION_DISEMBARKATION")),
                 li_value("OBSERVER_REPORT_DESCRIPTION_DISEMBARKATION_DATE", reports_info()$embarkation_end),
                 li_value("OBSERVER_REPORT_DESCRIPTION_DISEMBARKATION_PORT", reports_info()$disembarkation_port)
          ),
          column(4,
                 bs4InfoBox(
                   title = i18n("OBSERVER_REPORT_DESCRIPTION_DAYS_ON_BOARD"),
                   value = paste(round(as.numeric(reports_info()$days_at_sea),0),i18n("OBSERVER_REPORT_DESCRIPTION_DAYS")),
                   icon = icon("clock"),
                   color = "info",
                   width = 12
                 )
          )
        ),
        tags$b(i18n("OBSERVER_REPORT_DESCRIPTION_TIMELINE")),
        plotlyOutput(ns("observer_timeline"), height = "120px")
      )
      
    })

    
    # --- Vessel Tab ---
    #Vessel Tab - content wrapper
    output$vessel_content <-renderUI({
      #name
      tagList(
        tags$h4(i18n("OBSERVER_REPORT_VESSEL_INFO_TITLE")),
      
      #general description
      bs4Card(
        title = i18n("OBSERVER_REPORT_VESSEL"),width = 12,solidHeader = TRUE,status = "primary",
        tags$div(
          style = "margin-top:10px;",
          fluidRow(
            column(6, 
            tags$ul(
              li_value("OBSERVER_REPORT_VESSEL_NAME", report_vessel()$VESSEL_NAME),
              li_value("OBSERVER_REPORT_VESSEL_OWNER", report_vessel()$VESSEL_OWNER),
              li_value("OBSERVER_REPORT_NATIONALITY", report_vessel()$NATIONALITY)
            )
            ),
            column(6,
              tags$ul(
              li_value("OBSERVER_REPORT_CONSTRUCTION_YEAR", report_vessel()$CONSTRUCTION_YEAR),
              li_value("OBSERVER_REPORT_REGISTRATION_NUMBER", report_vessel()$REGISTRATION_NUMBER),
              li_value("OBSERVER_REPORT_LICENCE_NUMBER", report_vessel()$LICENCE_NUMBER)
              )
            )
          )
        )
      ),
      bs4Card(
        title = i18n("OBSERVER_REPORT_VESSEL_CARACTERISTICS"),width = 12,solidHeader = TRUE,status = "info",
        tags$div(
          style = "margin-top:10px;",
          fluidRow(
            column(6, 
              tags$ul(
                li_value_unit("OBSERVER_REPORT_LOA", report_vessel()$LOA, report_vessel()$LOA_UNIT),
                li_value_unit("OBSERVER_REPORT_DRA", report_vessel()$DRA, report_vessel()$DRA_UNIT),
                li_value_unit("OBSERVER_REPORT_WIDTH", report_vessel()$WIDTH, report_vessel()$WIDTH_UNIT),
                li_value_unit("OBSERVER_REPORT_GT", report_vessel()$GT, report_vessel()$GT_UNIT)
              )
            ),
            column(6,
              tags$ul(
                li_value_unit("OBSERVER_REPORT_NET_TONNAGE", report_vessel()$NET_TONNAGE, report_vessel()$NET_TONNAGE_UNIT),
                li_value_unit("OBSERVER_REPORT_SPEED", report_vessel()$SPEED, report_vessel()$SPEED_UNIT),
                li_value_unit("OBSERVER_REPORT_ENGINE_POWER", report_vessel()$ENGINE_POWER, report_vessel()$ENGINE_POWER_UNIT)
              )
            )
          )
          )
        ),
      bs4Card(
        title = i18n("OBSERVER_REPORT_VESSEL_EQUIPMENTS"),width = 12,solidHeader = TRUE,status = "warning",
        tags$div(
          style = "margin-top:10px;",
          fluidRow(
            column(6, 
              tags$ul(
                li_bool("OBSERVER_REPORT_VHF_RADIO", report_vessel()$VHF_RADIO),
                li_bool("OBSERVER_REPORT_HF_RADIO", report_vessel()$HF_RADIO),
                li_bool("OBSERVER_REPORT_RADAR", report_vessel()$RADAR),
                li_bool("OBSERVER_REPORT_SOUNDER", report_vessel()$SOUNDER),
                li_bool("OBSERVER_REPORT_SONAR", report_vessel()$SONAR)
              )
            ),
            column(6,
              tags$ul(
                li_bool("OBSERVER_REPORT_AUTOPILOT", report_vessel()$AUTOPILOT),
                li_bool("OBSERVER_REPORT_SATELITE", report_vessel()$SATELITE),
                li_bool("OBSERVER_REPORT_ROUTE_TRACER", report_vessel()$ROUTE_TRACER),
                li_bool("OBSERVER_REPORT_NET_PROBE", report_vessel()$NET_PROBE)
              )
            )
          )
        )
      ),
      bs4Card(
        title = i18n("OBSERVER_REPORT_FISH_PRESERVATION"),width = 12,solidHeader = TRUE,status = "danger",
        tags$div(
          style = "margin-top:10px;",
          fluidRow(
            column(6, 
              tags$ul(
                li_value("OBSERVER_REPORT_FISH_CONSERVATION_METHODS", report_vessel()$FISH_CONSERVATION_METHODS),
                li_value("OBSERVER_REPORT_NUMBER_OF_STORES", report_vessel()$NUMBER_OF_STORES)
              )
            ),
            column(6,
              tags$ul(
                li_value_unit("OBSERVER_REPORT_STORAGE_CAPACITY",report_vessel()$STORAGE_CAPACITY,report_vessel()$STORAGE_CAPACITY_UNIT),
                li_value_unit("OBSERVER_REPORT_FREEZING_CAPACITY",report_vessel()$FREEZING_CAPACITY,report_vessel()$FREEZING_CAPACITY_UNIT)
              )
            )
          )
        )
      )
      )
    })
    
    # --- Trip Tab ---
    
    #Trip Tab - Timeline
    
    assign_activity_lanes <- function(df) {
      
      df <- df %>% arrange(DATE_FROM)
      
      lane_end <- c()               
      lane_id  <- integer(nrow(df)) 
      
      for (i in seq_len(nrow(df))) {
        
        placed <- FALSE
        
        if (length(lane_end) > 0) {
          for (l in seq_along(lane_end)) {
            if (df$DATE_FROM[i] >= lane_end[l]) {
              lane_id[i]  <- l
              lane_end[l] <- df$DATE_TO[i]
              placed <- TRUE
              break
            }
          }
        }
        
        if (!placed) {
          lane_end <- c(lane_end, df$DATE_TO[i])
          lane_id[i] <- length(lane_end)
        }
      }
      
      df$lane <- lane_id
      df
    }
    
    output$activities_timeline <- renderPlotly({
      
      d <- report_activities_sum() %>%
        mutate(
          obs_status = ifelse(OBSERVER_PRESENT == 1, "Observer Present", "Observer Absent"),
          tooltip = paste0(
            i18n("OBSERVER_REPORT_TRIP_ACTIVITY"), " #", ACTIVITY_ID,
            "<br>", i18n("OBSERVER_REPORT_TRIP_FROM"), ": ", DATE_FROM,
            "<br>", i18n("OBSERVER_REPORT_TRIP_TO"), ": ", DATE_TO,
            "<br>", i18n("OBSERVER_REPORT_TRIP_LANDED_QUANTITY"), ": ", round(LANDED_QUANTITY, 2),
            "<br>", i18n("OBSERVER_REPORT_TRIP_DISCARD_QUANTITY"), ": ", round(DISCARD_QUANTITY, 2),
            "<br>", i18n("OBSERVER_REPORT_TRIP_ETP"), ": ",
            ifelse(SP_IS_ETP == 1, i18n("OBSERVER_REPORT_TRIP_YES"), i18n("OBSERVER_REPORT_TRIP_NO")),
            "<br>", i18n("OBSERVER_REPORT_TRIP_DAYTIME"), ": ",
            ifelse(IS_DAYTIME == 1, i18n("OBSERVER_REPORT_TRIP_YES"), i18n("OBSERVER_REPORT_TRIP_NO")),
            "<br>", i18n("OBSERVER_REPORT_TRIP_WIRED_CABLE"), ": ",
            ifelse(WIRED_CABLE == 1, i18n("OBSERVER_REPORT_TRIP_YES"), i18n("OBSERVER_REPORT_TRIP_NO"))
          )
        ) %>%
        assign_activity_lanes() %>%
        mutate(
          y_lane = - lane
        )
      
      req(nrow(d) > 0)
      
      trip_start <- min(reports_info()$trip_start, na.rm = TRUE)
      trip_end   <- max(reports_info()$trip_end,   na.rm = TRUE)
      
      trip_lane <- i18n("OBSERVER_REPORT_TRIP_FULL_TRIP")
      
      y_levels <- c(trip_lane, unique(d$y_lane))
      
      p <- plotly::plot_ly() %>%
        
        add_segments(
          x = trip_start,
          xend = trip_end,
          y = 0,
          yend = 0,
          line = list(width = 14, color = "rgba(180,180,180,0.35)"),
          name = i18n("OBSERVER_REPORT_TRIP_FULL_TRIP"),
          hoverinfo = "none"
        ) %>%
        
        add_segments(
          data = d,
          x = ~DATE_FROM,
          xend = ~DATE_TO,
          y = ~y_lane,
          yend = ~y_lane,
          color = ~obs_status,
          colors = c(
            "Observer Present" = "rgba(255,140,0,0.9)",
            "Observer Absent"  = "rgba(70,130,180,0.8)"
          ),# To check how make it workable with i18n
          line = list(width = 6),
          text = ~tooltip,
          hoverinfo = "text"
        ) %>%
        
        layout(
          xaxis = list(
            title = "",
            type = "date"
          ),
          yaxis = list(
            title = "",
            showticklabels = FALSE,
            zeroline = FALSE
          ),
          hovermode = "closest",
          margin = list(l = 20, r = 20, t = 20, b = 40)
        )
      
      p
    })
    
    #Trip Tab - Indicators  
    output$first_line_indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = i18n("OBSERVER_REPORT_TRIP_INDICATOR_TRIP_DURATION"),
          value = sprintf("%s %s",round(sum(as.numeric(reports_info()$trip_end - reports_info()$trip_start, units = "days"), na.rm = TRUE),0),i18n("OBSERVER_REPORT_TRIP_INDICATOR_DAYS")),
          subtitle = sprintf("%s - %s",reports_info()$trip_start,reports_info()$trip_end),
          icon = icon("ship"),
          color = "warning",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_REPORT_TRIP_INDICATOR_OBSERVATION_DURATION"),
          value = sprintf("%s %s",round(sum(as.numeric(reports_info()$observation_end - reports_info()$observation_start, units = "days"), na.rm = TRUE),0),"Days"),
          subtitle = sprintf("%s - %s",reports_info()$observation_start,reports_info()$observation_end),
          icon = icon("magnifying-glass"),
          color = "warning",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_REPORT_TRIP_INDICATOR_NUMBER_OF_OPERATIONS"),
          value = tot_act()$ACTIVITY,
          subtitle = sprintf("%s %s",tot_act()$OBSERVED,i18n("OBSERVER_REPORT_TRIP_INDICATOR_WITH_OBSERVER")),
          icon = icon("anchor"),
          color = "warning",
          width = 4
        )
      )
    })
    
    output$second_line_indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = i18n("OBSERVER_REPORT_TRIP_INDICATOR_TOTAL_LANDED"),
          value = paste0(round(tot_landed()$QUANTITY,0)," ",tot_landed()$UNIT),
          subtitle = sprintf("%s %s",tot_landed()$SP,i18n("OBSERVER_REPORT_TRIP_INDICATOR_SPECIES")),
          icon = icon("scale-balanced"),
          color = "info",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_REPORT_TRIP_INDICATOR_TOTAL_DISCARD"),
          value = paste0(round(tot_discard()$QUANTITY,0)," ",tot_discard()$UNIT),
          subtitle = sprintf("%s %s",tot_discard()$SP,i18n("OBSERVER_REPORT_TRIP_INDICATOR_SPECIES")),
          icon = icon("trash"),
          color = "info",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_REPORT_TRIP_INDICATOR_NUMBER_OF_SPECIES"),
          value = tot_sp()$SP,
          subtitle = sprintf("%s %s",tot_sp()$ETP,i18n("OBSERVER_REPORT_TRIP_INDICATOR_ETP_SPECIES")),
          icon = icon("fish"),
          color = "info",
          width = 4
        )
      )
    })
    
    #Trip Tab - Table
    
    icon_user_status <- function(value){
      if (is.na(value)) paste0(as.character(icon("triangle-exclamation", style = "color:darkorange;"))," ",i18n("OBSERVER_REPORT_UNKNOWN"))
      else if (value == 1) paste0(as.character(icon("user", style="color:green;"))," ",i18n("OBSERVER_REPORT_OBS_PRESENT"))
      else paste0(as.character(icon("user-slash", style="color:red;"))," ",i18n("OBSERVER_REPORT_OBS_ABSENT"))
    }
    
    icon_day_night <- function(value){
      if (is.na(value)) paste0(as.character(icon("triangle-exclamation", style = "color:darkorange;"))," ",i18n("OBSERVER_REPORT_UNKNOWN"))
      else if (value == 1) paste0(as.character(icon("sun", style="color:gold;"))," ",i18n("OBSERVER_REPORT_DAY"))
      else paste0(as.character(icon("moon", style="color:darkblue;"))," ",i18n("OBSERVER_REPORT_NIGHT"))
    }
    
    icon_etp <- function(value){
      if (is.na(value) || value == 0) ""     
      else paste0(as.character(icon("shield", style="color:red;"))," ",i18n("OBSERVER_REPORT_ETP"))
    }
    
    format_species <- function(name, sci_name, code){
      sprintf("[%s] %s (%s)", code, name, sci_name)
    }
    
    output$activities_table <- DT::renderDT({
      
      df <- report_activities()
      
      df_formatted <- df %>%
        mutate(
          OBSERVER_PRESENT = purrr::map_chr(OBSERVER_PRESENT, ~ as.character(icon_user_status(.x))),
          IS_DAYTIME       = purrr::map_chr(IS_DAYTIME, ~ as.character(icon_day_night(.x))),
          SPECIES          = purrr::pmap_chr(
            list(SP_NAME, SP_SCIENTIFIC_NAME, SP_CODE),
            ~ format_species(..1, ..2, ..3)
          ),
          ETP = purrr::map_chr(SP_IS_ETP, ~ as.character(icon_etp(.x))),
          LANDED_QUANTITY = purrr::pmap_chr(
            list(LANDED_QUANTITY, LANDED_QUANTITY_UNIT),
            ~ if (is.na(..1)) {
              as.character(icon("triangle-exclamation", style = "color:darkorange;"))
            } else if (is.na(..2) || ..2 == "") {
              as.character(round(..1, 2))
            } else {
              paste0(round(..1, 2), " ", ..2)
            }
          ),
          
          DISCARD_QUANTITY = purrr::pmap_chr(
            list(DISCARD_QUANTITY, DISCARD_QUANTITY_UNIT),
            ~ if (is.na(..1)) {
              as.character(icon("triangle-exclamation", style = "color:darkorange;"))
            } else if (is.na(..2) || ..2 == "") {
              as.character(round(..1, 2))
            } else {
              paste0(round(..1, 2), " ", ..2)
            }
          )
        ) %>%
        select(
          ACTIVITY_ID, OBSERVER_PRESENT, DATE_FROM, DATE_TO, IS_DAYTIME,
          GEAR, SPECIES, ETP, LANDED_QUANTITY, DISCARD_QUANTITY
        )
      
      datatable(
        df_formatted,
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          columnDefs = list(
            list(width = '80px', targets = c(0,1,3,4)),
            list(width = '120px', targets = c(5,6,7))
          )
        ),
        colnames = c(
          i18n("OBSERVER_REPORT_TRIP_ACTIVITY_ID"),
          i18n("OBSERVER_REPORT_TRIP_OBSERVER_PRESENT"),
          i18n("OBSERVER_REPORT_TRIP_FROM"),
          i18n("OBSERVER_REPORT_TRIP_TO"),
          i18n("OBSERVER_REPORT_TRIP_DAYTIME"),
          i18n("OBSERVER_REPORT_TRIP_GEAR"),
          i18n("OBSERVER_REPORT_TRIP_SPECIES"),
          i18n("OBSERVER_REPORT_TRIP_ETP"),     
          i18n("OBSERVER_REPORT_TRIP_LANDED_QUANTITY"),
          i18n("OBSERVER_REPORT_TRIP_DISCARD_QUANTITY")
        )
      )
    })
    
    #Trip Tab - Map
    output$map <- renderLeaflet({
      
      d <- report_activities_sum() %>%
        filter(!is.na(LONGITUDE), !is.na(LATITUDE))
      
      req(nrow(d) > 0)
      
      leaflet(
        d,
        options = leafletOptions(
          minZoom = 5,
          maxZoom = 12,
          worldCopyJump = TRUE
        )
      ) %>%
        addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
        
        addLayersControl(
          baseGroups = c("Ocean", "Light", "OSM"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        addCircleMarkers(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          radius = 6,
          fillOpacity = 0.8,
          stroke = FALSE,
          color = ~ifelse(
            OBSERVER_PRESENT == 1, "#ff8c00",
            ifelse(OBSERVER_PRESENT == 0, "#4682b4", "#999999")
          ),
          popup = ~paste0(
            "<b>", i18n("OBSERVER_REPORT_TRIP_ACTIVITY"), " #", ACTIVITY_ID, "</b><br/>",
            i18n("OBSERVER_REPORT_TRIP_FROM"), ": ", DATE_FROM, "<br/>",
            i18n("OBSERVER_REPORT_TRIP_TO"), ": ", DATE_TO
          )
        ) %>%
        fitBounds(
          lng1 = min(d$LONGITUDE),
          lat1 = min(d$LATITUDE),
          lng2 = max(d$LONGITUDE),
          lat2 = max(d$LATITUDE)
        )
    })
    
    #Trip Tab - Plot
    prepare_bar_species <- function(df, top_n = 10) {
      
      df %>%
        mutate(
          LANDED_QUANTITY  = replace_na(LANDED_QUANTITY, 0),
          DISCARD_QUANTITY = replace_na(DISCARD_QUANTITY, 0),
          SPECIES = ifelse(
            is.na(SP_NAME) | SP_NAME == "",
            i18n("OBSERVER_REPORT_UNKNOWN"),
            SP_NAME
          )
        ) %>%
        group_by(SPECIES) %>%
        summarise(
          LANDED  = sum(LANDED_QUANTITY, na.rm = TRUE),
          DISCARD = sum(DISCARD_QUANTITY, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(TOTAL = LANDED + DISCARD) %>%
        arrange(desc(TOTAL)) %>%
        mutate(
          SPECIES_GROUP = ifelse(
            row_number() <= top_n,
            SPECIES,
            i18n("OBSERVER_REPORT_TRIP_OTHER_SPECIES")
          )
        ) %>%
        group_by(SPECIES_GROUP) %>%
        summarise(
          LANDED  = sum(LANDED),
          DISCARD = sum(DISCARD),
          .groups = "drop"
        )
    }
    
    prepare_bar_long <- function(df) {
      df %>%
        tidyr::pivot_longer(
          cols = c(LANDED, DISCARD),
          names_to = "TYPE",
          values_to = "QUANTITY"
        ) %>%
        mutate(
          TYPE_LABEL = recode(
            TYPE,
            LANDED  = i18n("OBSERVER_REPORT_TRIP_LANDED"),
            DISCARD = i18n("OBSERVER_REPORT_TRIP_DISCARD")
          )
        )
    }
    
    output$species_barplot <- renderPlotly({
      
      df <- prepare_bar_species(report_activities(), top_n = 10)
      df_long <- prepare_bar_long(df)
      
      req(nrow(df_long) > 0)
      
      plotly::plot_ly(
        data = df_long,
        x = ~QUANTITY,
        y = ~TYPE_LABEL,
        color = ~SPECIES_GROUP,
        type = "bar",
        orientation = "h",
        hovertemplate = paste(
          "<b>%{color}</b><br>",
          "%{y}: %{x:.2f}<extra></extra>"
        )
      ) %>%
        layout(
          barmode = "stack",
          xaxis = list(
            title = i18n("OBSERVER_REPORT_TRIP_TOTAL_QUANTITY")
          ),
          yaxis = list(
            title = ""
          ),
          legend = list(
            title = list(
              text = i18n("OBSERVER_REPORT_TRIP_SPECIES")
            )
          ),
          margin = list(l = 120, r = 20, t = 20, b = 40)
        )
    })
    
    #Trip Tab - content wrapper
    output$trip_content <-renderUI({
      tagList(
        tags$h4(i18n("OBSERVER_REPORT_TRIP_TITLE")),
        plotlyOutput(ns("activities_timeline"), height = "160px"),
        uiOutput(ns("first_line_indicators")),
        uiOutput(ns("second_line_indicators")),
        DT::dataTableOutput(ns("activities_table")),
        fluidRow(
          column(6,
                 box(title=i18n("OBSERVER_REPORT_TRIP_MAP_TITLE"),leafletOutput(ns("map"), height = "400px"),width=12)
          ),
          column(6,
                 box(title=i18n("OBSERVER_REPORT_TRIP_PLOT_TITLE"),plotlyOutput(ns("species_barplot"), height = "400px"),width=12)
          )
        )
      )
    })
    
    # --- Logbook Tab ---
    #Logbook Tab - content wrapper
    output$logbook_content <-renderUI({
      tagList(
        tags$h4(i18n("OBSERVER_REPORT_LOGBOOK_TITLE")),
        uiOutput(ns("logbook_status"))
      )
    })
    
    #Logbook Tab - message
    output$logbook_status <- renderUI({
      
      req(report_logbook)
      
      linked <- report_logbook()$LINKED_TO_LOGBOOK
      logbook_id <- report_logbook()$LOGBOOK_ID
      
      tags$div(
        style = "font-size:20px;",
        
        if (is.na(linked)) {
          
          tagList(
            icon("exclamation-triangle", class = "text-warning fa-2x"),
            tags$span(i18n("OBSERVER_REPORT_UNKNOWN"))
          )
          
        } else if (linked == 1) {
          
          tagList(
            icon("check-circle", class = "text-success fa-2x"),
            tags$span(" ",sprintf(i18n("OBSERVER_REPORT_LOGBOOK_LINKED"),logbook_id))
          )
          
        } else {
          
          tagList(
            icon("times-circle", class = "text-danger fa-3x"),
            tags$span(" ",i18n("OBSERVER_REPORT_LOGBOOK_NOT_LINKED"))
          )
        }
      )
    })
    
    
    # ---Result tabset---
    
    output$results <- renderUI({
      tagList(
        h4(paste0(i18n("OBSERVER_REPORT_REPORT_SELECTED"), ": ", "#",report_selected())),
      tabsetPanel(
        tabPanel(i18n("OBSERVER_REPORT_DESCRIPTION_TAB_TITLE"),uiOutput(ns("description_content"))),
        tabPanel(i18n("OBSERVER_REPORT_VESSEL_TAB_TITLE"),uiOutput(ns("vessel_content"))),
        tabPanel(i18n("OBSERVER_REPORT_TRIP_TAB_TITLE"),uiOutput(ns("trip_content"))),
        tabPanel(i18n("OBSERVER_REPORT_LOGBOOK_TAB_TITLE"),uiOutput(ns("logbook_content")))
      )
      )
    })
  })
}
