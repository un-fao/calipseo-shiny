#artfish_overview_server
artfish_overview_server <- function(id, parent.session, pool, reloader){

 moduleServer(id, function(input, output, session){   
    
  INFO("artfish-overview: START")
  MODULE_START_TIME <- Sys.time()
  
  ns<-session$ns
  
  data_bg<-reactiveVal(NULL)
  
  ref_species <- accessRefSpecies(pool)
  ref_fishing_units <- accessRefFishingUnits(pool)
  
  files <- getStatPeriods(config = appConfig, "artfish_estimates",target = "release")
  
  output$no_release<-renderUI({
    div(
      if(nrow(files)>0){
        NULL
      }else{
        p(i18n("ARTFISH_OVERVIEW_NO_RELEASE"))
      }
    )
  })
  
  req(nrow(files) > 0)
  
  estimate <- do.call(rbind,lapply(files$file, readr::read_csv))

  estimate <- estimate %>%
    merge(ref_fishing_units %>%
    select(ID,NAME) %>%
    rename(fishing_unit = ID,
           fishing_unit_label = NAME)
    ) %>%
    ungroup()
  
  estimate <- estimate %>%
    merge(ref_species %>%
            select(ID,NAME) %>%
            rename(species = ID,
                   species_label = NAME)
    )%>%
    ungroup()
  
  estimate <-estimate%>%
    mutate(date = as.Date(sprintf("%04d-%02d-01",year,month)))
  
  data_bg(estimate)

  output$time_selector <- renderUI({
    
  sliderInput(
    ns("time"),
    label = i18n("ARTFISH_OVERVIEW_TIME_SLIDER_LABEL"),
    min = min(estimate$date, na.rm = TRUE),
    max = max(estimate$date, na.rm = TRUE),
    value = c(
      min(estimate$date, na.rm = TRUE),
      max(estimate$date, na.rm = TRUE)
    ),
    timeFormat = "%b %Y"
  )
  
  })
  
  output$fishing_unit_selector <- renderUI({
    
    bg_ids <- unique(estimate$fishing_unit)
    ref_bg_sp <- subset(ref_fishing_units, ID %in% bg_ids)
    
    choices <- setNames(ref_bg_sp$ID, ref_bg_sp$NAME)
    
    shinyWidgets::pickerInput(
      inputId = ns("fishing_unit"),
      label   = i18n("ARTFISH_OVERVIEW_FISHING_UNIT_SELECTOR_LABEL"),
      choices = choices,
      selected = ref_bg_sp$ID,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `select-all-text` = i18n("ARTFISH_OVERVIEW_FISHING_UNIT_SELECTOR_SELECT_ALL"),
        `deselect-all-text` = i18n("ARTFISH_OVERVIEW_FISHING_UNIT_SELECTOR_DESELECT_ALL"),
        `selected-text-format` = "count > 3",
        `count-selected-text` = paste0("{0} ",i18n("ARTFISH_OVERVIEW_FISHING_UNIT_SELECTOR_SELECTED")),
        `none-selected-text` = i18n("ARTFISH_OVERVIEW_FISHING_UNIT_SELECTOR_NO_SELECTION"),
        `live-search` = TRUE
      )
    )
  })
  
  output$filter_selectors <- renderUI({
    fluidRow(
      column(4,uiOutput(ns("time_selector")),offset = 1),
      column(4,uiOutput(ns("fishing_unit_selector")),offset = 1)
    )
  })
  
  observeEvent(c(input$fishing_unit,input$time), {
    
    req(!is.null(estimate))
    
    data<-estimate%>%
           filter(
           date >= input$time[1],
           date <= input$time[2]
         )
    
    if (length(input$fishing_unit) == 0) {
      selection <- data[0, ]
    } else {
      selection <- subset(
        data,
        fishing_unit %in% input$fishing_unit
      )
    }

    selection <- selection[, c(
      "year",
      "month",
      "date",
      "fishing_unit",
      "fishing_unit_label",
      "species_label",
      "effort_nominal",
      "fleet_engagement_number",
      "catch_nominal_landed",
      "trade_value",
      "catch_cpue"
    )]
    
    data_bg(selection)
  })
  
  
  observeEvent(data_bg(),{
    req(!is.null(data_bg()))
    data <- data_bg()%>%
      ungroup()
    
    data_effort<-data%>%
                  select(date,fishing_unit,fishing_unit_label,effort_nominal,fleet_engagement_number) %>%
                  distinct() %>%
                  ungroup()
    
    total_effort<-data_effort%>%
                  summarise(effort_nominal=sum(effort_nominal,na.rm=T))
    
    total_catch<-data%>%
      summarise(catch_nominal_landed=sum(catch_nominal_landed,na.rm=T),
                trade_value=sum(trade_value,na.rm=T))
    
    output$indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = i18n("ARTFISH_OVERVIEW_INFOBOX_CATCH_TITLE"),
          value = sprintf("%s (%s)",formatC(total_catch$catch_nominal_landed, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_OVERVIEW_INFOBOX_CATCH_UNIT")),
          icon = icon("fish"),
          color = "primary",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("ARTFISH_OVERVIEW_INFOBOX_EFFORT_TITLE"),
          value = sprintf("%s (%s)",formatC(total_effort$effort_nominal, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_OVERVIEW_INFOBOX_EFFORT_UNIT")),
          icon = icon("clock"),
          color = "primary",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("ARTFISH_OVERVIEW_INFOBOX_VALUE_TITLE"),
          value = sprintf("%s (%s)",formatC(total_catch$trade_value, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_OVERVIEW_INFOBOX_VALUE_UNIT")),
          icon = icon("clock"),
          color = "primary",
          width = 4
        )
      )
    })
    
    #load artfish linechart servers
    #-> fleet engagement
    # artfish_line_chart_server(
    #   id = "boats", 
    #   label = i18n("ARTFISH_BOAT_GEAR_LABEL"),
    #   df = data_effort, colDate = "date", colTarget = "fishing_unit_label", colValue="fleet_engagement_number",
    #   ylab = i18n("ARTFISH_NUMBER_OF_BOATS_LABEL"), levels = level_choices, 
    #   stat = "sum", rank = TRUE,
    #   mode='plot+table',
    #   prefered_colnames = c(i18n("ARTFISH_TABLE_COLNAME_date"),i18n("ARTFISH_TABLE_COLNAME_AGG"),i18n("ARTFISH_TABLE_COLNAME_BOAT_GEAR"))
    #)
    
    generic_chart_server(
      id = "boats",
      df = data_effort,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "fleet_engagement_number",
      time_label = "",
      value_label = i18n("ARTFISH_OVERVIEW_PLOT_BOATS_VALUE_LABEL"),
      group_label = i18n("ARTFISH_OVERVIEW_PLOT_BOATS_GROUP_LABEL"),
      stat = "sum"
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
    )
    #-> effort
    # artfish_line_chart_server(
    #   id = "effort", 
    #   label=i18n("ARTFISH_BOAT_GEAR_LABEL"),
    #   df = data_effort, colDate = "date", colTarget = "fishing_unit_label", colValue = "effort_nominal",
    #   ylab = i18n("ARTFISH_EFFORT_DAYS_LABEL"), levels = level_choices,
    #   stat = "sum", rank = TRUE,
    #   mode = 'plot+table',
    #   prefered_colnames = c(i18n("ARTFISH_TABLE_COLNAME_date"),i18n("ARTFISH_TABLE_COLNAME_AGG"),i18n("ARTFISH_TABLE_COLNAME_BOAT_GEAR"))
    # )
    generic_chart_server(
      id = "effort",
      df = data_effort,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "effort_nominal",
      time_label = "",
      value_label = i18n("ARTFISH_OVERVIEW_PLOT_EFFORT_VALUE_LABEL"),
      group_label = i18n("ARTFISH_OVERVIEW_PLOT_EFFORT_GROUP_LABEL"),
      stat = "sum"
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct"),
      #time_choices = c("month")
    )
    
    #-> catch
    # artfish_line_chart_server(
    #   id = "catch",
    #   label = i18n("ARTFISH_SPECIES_LABEL"),
    #   df = data, colDate = "date",
    #   colTarget = "species_label", colValue = "catch_nominal_landed",
    #   ylab = i18n("ARTFISH_CATCH_LABEL"), levels = level_choices,
    #   stat = "sum", rank = TRUE,
    #   mode = 'plot+table',
    #   prefered_colnames = c(i18n("ARTFISH_TABLE_COLNAME_date"),i18n("ARTFISH_TABLE_COLNAME_AGG"),i18n("ARTFISH_TABLE_COLNAME_SPECIES"))
    # )
    
    generic_chart_server(
      id = "catch",
      df = data,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "catch_nominal_landed",
      stat = "sum",
      time_label = "",
      value_label = i18n("ARTFISH_OVERVIEW_PLOT_CATCH_VALUE_LABEL"),
      group_label = i18n("ARTFISH_OVERVIEW_PLOT_CATCH_GROUP_LABEL"),
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
    )
    
    generic_chart_server(
      id = "catch_sp",
      df = data,
      col_date = "date",
      col_group = "species_label",
      col_value = "catch_nominal_landed",
      stat = "sum",
      time_label = "",
      value_label = i18n("ARTFISH_OVERVIEW_PLOT_CATCH_SP_VALUE_LABEL"),
      group_label = i18n("ARTFISH_OVERVIEW_PLOT_CATCH_SP_GROUP_LABEL"),
      plot_types = c("rank_sum")
    )
    
    generic_chart_server(
      id = "value_sp",
      df = data,
      col_date = "date",
      col_group = "species_label",
      col_value = "trade_value",
      stat = "sum",
      time_label = "",
      value_label = i18n("ARTFISH_OVERVIEW_PLOT_VALUE_SP_VALUE_LABEL"),
      group_label = i18n("ARTFISH_OVERVIEW_PLOT_VALUE_SP_GROUP_LABEL"),
      plot_types = c("rank_sum")
    )
    
    #-> Trade value
    # artfish_line_chart_server(
    #   id = "value", 
    #   label = i18n("ARTFISH_SPECIES_LABEL"),
    #   df = data, colDate = "date", 
    #   colTarget = "species_label", colValue = "trade_value",
    #   ylab = i18n("ARTFISH_VALUE_LABEL"), levels = level_choices,
    #   stat = "sum", rank = TRUE,
    #   mode = 'plot+table',
    #   prefered_colnames = c(i18n("ARTFISH_TABLE_COLNAME_date"),i18n("ARTFISH_TABLE_COLNAME_AGG"),i18n("ARTFISH_TABLE_COLNAME_SPECIES"))
    # )
    
    generic_chart_server(
      id = "value",
      df = data,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "trade_value",
      time_label = "",
      value_label = i18n("ARTFISH_OVERVIEW_PLOT_VALUE_VALUE_LABEL"),
      group_label = i18n("ARTFISH_OVERVIEW_PLOT_VALUE_GROUP_LABEL"),
      stat = "sum"
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
    )

    
    #-> CPUE
    # artfish_line_chart_server(
    #   id = "cpue", 
    #   label = i18n("ARTFISH_SPECIES_LABEL"),
    #   df = data, colDate = "date",
    #   colTarget = "species_label", colValue = "catch_cpue",
    #   ylab = i18n("ARTFISH_CPUE_LABEL"), levels = level_choices,
    #   stat = "mean", rank = TRUE,
    #   mode = 'plot+table',
    #   prefered_colnames = c(i18n("ARTFISH_TABLE_COLNAME_date"),i18n("ARTFISH_TABLE_COLNAME_AGG"),i18n("ARTFISH_TABLE_COLNAME_SPECIES"))
    # )
    
    generic_chart_server(
      id = "cpue",
      df = data,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "catch_cpue",
      time_label = "",
      value_label = i18n("ARTFISH_OVERVIEW_PLOT_CPUE_VALUE_LABEL"),
      group_label = i18n("ARTFISH_OVERVIEW_PLOT_CPUE_GROUP_LABEL"),
      stat = "mean",
      time_choices = "month"
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
    )
    
    output$results<-renderUI({
    # tagList(
    #   fluidRow(
    #     column(6,
    #            #artfish_line_chart_ui(ns("boats"),title=i18n("ARTFISH_TITLE_CUMULATE_NUMBER_OF_BOATS"),sliderWidth =25)
    #     ),
    #     column(6,
    #            #artfish_line_chart_ui(ns("effort"),title=i18n("ARTFISH_TITLE_CUMULATE_EFFORT"),sliderWidth =25)
    #     )
    #   ),
    #   fluidRow(
    #     column(6,
    #       #artfish_line_chart_ui(ns("catch"),title=i18n("ARTFISH_TITLE_CUMULATE_CATCH"),sliderWidth =25)
    #     ),
    #     column(6,
    #       #artfish_line_chart_ui(ns("value"),title=i18n("ARTFISH_TITLE_CUMULATE_VALUE"),sliderWidth =25)
    #     )
    #     ),
    #   fluidRow(
    #     column(6,        
    #       #artfish_line_chart_ui(ns("cpue"),title=i18n("ARTFISH_AVERAGE_CPUE"),sliderWidth =25)
    #      )  ,
    #     column(6,        
    #            generic_chart_ui(ns("effort2"))
    #     )  
    #   )
    # )
      
      tagList(
        fluidRow(
          column(6,
                 generic_chart_ui(ns("catch"),title=i18n("ARTFISH_OVERVIEW_PLOT_CATCH_TITLE"),sliderWidth =25)
          ),
          column(6,
                 generic_chart_ui(ns("catch_sp"),title=i18n("ARTFISH_OVERVIEW_PLOT_CATCH_SP_TITLE"),sliderWidth =25)
          )
        ),
        fluidRow(
          column(6,
                 generic_chart_ui(ns("effort"),title=i18n("ARTFISH_OVERVIEW_PLOT_EFFORT_TITLE"),sliderWidth =25)
          ),
          column(6,
                 generic_chart_ui(ns("boats"),title=i18n("ARTFISH_OVERVIEW_PLOT_BOATS_TITLE"),sliderWidth =25)
          )
        ),
        fluidRow(
          column(6,
                 generic_chart_ui(ns("value"),title=i18n("ARTFISH_OVERVIEW_PLOT_VALUE_TITLE"),sliderWidth =25)
          ),
          column(6,
                 generic_chart_ui(ns("value_sp"),title=i18n("ARTFISH_OVERVIEW_PLOT_VALUE_SP_TITLE"),sliderWidth =25)
          )
        ),
        fluidRow(
          column(12,        
                 generic_chart_ui(ns("cpue"),title=i18n("ARTFISH_OVERVIEW_PLOT_CPUE_TITLE"),sliderWidth =25)
          )
        )
      )
    })
  })
  
  MODULE_END_TIME <- Sys.time()
  INFO("artfish-overview: END")
  DEBUG_MODULE_PROCESSING_TIME("Artfish-overview", MODULE_START_TIME, MODULE_END_TIME)
  
  
 })
  
}