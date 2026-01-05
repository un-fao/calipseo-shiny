#artfish_fishing_unit_server
artfish_fishing_unit_server <- function(id, parent.session, pool, reloader){
  
  moduleServer(id, function(input, output, session){   
  
    INFO("artfish-fishing_unit: START")
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
          p(i18n("ARTFISH_FISHING_UNIT_NO_RELEASE"))
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
    
    print("ARTFISH OUTPUT")
    print(names(estimate))
    
    data_bg(estimate)
    
    output$time_selector <- renderUI({
      
      sliderInput(
        ns("time"),
        label = i18n("ARTFISH_FISHING_UNIT_TIME_SLIDER_LABEL"),
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
        label   = i18n("ARTFISH_FISHING_UNIT_FISHING_UNIT_SELECTOR_LABEL"),
        choices = choices,
        selected = ref_bg_sp$ID,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `select-all-text` = i18n("ARTFISH_FISHING_UNIT_FISHING_UNIT_SELECTOR_SELECT_ALL"),
          `deselect-all-text` = i18n("ARTFISH_FISHING_UNIT_FISHING_UNIT_SELECTOR_DESELECT_ALL"),
          `selected-text-format` = "count > 3",
          `count-selected-text` = paste0("{0} ",i18n("ARTFISH_FISHING_UNIT_FISHING_UNIT_SELECTOR_SELECTED")),
          `none-selected-text` = i18n("ARTFISH_FISHING_UNIT_FISHING_UNIT_SELECTOR_NO_SELECTION"),
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
        "effort_activity_coefficient",
        "effort_total_fishing_duration",
        "fleet_engagement_number",
        "catch_nominal_landed",
        "catch_nominal_landed_sampled",
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
        select(date,fishing_unit,fishing_unit_label,effort_nominal,fleet_engagement_number,effort_activity_coefficient,effort_total_fishing_duration) %>%
        distinct() %>%
        ungroup()
      
      total_effort<-data_effort%>%
        summarise(effort_nominal=sum(effort_nominal,na.rm=T),
                  fleet_engagement_number=sum(fleet_engagement_number,na.rm=T)
                  )
  
      total_catch<-data%>%
        summarise(catch_nominal_landed=sum(catch_nominal_landed,na.rm=T),
                  trade_value=sum(trade_value,na.rm=T)
                  )
      
      
      output$indicators <- renderUI({
        fluidRow(
          bs4InfoBox(
            title = i18n("ARTFISH_FISHING_UNIT_INFOBOX_CATCH_TITLE"),
            value = sprintf("%s (%s)",formatC(total_catch$catch_nominal_landed, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_FISHING_UNIT_INFOBOX_CATCH_UNIT")),
            icon = icon("fish"),
            color = "primary",
            width = 3
          ),
          bs4InfoBox(
            title = i18n("ARTFISH_FISHING_UNIT_INFOBOX_VALUE_TITLE"),
            value = sprintf("%s (%s)",formatC(total_catch$trade_value, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_FISHING_UNIT_INFOBOX_VALUE_UNIT")),
            icon = icon("fish"),
            color = "primary",
            width = 3
          ),
          bs4InfoBox(
            title = i18n("ARTFISH_FISHING_UNIT_INFOBOX_EFFORT_TITLE"),
            value = sprintf("%s (%s)",formatC(total_effort$effort_nominal, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_FISHING_UNIT_INFOBOX_EFFORT_UNIT")),
            icon = icon("clock"),
            color = "primary",
            width = 3
          ),
          bs4InfoBox(
            title = i18n("ARTFISH_FISHING_UNIT_INFOBOX_BOAT_TITLE"),
            value = sprintf("%s (%s)",formatC(total_effort$fleet_engagement_number, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_FISHING_UNIT_INFOBOX_BOAT_UNIT")),
            icon = icon("clock"),
            color = "primary",
            width = 3
          )
        )
      })

      generic_chart_server(
        id = "catch_fu_tot",
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "catch_nominal_landed",
        stat = "sum",
        time_label = "",
        value_label = i18n("ARTFISH_FISHING_UNIT_PLOT_CATCH_FU_TOT_VALUE_LABEL"),
        group_label = i18n("ARTFISH_FISHING_UNIT_PLOT_CATCH_FU_TOT_GROUP_LABEL"),
        plot_types = c("rank_sum","donut")
      )
      
      generic_chart_server(
        id = "value_fu_tot",
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "trade_value",
        stat = "sum",
        time_label = "",
        value_label = i18n("ARTFISH_FISHING_UNIT_PLOT_VALUE_FU_TOT_VALUE_LABEL"),
        group_label = i18n("ARTFISH_FISHING_UNIT_PLOT_VALUE_FU_TOT_GROUP_LABEL"),
        plot_types = c("rank_sum","donut")
      )
      
      generic_chart_server(
        id = "catch_sp_tot",
        df = data,
        col_date = "date",
        col_group = "species_label",
        col_value = "catch_nominal_landed",
        stat = "sum",
        time_label = "",
        value_label = i18n("ARTFISH_FISHING_UNIT_PLOT_CATCH_SP_TOT_VALUE_LABEL"),
        group_label = i18n("ARTFISH_FISHING_UNIT_PLOT_CATCH_SP_TOT_GROUP_LABEL"),
        plot_types = c("rank_sum","donut")
      )
      
      generic_chart_server(
        id = "catch",
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "catch_nominal_landed",
        stat = "sum",
        time_label = "",
        value_label = i18n("ARTFISH_FISHING_UNIT_PLOT_CATCH_VALUE_LABEL"),
        group_label = i18n("ARTFISH_FISHING_UNIT_PLOT_CATCH_GROUP_LABEL"),
        #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
      )
      
      generic_chart_server(
        id = "cpue",
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "catch_cpue",
        time_label = "",
        value_label = i18n("ARTFISH_FISHING_UNIT_PLOT_CPUE_VALUE_LABEL"),
        group_label = i18n("ARTFISH_FISHING_UNIT_PLOT_CPUE_GROUP_LABEL"),
        stat = "mean",
        time_choices = "month"
        #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
      )
      
      generic_chart_server(
        id = "effort",
        df = data_effort,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "effort_nominal",
        time_label = "",
        value_label = i18n("ARTFISH_FISHING_UNIT_PLOT_EFFORT_VALUE_LABEL"),
        group_label = i18n("ARTFISH_FISHING_UNIT_PLOT_EFFORT_GROUP_LABEL"),
        stat = "sum"
        #plot_types = c("line","line_cumulate","area_stack","area_stack_pct"),
        #time_choices = c("month")
      )
      
      generic_chart_server(
        id = "activity",
        df = data_effort,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "effort_activity_coefficient",
        time_label = "",
        value_label = i18n("ARTFISH_FISHING_UNIT_PLOT_ACTIVITY_VALUE_LABEL"),
        group_label = i18n("ARTFISH_FISHING_UNIT_PLOT_ACTIVITY_GROUP_LABEL"),
        stat = "sum"
        #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
      )
      
      generic_chart_server(
        id = "boats",
        df = data_effort,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "fleet_engagement_number",
        time_label = "",
        value_label = i18n("ARTFISH_FISHING_UNIT_PLOT_BOATS_VALUE_LABEL"),
        group_label = i18n("ARTFISH_FISHING_UNIT_PLOT_BOATS_GROUP_LABEL"),
        stat = "sum"
        #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
      )
      
      generic_chart_server(
        id = "value",
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "trade_value",
        time_label = "",
        value_label = i18n("ARTFISH_FISHING_UNIT_PLOT_VALUE_VALUE_LABEL"),
        group_label = i18n("ARTFISH_FISHING_UNIT_PLOT_VALUE_GROUP_LABEL"),
        stat = "sum"
        #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
      )
      

      
      output$results<-renderUI({
        
        tagList(
          fluidRow(
            column(4,
                   generic_chart_ui(ns("catch_sp_tot"),title=i18n("ARTFISH_FISHING_UNIT_PLOT_CATCH_SP_TOT_TITLE"),sliderWidth =25)
            ),
            column(4,
                   generic_chart_ui(ns("catch_fu_tot"),title=i18n("ARTFISH_FISHING_UNIT_PLOT_CATCH_FU_TOT_TITLE"),sliderWidth =25)
            ),
            column(4,
                   generic_chart_ui(ns("value_fu_tot"),title=i18n("ARTFISH_FISHING_UNIT_PLOT_VALUE_FU_TOT_TITLE"),sliderWidth =25)
            )
          ),
          fluidRow(generic_chart_ui(ns("catch"),title=i18n("ARTFISH_FISHING_UNIT_PLOT_CATCH_TITLE"),sliderWidth =25)),
          fluidRow(generic_chart_ui(ns("cpue"),title=i18n("ARTFISH_FISHING_UNIT_PLOT_CPUE_TITLE"),sliderWidth =25)),
          fluidRow(generic_chart_ui(ns("effort"),title=i18n("ARTFISH_FISHING_UNIT_PLOT_EFFORT_TITLE"),sliderWidth =25)),
          fluidRow(generic_chart_ui(ns("activity"),title=i18n("ARTFISH_FISHING_UNIT_PLOT_ACTIVITY_TITLE"),sliderWidth =25)),
          fluidRow(generic_chart_ui(ns("boats"),title=i18n("ARTFISH_FISHING_UNIT_PLOT_BOATS_TITLE"),sliderWidth =25)),
          fluidRow(generic_chart_ui(ns("value"),title=i18n("ARTFISH_FISHING_UNIT_PLOT_VALUE_TITLE"),sliderWidth =25))
        )
      })
    })
    
    MODULE_END_TIME <- Sys.time()
    INFO("artfish-unit: END")
    DEBUG_MODULE_PROCESSING_TIME("Artfish-fishing_unit", MODULE_START_TIME, MODULE_END_TIME)
    
  })  
  
}