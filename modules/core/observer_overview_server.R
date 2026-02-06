#observer_overview_server
observer_overview_server <- function(id,parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    reports_info <- accessObserverReportsSummary(pool)
    
    reports_info$embarkation_start <- as.POSIXct(as.character(reports_info$embarkation_start))
    attr(reports_info$embarkation_start, "tzone") <- appConfig$country_profile$timezone
    reports_info$embarkation_end <- as.POSIXct(as.character(reports_info$embarkation_end))
    attr(reports_info$embarkation_end, "tzone") <- appConfig$country_profile$timezone
    reports_info$observation_start <- as.POSIXct(as.character(reports_info$observation_start))
    attr(reports_info$observation_start, "tzone") <- appConfig$country_profile$timezone
    reports_info$observation_end <- as.POSIXct(as.character(reports_info$observation_end))
    attr(reports_info$observation_end, "tzone") <- appConfig$country_profile$timezone
    reports_info$year<-year(reports_info$embarkation_end)
    
    current_year<-Sys.time()
    current_year<- as.POSIXct(as.character(current_year))
    attr(current_year, "tzone") <- appConfig$country_profile$timezone
    current_year<-year(current_year)
    
    
    safe_summarise <- function(df, year_filter = NULL) {
      df_filtered <- df
      if(!is.null(year_filter)) df_filtered <- df %>% filter(year == year_filter)
      
      if(nrow(df_filtered) == 0) {
        return(tibble(
          nb_reports   = 0,
          nb_vessels   = 0,
          nb_observers = 0,
          total_obs_days = 0,
          last_report  = as.Date(NA),
          total_catch  = 0,
          catch_unit   =NA_character_,
          average_catch = NA_real_,
          obs_period  = NA_character_
        ))
      }
      
      df_filtered %>%
        summarise(
          nb_reports      = n_distinct(report_id),
          nb_vessels      = n_distinct(vessel_id),
          nb_observers    = n_distinct(observer_name),
          total_obs_days  = sum(as.numeric(observation_end - observation_start, units = "days"), na.rm = TRUE),
          last_report     = max(observation_end, na.rm = TRUE),
          total_catch     = sum(total_catch, na.rm = TRUE),
          catch_unit      = unique(total_catch_unit),
          average_catch   = ifelse(n_distinct(report_id) > 0, sum(total_catch, na.rm = TRUE)/n_distinct(report_id), NA_real_),
          obs_period      = ifelse(year(min(observation_start, na.rm = TRUE))==year(max(observation_end, na.rm = TRUE)),
                                   year(min(observation_start, na.rm = TRUE)),
                                   paste0(
                                    year(min(observation_start, na.rm = TRUE)), " - ",
                                    year(max(observation_end, na.rm = TRUE))
                                  )
                            )
        )
    }
    
    safe_summarise_logbook <- function(df, year_filter = NULL) {
      df_filtered <- df
      if(!is.null(year_filter)) df_filtered <- df %>% filter(year == year_filter)
      
      if(nrow(df_filtered) == 0) {
        return(tibble(
          total_catch  = 0,
          average_catch = NA_real_
        ))
      }
      
      df_filtered %>%
        summarise(
          total_catch     = sum(total_catch, na.rm = TRUE),
          average_catch   = ifelse(n_distinct(trip_id) > 0, sum(total_catch, na.rm = TRUE)/n_distinct(trip_id), NA_real_)
        )
    }
    
    agg_all <- safe_summarise(reports_info)
    agg_cy  <- safe_summarise(reports_info, year_filter = current_year)
    agg_ly  <- safe_summarise(reports_info, year_filter = current_year - 1)
    
    agg_all <- agg_all %>% mutate(across(everything(), as.character))
    agg_cy  <- agg_cy  %>% mutate(across(everything(), as.character))
    agg_ly  <- agg_ly  %>% mutate(across(everything(), as.character))
    
    merged_tbl <- agg_all %>%
      pivot_longer(everything(), names_to = "variable", values_to = "all") %>%
      left_join(agg_cy %>% pivot_longer(everything(), names_to = "variable", values_to = "c_year"), by = "variable") %>%
      left_join(agg_ly %>% pivot_longer(everything(), names_to = "variable", values_to = "l_year"), by = "variable") %>%
      mutate(
        evolution_pct = case_when(
          is.na(c_year) | is.na(l_year) ~ NA_character_,
          as.numeric(l_year) == 0 ~ NA_character_,
          TRUE ~ paste0(round((as.numeric(c_year) - as.numeric(l_year)) / as.numeric(l_year) * 100, 1), "%")
        )
      )
    
    merged_tbl <- merged_tbl %>%
      mutate(
        current_year_rounded = case_when(
          variable %in% c("total_obs_days", "total_catch", "average_catch") & !is.na(c_year) ~ as.character(round(as.numeric(c_year))),
          TRUE ~ as.character(c_year)
        ),
        
        display_text = case_when(
          variable %in% c("obs_period", "last_report") ~ NA_character_, 
          !is.na(current_year_rounded) ~ paste0(current_year, ": ", current_year_rounded),
          TRUE ~ i18n("OBSERVER_OVERVIEW_NO_DATA")
        )
      ) %>%
      select(-current_year_rounded)
    
    logbooks_info<-accessFishingTripsCatch(pool,trip_type = "1")
    
    logbooks_info$date_from <- as.POSIXct(as.character(logbooks_info$date_from))
    attr(logbooks_info$date_from, "tzone") <- appConfig$country_profile$timezone
    logbooks_info$date_to <- as.POSIXct(as.character(logbooks_info$date_to))
    attr(logbooks_info$date_to, "tzone") <- appConfig$country_profile$timezone
    logbooks_info$year<-year(logbooks_info$date_to)
    
    agg_all <- safe_summarise_logbook(logbooks_info)
    agg_cy  <- safe_summarise_logbook(logbooks_info, year_filter = current_year)
    agg_ly  <- safe_summarise_logbook(logbooks_info, year_filter = current_year - 1)
    
    agg_all <- agg_all %>% mutate(across(everything(), as.character))
    agg_cy  <- agg_cy  %>% mutate(across(everything(), as.character))
    agg_ly  <- agg_ly  %>% mutate(across(everything(), as.character))
    
    merged_tbl_logbook <- agg_all %>%
      pivot_longer(everything(), names_to = "variable", values_to = "all") %>%
      left_join(agg_cy %>% pivot_longer(everything(), names_to = "variable", values_to = "c_year"), by = "variable") %>%
      left_join(agg_ly %>% pivot_longer(everything(), names_to = "variable", values_to = "l_year"), by = "variable") %>%
      mutate(
        evolution_pct = case_when(
          is.na(c_year) | is.na(l_year) ~ NA_character_,
          as.numeric(l_year) == 0 ~ NA_character_,
          TRUE ~ paste0(round((as.numeric(c_year) - as.numeric(l_year)) / as.numeric(l_year) * 100, 1), "%")
        )
      )
    
    merged_tbl_logbook <- merged_tbl_logbook %>%
      mutate(
        current_year_rounded = case_when(
          !is.na(c_year) ~ as.character(round(as.numeric(c_year))),
          TRUE ~ as.character(c_year)
        ),
        display_text = case_when(
          !is.na(current_year_rounded) ~ paste0(current_year, ": ", current_year_rounded),
          TRUE ~ i18n("OBSERVER_OVERVIEW_NO_DATA")
        )
      ) %>%
      select(-current_year_rounded)
    
    catch_unit<-unique(reports_info$total_catch_unit)
    
    output$first_line_indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_NB_REPORTS"),
          value = merged_tbl$all[merged_tbl$variable == "nb_reports"],
          subtitle = tagList(
            merged_tbl$display_text[merged_tbl$variable == "nb_reports"],
            format_evolution(
              merged_tbl$evolution_pct[merged_tbl$variable == "nb_reports"]
            )
          ),
          icon = icon("file-alt"),
          color = "primary",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_NB_VESSELS"),
          value = merged_tbl$all[merged_tbl$variable == "nb_vessels"],
          subtitle = tagList(
            merged_tbl$display_text[merged_tbl$variable == "nb_vessels"],
            format_evolution(
              merged_tbl$evolution_pct[merged_tbl$variable == "nb_vessels"]
            )
          ),
          icon = icon("ship"),
          color = "primary",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_NB_OBSERVERS"),
          value = merged_tbl$all[merged_tbl$variable == "nb_observers"],
          subtitle = tagList(
            merged_tbl$display_text[merged_tbl$variable == "nb_observers"],
            format_evolution(
              merged_tbl$evolution_pct[merged_tbl$variable == "nb_observers"]
            )
          ),
          icon = icon("user"),
          color = "primary",
          width = 4
        )
      )
    })
    
    output$second_line_indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_OBSERVATION_PERIOD"),
          value = merged_tbl$all[merged_tbl$variable == "obs_period"],
          subtitle = "",
          icon = icon("calendar-alt"),
          color = "warning",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_OBSERVED_DAYS"),
          value = round(as.numeric(merged_tbl$all[merged_tbl$variable == "total_obs_days"]),0),
          subtitle = tagList(
            merged_tbl$display_text[merged_tbl$variable == "total_obs_days"],
            format_evolution(
              merged_tbl$evolution_pct[merged_tbl$variable == "total_obs_days"]
            )
          ),
          icon = icon("clock"),
          color = "warning",
          width = 4
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_LAST_REPORT"),
          value = merged_tbl$all[merged_tbl$variable == "last_report"],
          subtitle = "",
          icon = icon("calendar-check"),
          color = "warning",
          width = 4
        )
      )
    })
    
    format_evolution <- function(pct) {
      
      if (is.na(pct) || pct == "") return(NULL)
      
      value <- suppressWarnings(as.numeric(gsub("%", "", pct)))
      if (is.na(value)) return(NULL)
      
      arrow <- if (value > 0) {
        icon("arrow-up", style = "color:green;")
      } else if (value < 0) {
        icon("arrow-down", style = "color:red;")
      } else {
        icon("arrow-right", style = "color:gray;")
      }
      
      tagList(
        span(" ("),
        span(pct),
        span(" "),
        arrow,
        span(" "),
        span(
          if (value == 0)
            i18n("OBSERVER_OVERVIEW_EVOLUTION_NO_CHANGE")
          else
            i18n("OBSERVER_OVERVIEW_EVOLUTION_FROM_LAST_YEAR"),
          style = "color:#666;"
        ),
        span(")")
      )
    }
    
    
    output$third_line_indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_TOTAL_CATCH_OBSERVED"),
          value = paste0(round(as.numeric(merged_tbl$all[merged_tbl$variable == "total_catch"]),0)," ",catch_unit),
          subtitle = tagList(
            merged_tbl$display_text[merged_tbl$variable == "total_catch"],
            format_evolution(
              merged_tbl$evolution_pct[merged_tbl$variable == "total_catch"]
            )
          ),
          icon = icon("clipboard-list"),
          color = "info",
          width = 3
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_TOTAL_CATCH_REPORTED"),
          value = paste0(merged_tbl_logbook$all[merged_tbl_logbook$variable == "total_catch"]," ",catch_unit),
          subtitle = tagList(
            merged_tbl_logbook$display_text[merged_tbl_logbook$variable == "total_catch"],
            format_evolution(
              merged_tbl_logbook$evolution_pct[merged_tbl_logbook$variable == "total_catch"]
            )
          ),
          icon = icon("fish"),
          color = "info",
          width = 3
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_AVERAGE_CATCH_OBSERVED"),
          value = paste0(round(as.numeric(merged_tbl$all[merged_tbl$variable == "average_catch"]),0)," ",catch_unit),
          subtitle = tagList(
            merged_tbl$display_text[merged_tbl$variable == "average_catch"],
            format_evolution(
              merged_tbl$evolution_pct[merged_tbl$variable == "average_catch"]
            )
          ),
          icon = icon("clipboard-list"),
          color = "info",
          width = 3
        ),
        bs4InfoBox(
          title = i18n("OBSERVER_OVERVIEW_AVERAGE_CATCH_REPORTED"),
          value = paste0(merged_tbl_logbook$all[merged_tbl_logbook$variable == "average_catch"]," ",catch_unit),
          subtitle = tagList(
            merged_tbl_logbook$display_text[merged_tbl_logbook$variable == "average_catch"],
            format_evolution(
              merged_tbl_logbook$evolution_pct[merged_tbl_logbook$variable == "average_catch"]
            )
          ),
          icon = icon("fish"),
          color = "info",
          width = 3
        )
      )
    })
    
      req(reports_info)
      summary_report<-reports_info

    
    output$timeplot <- renderUI({
      req(reports_info)
      
      time_table <- subset(reports_info, !is.na(observation_start) & !is.na(observation_end))
      
      if (nrow(time_table) > 0) {
        generic_chart_ui(id = ns("ts1"))
      } else {
        div(paste0("(", i18n("OBSERVER_OVERVIEW_PLOT_NODATE_DISCLAMER"), ")"))
      }
    })
    
    reports_count<- reports_info %>%
      filter(!is.na(observation_start), !is.na(observation_end)) %>%
      select(report_id,vessel_id,observation_start,observation_end)%>%
      distinct()%>%
      mutate(category="reports",value=1)%>%
      ungroup()
    
    generic_chart_server(
      id = "ts1",
      df = reports_count,
      col_date = "observation_end",
      col_group = "category",
      col_value = "value",
      time_label = "",
      value_label = i18n("OBSERVER_OVERVIEW_NB_REPORTS"),
      group_label = "",
      stat = "sum",
      plot_types = c("bar_stack","stacked_area","line","heatmap","bubble"),
      time_choices = c("year")
    )
    
  })
}
