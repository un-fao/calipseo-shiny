#observer_overview_server
observer_overview_server <- function(id,parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    reports_info <- accessObserverReportsSummary(pool)
    
    print(head(reports_info))
    
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
    
    print(head(reports_info))
    
    # --- Agrégation sécurisée pour les valeurs numériques
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
    
    # --- Agrégations
    agg_all <- safe_summarise(reports_info)
    agg_cy  <- safe_summarise(reports_info, year_filter = current_year)
    agg_ly  <- safe_summarise(reports_info, year_filter = current_year - 1)
    
    # --- Convertir en character pour pivot
    agg_all <- agg_all %>% mutate(across(everything(), as.character))
    agg_cy  <- agg_cy  %>% mutate(across(everything(), as.character))
    agg_ly  <- agg_ly  %>% mutate(across(everything(), as.character))
    
    # --- Merge en tableau unique
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
        # arrondir certaines variables à l'unité
        current_year_rounded = case_when(
          variable %in% c("total_obs_days", "total_catch", "average_catch") & !is.na(c_year) ~ as.character(round(as.numeric(c_year))),
          TRUE ~ as.character(c_year)
        ),
        
        # créer display_text
        display_text = case_when(
          variable %in% c("obs_period", "last_report") ~ NA_character_, # pas de texte sous la valeur principale
          !is.na(current_year_rounded) & !is.na(evolution_pct) ~ paste0(current_year, ": ", current_year_rounded, " (", evolution_pct, ")"),
          !is.na(current_year_rounded) & is.na(evolution_pct)  ~ paste0(current_year, ": ", current_year_rounded),
          TRUE                                                ~ "No data"
        )
      ) %>%
      select(-current_year_rounded)
    
    # --- résultat
    print(merged_tbl)
    
    logbooks_info<-accessFishingTripsCatch(pool,trip_type = "1")
    print(logbooks_info)
    
    logbooks_info$date_from <- as.POSIXct(as.character(logbooks_info$date_from))
    attr(logbooks_info$date_from, "tzone") <- appConfig$country_profile$timezone
    logbooks_info$date_to <- as.POSIXct(as.character(logbooks_info$date_to))
    attr(logbooks_info$date_to, "tzone") <- appConfig$country_profile$timezone
    logbooks_info$year<-year(logbooks_info$date_to)
    
    # --- Agrégations
    agg_all <- safe_summarise_logbook(logbooks_info)
    agg_cy  <- safe_summarise_logbook(logbooks_info, year_filter = current_year)
    agg_ly  <- safe_summarise_logbook(logbooks_info, year_filter = current_year - 1)
    
    # --- Convertir en character pour pivot
    agg_all <- agg_all %>% mutate(across(everything(), as.character))
    agg_cy  <- agg_cy  %>% mutate(across(everything(), as.character))
    agg_ly  <- agg_ly  %>% mutate(across(everything(), as.character))
    
    # --- Merge en tableau unique
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
        # arrondir certaines variables à l'unité
        current_year_rounded = case_when(
          !is.na(c_year) ~ as.character(round(as.numeric(c_year))),
          TRUE ~ as.character(c_year)
        ),
        
        # créer display_text
        display_text = case_when(
          !is.na(current_year_rounded) & !is.na(evolution_pct) ~ paste0(current_year, ": ", current_year_rounded, " (", evolution_pct, ")"),
          !is.na(current_year_rounded) & is.na(evolution_pct)  ~ paste0(current_year, ": ", current_year_rounded),
          TRUE                                                ~ "No data"
        )
      ) %>%
      select(-current_year_rounded)
    
    print(merged_tbl_logbook)
    
    output$first_line_indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = "Number of Reports",
          value = merged_tbl$all[merged_tbl$variable == "nb_reports"],
          subtitle = merged_tbl$display_text[merged_tbl$variable == "nb_reports"],
          icon = icon("file-alt"),
          color = "primary",
          width = 4
        ),
        bs4InfoBox(
          title = "Number of Vessels",
          value = merged_tbl$all[merged_tbl$variable == "nb_vessels"],
          subtitle = merged_tbl$display_text[merged_tbl$variable == "nb_vessels"],
          icon = icon("ship"),
          color = "primary",
          width = 4
        ),
        bs4InfoBox(
          title = "Number of Observers",
          value = merged_tbl$all[merged_tbl$variable == "nb_observers"],
          subtitle = merged_tbl$display_text[merged_tbl$variable == "nb_observers"],
          icon = icon("user"),
          color = "primary",
          width = 4
        )
      )
    })
    
    output$second_line_indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = "Observation Period",
          value = merged_tbl$all[merged_tbl$variable == "obs_period"],
          subtitle = "",
          icon = icon("calendar-alt"),
          color = "warning",
          width = 4
        ),
        bs4InfoBox(
          title = "Observed Days",
          value = merged_tbl$all[merged_tbl$variable == "total_obs_days"],
          subtitle = merged_tbl$display_text[merged_tbl$variable == "total_obs_days"],
          icon = icon("clock"),
          color = "warning",
          width = 4
        ),
        bs4InfoBox(
          title = "Last Report",
          value = merged_tbl$all[merged_tbl$variable == "last_report"],
          subtitle = "",
          icon = icon("calendar-check"),
          color = "warning",
          width = 4
        )
      )
    })
    
    output$third_line_indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = "Total Catch Observed",
          value = merged_tbl$all[merged_tbl$variable == "total_catch"],
          subtitle = merged_tbl$display_text[merged_tbl$variable == "total_catch"],
          icon = icon("clipboard-list"),
          color = "info",
          width = 3
        ),
        bs4InfoBox(
          title = "Total Catch Reported",
          value = merged_tbl_logbook$all[merged_tbl_logbook$variable == "total_catch"],
          subtitle = merged_tbl_logbook$display_text[merged_tbl_logbook$variable == "total_catch"],
          icon = icon("fish"),
          color = "info",
          width = 3
        ),
        bs4InfoBox(
          title = "Average Catch Observed",
          value = merged_tbl$all[merged_tbl$variable == "average_catch"],
          subtitle = merged_tbl$display_text[merged_tbl$variable == "average_catch"],
          icon = icon("clipboard-list"),
          color = "info",
          width = 3
        ),
        bs4InfoBox(
          title = "Average Catch Reported",
          value = merged_tbl_logbook$all[merged_tbl_logbook$variable == "average_catch"],
          subtitle = merged_tbl_logbook$display_text[merged_tbl_logbook$variable == "average_catch"],
          icon = icon("fish"),
          color = "info",
          width = 3
        )
      )
    })
    
      req(reports_info)
      summary_report<-reports_info
    
    output$reports_summary <- renderDT(
      summary_report,
      container = initDTContainer(summary_report),
      server = FALSE,
      escape = FALSE,
      rownames = FALSE,
      extensions = c("Buttons"),
      filter = list(position = 'top', clear = FALSE),
      
      options = list(
        autoWidth = FALSE,
        dom = 'Bfrtip',
        deferRender = TRUE,
        scroll = FALSE,
        buttons = list(
          list(extend = 'copy'),
          list(extend = 'csv', filename = i18n("SUMMARY") , title = NULL, header = TRUE),
          list(extend = 'excel', filename =  i18n("SUMMARY"), title = NULL, header = TRUE),
          list(extend = "pdf", title = i18n("SUMMARY"), header = TRUE, orientation = "landscape")
        ),
        exportOptions = list(
          modifiers = list(page = "all", selected = TRUE)
        ),
        language = list(url = i18n("TABLE_LANGUAGE")),
        pageLength = 10,
        initComplete = js
      )
    )
    
    output$timeplot <- renderUI({
      req(reports_info)
      time_table<-subset(reports_info,!is.na(OBSERVATION_PERIOD_START)&!is.na(OBSERVATION_PERIOD_END))
      
      if(nrow(time_table)>0){
        time_table<-time_table%>%
          mutate(year=year(OBSERVATION_PERIOD_END))%>%
          group_by(year)%>%
          summarise(n=length(ID))%>%
          ungroup()
        
        #TEST barplot when data available
      }else{
      div(paste0("(",i18n("PLOT_NODATE_DISCLAMER"),")"))
      }
    })    
  })
}
