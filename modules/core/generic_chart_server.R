generic_chart_server <- function(
  id,
  df,
  col_date,
  col_group,
  col_value,
  time_label = "Date",
  value_label = "Value",
  group_label = "Group",
  plot_types = NULL,
  time_choices = c("month","year"),
  stat = "sum",    
  error = "none"  
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    compute_stats <- function(x, error_type) {
     
       x <- x[!is.na(x)]
      
       n <- length(x)
      
       if (n == 0) return(c(mean = NA_real_, ymin = NA_real_, ymax = NA_real_))
      
       m <- mean(x)
      
       if (error_type == "sd") {
        sdv <- sd(x)
        return(c(mean = m, ymin = m - sdv, ymax = m + sdv))
      } else if (error_type == "se") {
        sdv <- sd(x)
        se <- sdv / sqrt(n)
        return(c(mean = m, ymin = m - se, ymax = m + se))
      } else if (error_type == "ci95") {
        sdv <- sd(x)
        se <- sdv / sqrt(n)
        ci <- qnorm(0.975) * se
        return(c(mean = m, ymin = m - ci, ymax = m + ci))
      } else {
        return(c(mean = m, ymin = NA_real_, ymax = NA_real_))
      }
    }
    
  
    all_plot_types <- c(
      "Line"            = "line",
      "Line (total)"    = "line_sum",
      "Line (mean)"     = "line_mean",
      "Stacked area"    = "area_stack",
      "Stacked area %"  = "area_stack_pct",
      "Bar (mean)"      = "bar_mean",
      "Stacked bars"    = "bar_stack",
      "Stacked bars %"  = "bar_stack_pct",
      "Rank (total)"    = "rank_sum",
      "Heatmap"         = "heatmap",
      "Bubble"          = "bubble",
      "Pie"             = "pie",
      "Donut"           = "donut",
      "Treemap"         = "treemap",
      "Boxplot"         = "boxplot"
    )
    
  
    plot_types_sum  <- c("line","line_sum","line_mean","area_stack","area_stack_pct","bar_mean","bar_stack","bar_stack_pct","rank_sum","heatmap","bubble","pie","donut","treemap","boxplot")
    plot_types_mean <- c("line","line_mean","bar_mean","heatmap","bubble","pie","donut","treemap","boxplot") # stacked exclus
    
    if (stat == "sum") {
      allowed <- plot_types_sum
    } else if (stat == "mean") {
      allowed <- plot_types_mean
    } else stop("stat must be 'sum' or 'mean'")
    
    plot_type_choices <- all_plot_types[all_plot_types %in% allowed]
    if (!is.null(plot_types)) {
      
      plot_type_choices <- plot_type_choices[plot_type_choices %in% plot_types]
    }
    
    granu_choices <- c("Month" = "month", "Year" = "year")
    
    output$error_wrapper <- renderUI({
      req(input$plot_style)
      div(
        style = if(!input$plot_style %in% c("line_sum", "line_mean", "bar_mean"))  "display:none;" else NULL,
      selectInput(ns("error_type"),
                  "Error bars :",
                  choices = c("None" = "none", "Standard deviation" = "sd", "Standard error" = "se", "CI95%" = "ci95"),
                  selected = error)
      )
    })
    
    output$plot_style_wrapper <- renderUI({
      div(
        style = if(length(plot_type_choices) == 1) "display:none;" else NULL,
        selectInput(ns("plot_style"), "Plot style :", choices = plot_type_choices, selected = plot_type_choices[1])
      )
    })
    output$granularity_wrapper <- renderUI({
      granu <- granu_choices[granu_choices %in% time_choices]
      div(
        style = if(length(granu) == 1) "display:none;" else NULL,
        selectInput(ns("granularity"), "Granularity :", choices = granu, selected = granu[1])
      )
    })
    
    data_formatted <- reactive({
      req(df)
      gran <- ifelse(is.null(input$granularity), "year", input$granularity)
      err_type <- if (stat == "mean") {
        if (!is.null(input$error_type)) input$error_type else error
      } else {
        "none"
      }
      
      df1 <- df %>%
        rename(
          date  = !!rlang::sym(col_date),
          group = !!rlang::sym(col_group),
          raw_value = !!rlang::sym(col_value)   
        ) %>%
        mutate(date = as.Date(date),
               period_date = dplyr::case_when(
                 gran == "year"  ~ as.Date(paste0(format(date, "%Y"), "-01-01")),
                 gran == "month" ~ as.Date(paste0(format(date, "%Y-%m"), "-01"))
               ),
               period = ifelse(gran == "year", format(period_date, "%Y"), format(period_date, "%Y-%m"))
        )
      
      if (stat == "sum") {
        
        df_agg <- df1 %>%
          group_by(period_date, period, group) %>%
          summarise(value = sum(raw_value, na.rm = TRUE), .groups = "drop") %>%
          arrange(period_date, group)
        
        df_agg$ymin <- NA_real_
        df_agg$ymax <- NA_real_
        return(df_agg)
      }
      
      df_stats <- df1 %>%
        group_by(period_date, period, group) %>%
        summarise(raws = list(raw_value), .groups = "drop") 
      
      df_stats2 <- df_stats %>%
        rowwise() %>%
        mutate(
          st = list(compute_stats(unlist(raws), err_type)),
          value = as.numeric(st["mean"]),
          ymin  = as.numeric(st["ymin"]),
          ymax  = as.numeric(st["ymax"])
        ) %>%
        ungroup() %>%
        select(period_date, period, group, value, ymin, ymax) %>%
        arrange(period_date, group)
      
      return(df_stats2)
    })
    
    plot_reactive <- reactive({
      d <- data_formatted()
      req(nrow(d) > 0)
      
      style <- ifelse(is.null(input$plot_style), names(plot_type_choices)[1], input$plot_style)
      
      if (style %in% c("line","line_sum","line_mean","area_stack","area_stack_pct","bar_mean","bar_stack","bar_stack_pct")) {
        x_lab <- time_label; y_lab <- value_label
      } else if (style %in% c("heatmap","bubble")) {
        x_lab <- time_label; y_lab <- group_label
      } else if (style == "boxplot") {
        x_lab <- group_label; y_lab <- value_label
      } else {
        x_lab <- NULL; y_lab <- NULL
      }
      
      full_seq <- if (input$granularity == "month") {
        seq.Date(from = min(d$period_date), to = max(d$period_date), by = "month")
      } else {
        seq.Date(from = min(d$period_date), to = max(d$period_date), by = "year")
      }
      all_groups <- sort(unique(d$group))
      full_grid <- expand.grid(period_date = full_seq, group = all_groups, stringsAsFactors = FALSE)
      d_full <- merge(full_grid, d, by = c("period_date", "group"), all.x = TRUE)
      d_full <- d_full %>% arrange(period_date, group) %>%
        mutate(period = ifelse(format(period_date, "%m") == "01", format(period_date, "%Y"), format(period_date, "%Y-%m")))
      
      stacked_modes <- c("bar_stack","bar_stack_pct","area_stack","area_stack_pct")
      d_plot <- d_full
      if (style %in% stacked_modes) d_plot$value <- ifelse(is.na(d_plot$value), 0, d_plot$value)
      
      needs_group_aggregation <- style %in% c("line_sum", "line_mean", "bar_mean")
      aggregator <- ifelse(style %in% c("line_sum","area_stack","bar_stack","bar_stack_pct","area_stack_pct"), "sum",
                           ifelse(style %in% c("line_mean","bar_mean"), "mean", NA_character_))
      
      if (style %in% c("pie","donut","treemap")) {
        d_synth <- d_full %>%
          group_by(group) %>%
          summarise(value = if (stat == "sum") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE), .groups = "drop")
        
        if (style == "pie") {
          p <- plotly::plot_ly(d_synth, labels = ~group, values = ~value, type = "pie")
        } else if (style == "donut") {
          p <- plotly::plot_ly(d_synth, labels = ~group, values = ~value, type = "pie", hole = 0.6)
        } else {
          p <- plotly::plot_ly(d_synth, type = "treemap", labels = ~group, parents = NA, values = ~value, textinfo = "label+value+percent parent")
        }
        return(p %>% layout(hovermode = "x unified"))
      }
      
      if (style == "rank_sum") {
        req(stat == "sum")
        
        top_n <- 10
        
        d_rank <- d_full %>%
          group_by(group) %>%
          summarise(
            value = sum(value, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(!is.na(value)) %>%
          arrange(desc(value)) %>%
          mutate(
            rank = row_number(),
            pct  = value / sum(value, na.rm = TRUE)
          ) %>%
          slice_head(n = top_n)
        
        p <- plotly::plot_ly(
          data = d_rank,
          x = ~value,
          y = ~rank,
          type = "bar",
          orientation = "h",
          text = ~paste0(
            group, "<br>",
            round(value, 2), " (",
            scales::percent(pct, accuracy = 0.1), ")"
          ),
          textposition = "auto",
          hoverinfo = "none"
        ) %>%
          layout(
            showlegend = FALSE,
            uniformtext = list(minsize = 9, mode = "show"),
            yaxis = list(
              title = "Rank",
              autorange = "reversed",
              tickmode = "array",
              tickvals = d_rank$rank,
              ticktext = d_rank$rank
            ),
            xaxis = list(
              title = value_label,
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = FALSE
            ),
            plot_bgcolor  = "rgba(0,0,0,0)",
            paper_bgcolor = "rgba(0,0,0,0)"
          )
        
        return(p)
      }
    
      if (style == "line") {
      
        p <- plotly::plot_ly(d_full, x = ~period_date, y = ~value, color = ~group, type = "scatter", mode = "lines+markers", connectgaps = FALSE)
      } else if (style == "line_sum") {
        d_agg <- d_full %>% group_by(period_date) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
        p <- plotly::plot_ly(d_agg, x = ~period_date, y = ~value, type = "scatter", mode = "lines+markers")
      } else if (style == "line_mean") {
        d_agg <- d_full %>% group_by(period_date) %>% summarise(value = mean(value, na.rm = TRUE), ymin = min(ymin, na.rm = TRUE), ymax = max(ymax, na.rm = TRUE), .groups = "drop")
      
        p <- plotly::plot_ly(data= d_agg,x = ~period_date, y = ~value, type = "scatter", name="mean",mode = "lines+markers", connectgaps = FALSE) %>%
          plotly::add_ribbons(data = d_agg, x = ~period_date, ymin = ~ymin, ymax = ~ymax, fillcolor = "rgba(0,0,150,0.15)", line = list(color = "rgba(0,0,0,0)"), name = "error")
      } else if (style %in% c("bar_stack","bar_stack_pct")) {
        p <- plotly::plot_ly(d_plot, x = ~period_date, y = ~value, color = ~group, type = "bar") %>% layout(barmode = "stack")
        if (style == "bar_stack_pct") p <- p %>% layout(barnorm = "percent", yaxis = list(ticksuffix = "%"))
      } else if (style == "bar_mean") {
        d_agg <- d_full %>% group_by(period_date) %>% summarise(value = mean(value, na.rm = TRUE), ymin = min(ymin, na.rm = TRUE), ymax = max(ymax, na.rm = TRUE), .groups = "drop")
        p <- plotly::plot_ly(d_agg, x = ~period_date, y = ~value, type = "bar", error_y = list(type = "data", array = ~ (ymax - value), arrayminus = ~(value - ymin), visible = TRUE))
      } else if (style %in% c("area_stack","area_stack_pct")) {
        groupnorm_val <- if (style == "area_stack_pct") "percent" else NULL
        p <- plotly::plot_ly(d_plot, x = ~period_date, y = ~value, color = ~group, type = "scatter", mode = "none", stackgroup = "one", groupnorm = groupnorm_val, fill = "tonexty")
      } else if (style == "heatmap") {
        
        d_full$period <- factor(d_full$period, levels = sort(unique(d_full$period)))
        p <- plotly::plot_ly(d_full, x = ~period, y = ~group, z = ~value, type = "heatmap", colorscale = list(list(0,"#053061"), list(0.5,"#92C5DE"), list(0.9,"#FFFFBF"), list(1,"#F46D43")), reversescale = FALSE, zauto = TRUE, showscale = TRUE)
      } else if (style == "bubble") {
        d_b <- d_full %>% mutate(display_value = ifelse(is.na(value), 0, value))
        maxv <- max(d_b$display_value, na.rm = TRUE)
        sizeref <- ifelse(is.finite(maxv) && maxv > 0, 2 * maxv / (50^2), 1)
        
        d_b <- d_b %>% mutate(tooltip = paste0(group, "<br>", format(period_date, "%Y-%m-%d"), "<br>", ifelse(is.na(value), "(no data)", format(round(value,2), nsmall=2))))
        p <- plotly::plot_ly(d_b, x = ~period_date, y = ~group, size = ~display_value, color = ~group, text = ~tooltip, hoverinfo = "text", type = "scatter", mode = "markers", marker = list(sizemode = "area", sizeref = sizeref, sizemin = 1))
      } else if (style == "boxplot") {
        
        gran <- ifelse(is.null(input$granularity), "year", input$granularity)
        df_raw <- df %>%
          rename(date = !!rlang::sym(col_date), group = !!rlang::sym(col_group), raw_value = !!rlang::sym(col_value)) %>%
          mutate(date = as.Date(date),
                 period_date = if (gran == "year") as.Date(paste0(format(date, "%Y"), "-01-01")) else as.Date(paste0(format(date, "%Y-%m"), "-01"))
          ) %>%
          filter(group %in% all_groups)
        p <- plotly::plot_ly(df_raw, x = ~group, y = ~raw_value, type = "box", color = ~group, boxpoints = "all", jitter = 0.5, pointpos = 0)
      } else {
        p <- plotly::plot_ly() 
      }
      
      if (style == "heatmap") {
        p<-p %>% layout(xaxis = list(title = xlab), yaxis = list(title = ylab), hovermode = "x unified")
      } else {
        
        if (input$granularity == "year") {
          p<-p %>% layout(
            xaxis = list(
              title = x_lab,
              dtick = "M12",      
              tickformat = "%Y",
              rangeselector = list(
                buttons = list(
                  list(count = 3,  label = "3M", step = "month", stepmode = "backward"),
                  list(count = 6,  label = "6M", step = "month", stepmode = "backward"),
                  list(count = 1,  label = "1Y", step = "year",  stepmode = "backward"),
                  list(count = 2,  label = "2Y", step = "year",  stepmode = "backward"),
                  list(count = 3,  label = "3Y", step = "year",  stepmode = "backward"),
                  list(count = 5,  label = "5Y", step = "year",  stepmode = "backward"),
                  list(step = "year", stepmode = "todate", label = "YTD"),
                  list(step = "all", label = "All")
                )
              ),
              rangeslider = list(visible = FALSE)
            ),
            yaxis = list(title = y_lab),
            hovermode = "x unified",
            connectgaps = FALSE
          )
        } else {
          p<-p %>% layout(
            xaxis = list(
              title = x_lab,
              dtick = "M1",     
              tickformat = "%Y-%m",    
              rangeselector = list(
                buttons = list(
                  list(count = 3,  label = "3M", step = "month", stepmode = "backward"),
                  list(count = 6,  label = "6M", step = "month", stepmode = "backward"),
                  list(count = 1,  label = "1Y", step = "year",  stepmode = "backward"),
                  list(count = 2,  label = "2Y", step = "year",  stepmode = "backward"),
                  list(count = 3,  label = "3Y", step = "year",  stepmode = "backward"),
                  list(count = 5,  label = "5Y", step = "year",  stepmode = "backward"),
                  list(step = "year", stepmode = "todate", label = "YTD"),
                  list(step = "all", label = "All")
                )
              ),
              rangeslider = list(visible = FALSE)
            ),
            yaxis = list(title = y_lab),
            hovermode = "x unified",
            connectgaps = FALSE
          )
        }
      }
      
      p
    })
    
    output$plot <- plotly::renderPlotly({ plot_reactive() })
    

    output$table <- DT::renderDT({
      dtab <- data_formatted()
      if (!"value" %in% names(dtab)) stop("value missing in data_formatted()")
      dtab %>% tidyr::pivot_wider(names_from = group, values_from = value) %>%
        DT::datatable(extensions = "Buttons", options = list(dom = "Bfrtip"))
    })
    
    output$result <- renderUI({
      tabsetPanel(
        tabPanel("Plot", plotly::plotlyOutput(ns("plot")) %>% shinycssloaders::withSpinner(type = 4)),
        tabPanel("Statistics", DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(type = 4))
      )
    })
    
    output$dl_html <- downloadHandler(
      filename = function() paste0("chart_", Sys.Date(), ".html"),
      content = function(file) {
        tmp <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(plot_reactive(), tmp, selfcontained = TRUE)
        file.copy(tmp, file)
      }
    )
    
    output$dl_png <- downloadHandler(
      filename = function() paste0("chart_", Sys.Date(), ".png"),
      content = function(file) {
        plotly::export(plot_reactive(), file = file)
      }
    )
    
  })
}
