#logbooks_overview_server
logbooks_overview_server <- function(id, parent.session, lang = NULL, pool, reloader){
 
 moduleServer(id, function(input, output, session) {  

  ns<-session$ns
   
  INFO("logbooks-overview: START")
  MODULE_START_TIME <- Sys.time() 
  
  #i18n
  #-----------------------------------------------------------------------------
  i18n_translator <- get_reactive_translator(lang)
  i18n <- function(key){ i18n_translator()$t(key) }
  #-----------------------------------------------------------------------------
  
  #year refs
  currentyear <- as.integer(format(Sys.Date(),"%Y"))
  lastyear <- currentyear-1
  
  #reactives
  #reactives - info
  infos_fetched <- reactiveVal(FALSE)
  infos <- reactiveValues(
    currentyear = currentyear,
    lastyear = lastyear,
    nb_active_vessel_lastyear = NULL,
    nb_active_vessel_currentyear = NULL,
    total_lastyear = NULL,
    total_currentyear = NULL
  )
  #reactives for global view
  gv_data_formated <- reactiveVal(NULL)
  gv_data_ready <- reactiveVal(FALSE)
  
  #Access data for indicators
  observe({
    logbooks_lastyear <- accessLogBooks(pool, lastyear)
    logbooks_currentyear <- accessLogBooks(pool, currentyear)
    infos$nb_active_vessel_lastyear<-length(unique(logbooks_lastyear$regnum))
    infos$nb_active_vessel_currentyear<-length(unique(logbooks_currentyear$regnum))
    infos$total_lastyear <- sum(logbooks_lastyear$quantity) #assumes all units = KG
    infos$total_currentyear <- sum(logbooks_currentyear$quantity) #assumes all units = KG
  })
  
  observe({
    
    if(all(!sapply(reactiveValuesToList(infos), is.null))) infos_fetched(TRUE)
    
    #info
    output$logbooks_overview_info <- renderText({
      #session$userData$page("logbooks_overview")
      text <- paste0("<h3>", i18n("LOGBOOKS_OVERVIEW_TITLE")," – <small>", i18n("LOGBOOKS_OVERVIEW_SUBTITLE"),"</small></h3><hr>")
      text
    })
    
    #UI indicators
    output$nb_infos <- renderUI({
      fluidRow(
          div(class = "col-md-6",
            box(
              title = HTML(sprintf("<b>%s</b>",as.integer(format(Sys.Date(), "%Y"))-1)),
              width = 12, collapsible = FALSE,
              fluidRow(
                bs4Dash::infoBox(
                  title = i18n("INFOBOX_OVERVIEW_TOTAL_QUANTITY"), 
                  value = paste(round(measurements::conv_unit(infos$total_lastyear, PREF_UNIT_WEIGHT$CODE, "metric_ton"),2), i18n("TOTAL_QUANTITY_UNITS_TONS")),
                  icon = icon("fish"), color = "primary", 
                  width = 6),
                bs4Dash::infoBox(
                  title = i18n("INFOBOX_OVERVIEW_TOTAL_PARTICIPATING_VESSELS"), 
                  value = infos$nb_active_vessel_lastyear, 
                  icon = icon("ship"), color = "navy",
                  width = 6)
              )
            )
          ),
          div(class = "col-md-6",
            box(
              title=HTML(sprintf("<b>%s</b>",format(Sys.Date(), "%Y"))),
              width = 12, collapsible = FALSE,
              fluidRow(
                bs4Dash::infoBox(
                  title = i18n("INFOBOX_OVERVIEW_TOTAL_QUANTITY"), 
                  value = paste(round(measurements::conv_unit(infos$total_currentyear, PREF_UNIT_WEIGHT$CODE, "metric_ton"),2), i18n("TOTAL_QUANTITY_UNITS_TONS")), 
                  icon = icon("fish"), color = "primary",
                  width = 6),
                bs4Dash::infoBox(
                  title = i18n("INFOBOX_OVERVIEW_TOTAL_PARTICIPATING_VESSELS"), 
                  value = infos$nb_active_vessel_currentyear, 
                  icon = icon("ship"), color = "navy", 
                  width = 6)
              )
            )
          )
        )
      
    })
    
  })

  #logbooks series
  data_logbooks <- accessLogBooksMultiyear(pool)  
  
  #ref data
  cl_asfis_species<-getRemoteReferenceDataset("cl_asfis_species")
  cl_asfis_species<-subset(cl_asfis_species, select=c('code','isscaap_group_code'))
  names(cl_asfis_species)<-c('species_asfis','isscaap_group_code')
  cl_isscaap_group <- getRemoteReferenceDataset("cl_isscaap_group")
  cl_isscaap_group <- subset(cl_isscaap_group,select=c('code','name_en'))
  names(cl_isscaap_group) <- c('isscaap_group_code','ISSCAAP_Group_En')
  fish_group <- merge(cl_asfis_species, cl_isscaap_group)
  fish_group <- subset(fish_group, select = -c(isscaap_group_code))
  
  #event reactive to transform data depending on the data granularity (year, month, week)
  gv_data_formating <- eventReactive(input$gv_granu,{
    
    gv_format_date <- if(input$gv_granu == i18n("YEARLY")){
      "%Y"
    }else if(input$gv_granu == i18n("MONTHLY")){
      "%Y-%m"
    }else if(input$gv_granu == i18n("WEEKLY")){
      "%Y-%U"
    }
    
    df<-data_logbooks |>
      mutate(date = as.character(format(as.Date(date),format = gv_format_date))) |>
      group_by(date) |>
      summarise(nb_vessel = length(unique(regnum))) |>
      ungroup()
    
    gv_data_formated(df)
    gv_data_ready(TRUE)
  }
  )
  
  #UIs
  #generic plot for logbooks
  output$gv_plot <- renderPlotly({
    gv_data_formating()
    
    if(isTRUE(gv_data_ready())){
    
      p <- gv_data_formated() |> plot_ly(x = ~date)
      p <- p |>    
            add_trace(
              type = "scatter", mode = "lines+markers",
              y = ~nb_vessel, line = list(simplyfy = F),
              text = ~sprintf(paste("%s: %s",i18n("GLOBAL_VESSEL_PLOT_UNIT")),date,round(nb_vessel)))
      
        p |>layout(
          showlegend=F,
          hovermode ='closest',
          xaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            title = i18n("GLOBAL_VESSEL_PLOT_XLAB"),
            zeroline = F
          ),
          yaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            title = i18n("GLOBAL_VESSEL_PLOT_YLAB"),
            zeroline = F
          )
        )
    }
  })
  
  #generic table for logbooks - PARTICIPATING VESSELS
  output$gv_table <- DT::renderDT(server = FALSE, {
    
    gv_data_formating()
    
    if(isTRUE(gv_data_ready())){
      
      gv_format_date <- if(input$gv_granu == i18n("YEARLY")){
        "%Y"
      }else if(input$gv_granu == i18n("MONTHLY")){
        "%Y-%m"
      }else if(input$gv_granu == i18n("WEEKLY")){
        "%Y-%U"
      }
      
      granu<-switch(gv_format_date,
        "%Y" = i18n("GRANU_LABEL_YEAR"),
        "%Y-%m" = i18n("GRANU_LABEL_MONTH"),
        "%Y-%U" = i18n("GRANU_LABEL_WEEK")
      )
      
      dt <- gv_data_formated() |>
        rename(!!granu:=date,
             !!i18n("GLOBAL_VESSEL_LABEL"):=nb_vessel)
      
      DT::datatable(
        dt,
        extensions = c("Buttons"),
        escape = FALSE,
        filter = list(position = 'top',clear =FALSE),
        options = list(
          dom = 'Bfrtip',
          scrollX=TRUE,
          pageLength=5,
          orientation ='landscape',
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv', filename =  sprintf(i18n("STATISTIC_DATA_EXPORT_FILENAME"),i18n("GLOBAL_VESSEL_LABEL"),granu), title = NULL, header = TRUE),
            list(extend = 'excel', filename =  sprintf(i18n("STATISTIC_DATA_EXPORT_FILENAME"),i18n("GLOBAL_VESSEL_LABEL"),granu), title = NULL, header = TRUE),
            list(extend = "pdf", pageSize = 'A4',orientation = 'landscape',filename = sprintf(i18n("STATISTIC_DATA_EXPORT_FILENAME"),i18n("GLOBAL_VESSEL_LABEL"),granu), 
                 title = sprintf(i18n("STATISTIC_PDF_TITLE"), i18n("GLOBAL_VESSEL_LABEL"),granu), header = TRUE)
          ),
          exportOptions = list(
            modifiers = list(page = "all",selected=TRUE)
          ),
          language = list(url = i18n("TABLE_LANGUAGE"))
        )
      )
    }
  })
  
  #Main UI for plot/statistics table
  output$gv_result<-renderUI({
    tabsetPanel(
      tabPanel(i18n("TABPANEL_PLOT"), plotlyOutput(ns("gv_plot")) |> withSpinner(type = 4)),
      tabPanel(i18n("TABPANEL_STATISTIC"), DTOutput(ns("gv_table")) |> withSpinner(type = 4))
    )
  })
  
  #line chart server - GLOBAL QUANTITY
  line_chart_server(
    id = "gq",
    lang = lang,
    label = i18n("GLOBAL_QUANTITY_LABEL"),
    df = data_logbooks |> mutate(label = "Total"), 
    colDate = "date", colTarget="label", 
    ylab = sprintf('%s (%s)', i18n("QUANTITY_PLOT_YLAB"), PREF_UNIT_WEIGHT$CODE),
    valueUnit = PREF_UNIT_WEIGHT$CODE,
    colValue="quantity", rank = FALSE,
    mode='plot+table'
  )
  
  #line chart server - VESSEL TYPES
  line_chart_server(
    id = "vt", 
    lang = lang,
    label = i18n("VESSEL_TYPE_LABEL"),
    df = data_logbooks, 
    colDate = "date",
    colTarget = "vesseltype",
    ylab = sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"), PREF_UNIT_WEIGHT$CODE),
    valueUnit = PREF_UNIT_WEIGHT$CODE,
    colValue = "quantity",
    rank = FALSE,
    mode = 'plot+table'
  )
  
  #line chart server - GEAR TYPES
  line_chart_server(
    id = "gt",
    lang = lang,
    label = i18n("GEAR_TYPE_LABEL"),
    df = data_logbooks, 
    colDate = "date",
    colTarget = "fishing_gear",
    ylab = sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT$CODE),
    valueUnit = PREF_UNIT_WEIGHT$CODE,
    colValue="quantity",
    rank = FALSE,
    mode = 'plot+table'
  )
  
  #line chart server - SPECIES
  line_chart_server(
    id = "sp", 
    lang = lang,
    label = i18n("SPECIES_LABEL"),
    df = data_logbooks |>
          mutate(text = sprintf("%s-<em>%s</em> (<b>%s</b>)", species_desc, species_sci, species_asfis)),
    colDate = "date",
    colTarget = "species_desc",
    ylab = sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT$CODE),
    valueUnit = PREF_UNIT_WEIGHT$CODE,
    colValue = "quantity",
    colText = "text",
    rank = TRUE,
    nbToShow = 5,
    rankLabel = i18n("RANK_LABEL"),
    mode = 'plot+table'
  )
  
  #line chart server - SPECIES GROUPS
  line_chart_server(
    id = "fg", 
    lang = lang,
    label = i18n("FISHING_GEAR_LABEL"),
    df = data_logbooks |> left_join(fish_group),
    colDate = "date", 
    colTarget = "ISSCAAP_Group_En",
    ylab = sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT$CODE),
    valueUnit = PREF_UNIT_WEIGHT$CODE,
    colValue = "quantity",
    rank = FALSE,
    mode = 'plot+table'
  )
  
  #line chart server - LANDING SITES
  line_chart_server(
    id = "ls", 
    lang = lang,
    label = i18n("LANDING_SITES_LABEL"),
    df = data_logbooks |> left_join(fish_group),
    colDate = "date",
    colTarget = "landing_site",
    ylab = sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT$CODE),
    valueUnit = PREF_UNIT_WEIGHT$CODE,
    colValue = "quantity", 
    rank = FALSE,
    mode = 'plot+table'
  )
  
  #line chart server - FISHING ZONES
  line_chart_server(
    id = "fz", 
    lang = lang,
    label = i18n("FISHING_ZONE_LABEL"),
    df = data_logbooks |> left_join(fish_group),
    colDate = "date",
    colTarget = "fishing_zone",
    ylab = sprintf('%s (%s)',i18n("QUANTITY_PLOT_YLAB"),PREF_UNIT_WEIGHT$CODE),
    valueUnit = PREF_UNIT_WEIGHT$CODE,
    colValue = "quantity",
    rank = FALSE,
    mode = 'plot+table'
  )

  #main UI
  output$main <- renderUI({
    tagList(
      fluidRow(
        column(
          width = 12, style = "margin:12px;",
          htmlOutput(ns("logbooks_overview_info"))
        )
      ),
      uiOutput(ns("nb_infos")),
      fluidRow(
        div(
          class = "col-md-12",
          bs4Dash::tabsetPanel(
            vertical = TRUE,
            type = "pills",
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_GLOBAL_QUANTITY")), box_height = '70px', 
              line_chart_ui(
                id = ns("gq")
              )
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_GLOBAL_VESSEL")), box_height = '70px', 
              bs4Dash::box(
                title = NULL,
                width = 12,
                sidebar = bs4Dash::boxSidebar(
                  id = ns("gv_box"),
                  width = 25,
                  style = 'font-size:14px;',
                  selectInput(ns("gv_granu"),label = paste0(i18n("TEMPORAL_RESOLUTION")," :"), choices = c(i18n("YEARLY"),i18n("MONTHLY"),i18n("WEEKLY")))
                ),
                uiOutput(ns("gv_result"))
              )
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_VESSEL_TYPES")), box_height = '70px', 
              line_chart_ui(
                id = ns("vt")
              )
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_GEAR_TYPES")), box_height = '70px',
              line_chart_ui(
                id = ns("gt")
              )
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_SPECIES")), box_height = '70px',
              bs4Dash::tabsetPanel(
                type = "pills",
                tabPanel(
                  title = i18n("TABPANEL_BREAKDOWN_BY_SPECIES"),
                  line_chart_ui(
                    id = ns("sp")
                  )
                ),
                tabPanel(
                  title = i18n("TABPANEL_BREAKDOWN_BY_SPECIES_GROUPS"),
                  line_chart_ui(
                    id = ns("fg")
                  )
                )
              )
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_LANDING_SITES")), box_height = '70px',
              line_chart_ui(
                id = ns("ls")
              )
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_FISHING_ZONE")), box_height = '70px',
              line_chart_ui(
                id = ns("fz")
              )
            )
          )
        )
      )
    )
  })
  
  MODULE_END_TIME <- Sys.time()
  INFO("logbooks-overview: END")
  DEBUG_MODULE_PROCESSING_TIME("Logbooks-overview", MODULE_START_TIME, MODULE_END_TIME)
  
 })
  
}
