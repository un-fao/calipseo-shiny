#market_trade_data_exporter_server
market_trade_data_exporter_server <- function(id, parent.session, lang = NULL, pool, reloader){
  
  moduleServer(id, function(input, output, session){   
    
    #-----------------------------------------------------------------------------
    # Init
    #-----------------------------------------------------------------------------
    ns<-session$ns
    
    INFO("market_trade_data_exporter: START")
    MODULE_START_TIME <- Sys.time()  
    
    #-----------------------------------------------------------------------------
    # i18n
    #-----------------------------------------------------------------------------
    i18n_translator <- get_reactive_translator(lang)
    i18n <- function(key){ i18n_translator()$t(key) }

    #-----------------------------------------------------------------------------
    # Reactive values
    #-----------------------------------------------------------------------------
    data_market<-reactiveVal(NULL)
    data_processed<-reactiveVal(NULL)
    
    excel_title_label<-reactiveVal(NULL)
    excel_period_label<-reactiveVal(NULL)
    
    #-----------------------------------------------------------------------------
    # Data loading
    #-----------------------------------------------------------------------------
    data<-accessMarketTradeExporter(pool)
    data$date<-as.Date(data$date)
    
    default_market<-accessPrefMarket(pool)
    
    market_list<-data%>%
      dplyr::select(market_code,market_name)%>%
      dplyr::distinct()
    
    market_choices<-setNames(
      market_list$market_code,
      market_list$market_name
    )
    
    #-----------------------------------------------------------------------------
    # UI Outputs
    #-----------------------------------------------------------------------------
    #Market selector
    output$market_wrapper<-renderUI({
      selectizeInput(ns("market"),
                     label=i18n("MARKET_TRADE_DATA_SELECT_MARKET"),
                     choices=market_choices,
                     selected=default_market,
                     multiple = F,
                     options = list(
                       placeholder = i18n("MARKET_TRADE_DATA_SELECT_MARKET_PLACEHOLDER")
                     )
      )
    })
    
    #Date picker
    output$date_wrapper <- renderUI({
      req(input$market)
      req(data_market())
      
      fluidRow(
        column(6,
               dateInput(ns("date_from"), 
                         label=i18n("MARKET_TRADE_DATA_DATE_FROM"),
                         min = min(data_market()$date,na.rm=T),
                         max = max(data_market()$date,na.rm=T),
                         value = min(data_market()$date, na.rm = TRUE)
               )
        ),
        column(6,
               dateInput(ns("date_to"), 
                         label=i18n("MARKET_TRADE_DATA_DATE_TO"),
                         min = min(data_market()$date,na.rm=T),
                         max = max(data_market()$date,na.rm=T),
                         value = max(data_market()$date, na.rm = TRUE)
               )
        )
      )
    })
    
    #Selection button
    output$select_btn_wrapper<-renderUI({
      actionButton(ns("select_btn"),i18n("MARKET_TRADE_DATA_SELECT_BTN"),width="75%",class="btn btn-primary btn-block")
    })
    
    #Data availability sparkline plot
    output$market_activity_plot <- renderPlotly({
      
      req(data_market())
      req(input$date_from)
      req(input$date_to)
      
      market_activity <- data_market() %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(
          n_visits = dplyr::n_distinct(visit_id),
          .groups = "drop"
        )
      
      plotly::plot_ly(
        market_activity,
        x = ~date,
        y = ~n_visits,
        type = "bar",
        opacity = 0.8
      ) %>%
        plotly::layout(
          
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          
          margin = list(
            l = 5,
            r = 5,
            t = 5,
            b = 20
          ),
          
          xaxis = list(
            title = "",
            showgrid = FALSE
          ),
          
          yaxis = list(
            title = "",
            showgrid = FALSE,
            showticklabels = FALSE,
            ticks = ""
          ),
          
          shapes = list(
            
            #selected period
            list(
              type = "rect",
              x0 = input$date_from,
              x1 = input$date_to,
              y0 = -0.1,
              y1 = 1.1,
              yref = "paper",
              fillcolor = "rgba(220,53,69,0.15)",
              line = list(width = 0)
            ),
            
            # date from
            list(
              type = "line",
              x0 = input$date_from,
              x1 = input$date_from,
              y0 = -0.1,
              y1 = 1.1,
              yref = "paper",
              line = list(
                color = "#dc3545",
                width = 2
              )
            ),
            
            # date_to
            list(
              type = "line",
              x0 = input$date_to,
              x1 = input$date_to,
              y0 = 0,
              y1 = 1,
              yref = "paper",
              line = list(
                color = "#dc3545",
                width = 2
              )
            )
          )
        )
      
    })
    
    #Market output table
    output$market_table<-DT::renderDT(server=F,{
      
      req(data_processed())
      
      DT::datatable(
        data_processed(),
        escape = FALSE,
        rownames = FALSE,
        filter = "top",
        options = list(
          autoWidth = FALSE,
          dom = "Bfrtip",
          pageLength = 10,
          language = list(url = i18n("TABLE_LANGUAGE")),
          initComplete = dt_select2_filters(names(data_processed())[1:2])
        )
      )
    })
    
    #Table UI wrapper
    output$table_wrapper <- renderUI({
      
      req(data_processed())
      
      #Message if no data
      if(nrow(data_processed())==0){
        return(
          HTML(i18n("MARKET_TRADE_DATA_NO_DATA"))
        )
      }
      
      #Table if data
      DTOutput(ns("market_table"))
      
    })
    
    #Export button
    output$export_btn_wrapper<-renderUI({
      req(data_processed())
      downloadButton(ns("export_btn"),i18n("MARKET_TRADE_DATA_EXPORT_BTN"),width="75%",class="btn btn-success btn-block",icon=NULL)
    })
    
    #Result wrapper
    output$result_wrapper<-renderUI({
      tagList(
        fluidRow(
          h3(i18n("MARKET_TRADE_DATA_EXPORTER_TITLE"))
        ),
        fluidRow(
          column(2,uiOutput(ns("market_wrapper"))),
          column(4,uiOutput(ns("date_wrapper"))),
          column(2,uiOutput(ns("select_btn_wrapper")),offset=4)
        ),
        fluidRow(
          plotlyOutput(ns("market_activity_plot"), height = "50px")
        ),
        fluidRow(
          uiOutput(ns("table_wrapper"))
        ),
        fluidRow(
          column(2,uiOutput(ns("export_btn_wrapper")),offset=10)
        )
      )
    })
    
    #-----------------------------------------------------------------------------
    # Reactivity
    #-----------------------------------------------------------------------------
    
    #Update date when selected market is modified
    observeEvent(input$market,{
      selection<-data%>%filter(market_code%in% input$market)
      data_market(selection)
      
      updateDateInput(session, "date_from",
                      value = min(selection$date, na.rm = TRUE))
      
      updateDateInput(session, "date_to",
                      value = max(selection$date, na.rm = TRUE))
    })
    
    #Reset table when selection is modified
    observeEvent(list(input$market,input$date_from,input$date_to),{
      data_processed(NULL)
    },ignoreInit = TRUE)
    
    
    #-----------------------------------------------------------------------------
    # Data processing
    #-----------------------------------------------------------------------------
    
    #Filter data and run computations
    observeEvent(input$select_btn,{
      req(input$market)
      req(input$date_from)
      req(input$date_to)
      req(input$date_from <= input$date_to)
      
      title_label<-sprintf("%s %s", toupper(names(market_choices[market_choices==input$market])), toupper(i18n("MARKET_TRADE_DATA_EXPORTER_EXCEL_TITLE")))
      excel_title_label(title_label)
      
      period_label<-sprintf("%s : %s %s %s %s",i18n("TERM_PERIOD"),i18n("TERM_FROM"),input$date_from,i18n("TERM_TO"),input$date_to)
      excel_period_label(period_label)
      
      selection<-data_market() %>%
        filter(date >= input$date_from &
                 date <= input$date_to)
      
      #MIF - By species
      inflow_sp<-selection%>%
        dplyr::filter(trade_flow_type == "MIF")
      
      period_days <- as.numeric(input$date_to - input$date_from + 1)
      observed_days <- length(unique(inflow_sp$date))
      
      inflow_sp <- inflow_sp %>%
        dplyr::group_by(species_group, species) %>%
        dplyr::summarise(mif = sum(quantity, na.rm = TRUE) /observed_days * period_days,.groups = "drop")
      
      #WHS - By species
      wholesale_sp<-selection%>%
        dplyr::filter(trade_flow_type == "WHS")%>%
        dplyr::group_by(visit_id, species_group,species) %>%
        dplyr::summarise(weighted_mean_by_visit =
                           weighted.mean(price,quantity)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(species_group,species) %>%
        dplyr::summarise(whs = mean(weighted_mean_by_visit)) %>%
        dplyr::ungroup()
      
      #RET - By species
      retail_sp<-selection%>%
        dplyr::filter(trade_flow_type == "RET")%>%
        dplyr::group_by(visit_id, species_group,species) %>%
        dplyr::summarise(mean_by_visit = mean(price)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(species_group,species) %>%
        dplyr::summarise(ret = mean(mean_by_visit)) %>%
        dplyr::ungroup()
      
      #join results by species group and species
      res_sp <- inflow_sp %>%
        dplyr::left_join(wholesale_sp) %>%
        dplyr::left_join(retail_sp) 
      
      #Compute aggregate by species group
      res_gr <- res_sp %>%
        dplyr::mutate(species ="zzz")%>%
        dplyr::group_by(species_group,species)%>%
        dplyr::summarise(
          mif = sum(mif),
          whs = sum(mif*whs) / sum(mif),
          ret = sum(mif*ret) / sum(mif)
        )%>%
        dplyr::ungroup()
      
      #Arrange, round and rename result
      res<-res_sp%>%
        dplyr::bind_rows(res_gr)%>%
        dplyr::arrange(species_group,species)%>%
        dplyr::mutate(species = ifelse(species == "zzz","***",species),
               pm = ret - whs)%>%
        dplyr::mutate(mif=round(mif,2),
               whs=round(whs,3),
               ret=round(ret,3),
               pm=round(pm,3)
        )%>%
        dplyr::select(species_group,species,mif,whs,ret,pm)%>%
        setNames(c(i18n("MARKET_TRADE_DATA_EXPORTER_COL_SPGR"),
                   i18n("MARKET_TRADE_DATA_EXPORTER_COL_SP"),
                   i18n("MARKET_TRADE_DATA_EXPORTER_COL_MIF"),
                   i18n("MARKET_TRADE_DATA_EXPORTER_COL_WHS"),
                   i18n("MARKET_TRADE_DATA_EXPORTER_COL_RET"),
                   i18n("MARKET_TRADE_DATA_EXPORTER_COL_PM") 
        ))
      
      data_processed(res)
    })
    
    #-----------------------------------------------------------------------------
    # Export
    #-----------------------------------------------------------------------------
    
    #Export button action
    output$export_btn <- downloadHandler(
      filename = function() {
        sprintf("%s_%s_%s.xlsx",input$market,input$date_from,input$date_to)
      },
      content = function(file) {
        
        wb <- createWorkbook()
        addWorksheet(wb, "Data")
        
        openxlsx::writeData(
          wb,
          "Data",
          x = excel_title_label(),
          startCol = "A",
          startRow = 1
        )
        
        openxlsx::writeData(
          wb,
          "Data",
          x = excel_period_label(),
          startCol = "A",
          startRow = 2
        )
        
        openxlsx::writeDataTable(
          wb,
          "Data",
          data_processed(),
          startCol = "A",
          startRow = 3,
          tableStyle = "TableStyleLight9"
        )
        
        openxlsx::setColWidths(
          wb,
          "Data",
          cols = 1:ncol(data_processed()),
          widths = "auto"
        )
        
        saveWorkbook(wb, file)
      }
    )
    
    #-----------------------------------------------------------------------------
    # End
    #-----------------------------------------------------------------------------
    
    MODULE_END_TIME <- Sys.time()
    INFO("market_trade_data_exporter: END")
    DEBUG_MODULE_PROCESSING_TIME("market_trade_data_exporter", MODULE_START_TIME, MODULE_END_TIME)
    
  })  
  
}