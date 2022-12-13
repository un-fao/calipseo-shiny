#computation_server
computation_server <- function(id, pool) {

 moduleServer(id, function(input, output, session){  
  
  ns <- session$ns
  print(names(session))
  
  
  AVAILABLE_INDICATORS <- getLocalCountryDataset(appConfig,"statistical_indicators.json")
  print(AVAILABLE_INDICATORS)
  
  output$computation_info <- renderText({
    text <- paste0("<h2>", i18n("COMPUTATION_TITLE")," <small>", i18n("COMPUTATION_SUBTITLE"),
                   userTooltip("This section lets you compute the different statistical descriptors by year including the 1st raised landings (LAN), value (VAL), number of fishing trips (TRP) and ratios such as Landings/Trip (L/T), Value/Trip (V/T), and Value/Landing (P/K)",
                               style = "font-size: 75%;"),"</small></h2><hr>")
    text
  })
  
  
  #--------------------------------
  #COMPUTATION / EXPORT MANAGEMENT
  #--------------------------------
  
  out <- reactiveValues(
    indicator = NULL,
    year = NULL,
    quarter = NULL,
    month = NULL,
    computation = NULL,
    computing = FALSE,
    summary = data.frame(
      Year = character(0),
      Status = character(0),
      File = character(0),
      Actions = character(0),
      stringsAsFactors = FALSE
    ),
    filename = NULL,
    filepath = NULL
  )
  
  available_periods<-reactiveVal(NULL)
 
  #getComputationStatus
  getComputationStatus <- function(indicator){
    
    staging <- list.files(path = sprintf("./out/staging/%s", indicator$id), recursive = TRUE)
    released <- list.files(path = sprintf("./out/release/%s", indicator$id), recursive = TRUE)
    print(staging)
    print(released)
    values <- unique(c(unlist(strsplit(staging, ".csv")), unlist(strsplit(released, ".csv"))))
    print(values)
    periods <- as.vector(sapply(values, function(x){ 
      x.splits <- unlist(strsplit(x,"_"))
      period <- x.splits[length(x.splits)]
      return(period)
    }))

    print(periods)
    
    df <- data.frame(
      Id = character(0),
      Period = character(0),
      Status = character(0),
      File = character(0),
      Actions = character(0),
      stringsAsFactors = FALSE
    )
    if(length(periods)>0){
      status <- sapply(periods, function(x){
       if(any(regexpr(x, released) > 0)){
         return("release")
       }else{
         return("staging")
       }
      })
      
      uuids <- NULL
      for(i in 1:length(periods)){
        one_uuid = uuid::UUIDgenerate() 
        uuids <- c(uuids, one_uuid)
      }
      #print(uuids)
      df <- tibble::tibble(
        Id = indicator$id,
        Period = periods,
        Status = status,
        File = sapply(1:length(periods), function(i){file.path("out", status[i], indicator$id, paste0( values[i], ".csv"))}),
        Actions = paste0(
          shinyInput(downloadButtonCustom, length(periods), indexes = uuids, id = 'button_result_', ns = ns, 
                     title = i18n("DOWNLOAD_RESULT_TITLE"), label = "", icon = icon("file-alt"),
                     onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))),
          if(!is.null(indicator$report_with)){
            shinyInput(downloadButtonCustom, length(periods), indexes = uuids, id = 'button_report_', ns = ns, 
                     title = i18n("DOWNLOAD_REPORT_TITLE"), label = "", icon = icon("file-contract"),
                     onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button")))
          }else{
            ""
          }
        )
      )
      
      #function to manage button server outputs
      manageButtonServerOutputs <- function(prefix, type){
        #clean button outputs reactives
        outs <- outputOptions(output)
        button_outs <- names(outs)[startsWith(names(outs), ns(prefix))]
        lapply(button_outs, function(name) {
          output[[name]] <<- NULL
        })
        #add new downloadHandler (for each button)
        lapply(1:nrow(df), function(i){
          idx = uuids[i]
          button_output <- paste0(prefix,idx)
          output[[button_output]] <<- downloadHandler(
            filename = function() {
              paste0(type, "_", out$indicator$id, "_", df[i,"Period"],"_", toupper(df[i,"Status"]), ifelse(type=="report",".xlsx", ".csv"))
            },
            content = function(con) {
              
              #TODO refactoring
              filename <- paste0(out$indicator$id, "_", df[i,"Period"], ".csv")
              filepath<-file.path("out", df[i,"Status"], df[i,"Id"], gsub("-","/",df[i,"Period"]), filename)
              print(filepath)
              data <- as.data.frame(readr::read_csv(filepath))
              print(head(data))
              print(type)
              if(type == "report"){
                generateReport(session, out$indicator,  df[i,"Year"], data, con)
              }else{
                readr::write_csv(data, con)
              }
            }
          )
        })
      }
      
      manageButtonServerOutputs("button_result_", "result")
      manageButtonServerOutputs("button_report_", "report")
      
     
      
    }
    return(df)
  }
  
  #computation status
  output$computation_summary <- renderDataTable(
    out$summary,
    server = FALSE,
    escape = FALSE,
    rownames = FALSE,
    colnames = c(i18n("COMPUTATION_STATUS_TABLE_COLNAME_1"),i18n("COMPUTATION_STATUS_TABLE_COLNAME_2"),
                 i18n("COMPUTATION_STATUS_TABLE_COLNAME_3"),i18n("COMPUTATION_STATUS_TABLE_COLNAME_4")),
    options = list(
      paging = FALSE,
      searching = FALSE,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
      language = list(url = i18n("TABLE_LANGUAGE"))
    )
  )
  
  #observe on 'compute' button
  observeEvent(input$computeButton,{
    
    disable("computeButton")
    
    progress <- shiny::Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())
    
    out$computing <- TRUE
    raw_output <- NULL
    out$computation <- NULL
    cat(sprintf(paste0(i18n("RETRIEVE_INDICATOR_FOR_LABEL")," '%s'\n"), input$computation_indicator))
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$label == input$computation_indicator})][[1]]
    indicator_msg <- sprintf(paste0(i18n("COMPUTATION_ACTIONBUTTON_LABEL")," %s - %s"), indicator$label, paste0(input$computation_year,"-",paste0(c(input$computation_quarter,input$computation_month),collapse="")))
    
    progress$set(message = indicator_msg, detail = i18n("COMPUTATION_PROGRESS_SUB_LABEL"), value = 0)
    
    cat(sprintf(paste0(i18n("LOAD_R_COMPUTE_SCRIPT_LABEL"),"'%s'\n"), indicator$compute_with$script))
    progress$set(message = indicator_msg, detail = i18n("LOADING_R_COMPUTE_SCRIPT_PROGRESS_LABEL"), value = 20)
    source(indicator$compute_with$script) #TODO to check if still needed
    
    #possible inputs
    #input$computation_year
    #input$computation_quarter
    #input$computation_month
    indicator_args <- switch(indicator$compute_by$period,
      "year" = c("year"),
      "quarter" = c("year","quarter"),
      "month" = c("year", "month")
    )
    
    #compute indicator evaluating fun
    cat(sprintf(paste0(i18n("EXECUTE_INDICATOR_LABEL"),"'%s'\n"), indicator$value))
    progress$set(message = indicator_msg, detail = i18n("EXECUTE_R_SCRIPT_LABEL"), value = 40)
    indicator_output <- try(eval(parse(text = paste0(indicator$compute_with$fun, "(",
      paste0(names(indicator$compute_with$fun_args), " = ", sapply(names(indicator$compute_with$fun_args), function(x){
        fun_arg_value <- indicator$compute_with$fun_args[[x]]
        parts <- unlist(strsplit(fun_arg_value, ":"))
        key <- parts[1]
        value <- parts[2]
        fun_arg_eval <- switch(key,
          "data" = paste0(value, "(con = pool, ",paste0(indicator_args, sprintf(" = input$computation_%s", indicator_args), collapse = ", "),")"),
          "process" = paste0("getProcessOutput(id = ", value,", ", paste0(indicator_args, sprintf(" = input$computation_%s", indicator_args), collapse = ", "),")"),
          "local" = getLocalCountryDataset(appConfig,value)
        )
        return(fun_arg_eval)
      }), collapse = ", ")
    ,")"))))

    if(!is(indicator_output, "try-error")){
      cat(sprintf(paste0(i18n("SUCCESS_COMPUTATION_INDICATOR_LABEL"),"'%s': %s results\n"), indicator$value, nrow(raw_output)))
      
      #export to computation directory
      progress$set(message = indicator_msg, detail = i18n("EXPORT_RESULTS_STAGING_LABEL"), value = 90)
      out$computation <- indicator_output
      out$computing <- FALSE
      out$indicator <- indicator
      out$year <- input$computation_year
      out$quarter <- if(!is.null(input$computation_quarter)) paste0("Q",input$computation_quarter) else NULL
      out$month <- if(!is.null(input$computation_month)) paste0("M",input$computation_month) else NULL
      print(input$computation_month)
      out$filename <- paste0(indicator$id, "_", input$computation_year,"-", paste0(c(out$quarter, out$month), collapse=""), ".csv")
      out$filepath <- file.path("out/staging", indicator$id, input$computation_year, paste0(c(out$quarter, out$month), collapse=""), out$filename)
    
      print(out$filepath)
      
      if(!dir.exists(dirname(out$filepath))) dir.create(dirname(out$filepath), recursive = TRUE)
  
      readr::write_csv(indicator_output, out$filepath, )
      
      progress$set(message = indicator_msg, detail = i18n("COMPUTATION_SUCCESSFUL_LABEL"), value = 100)
      out$summary <- getComputationStatus(indicator)
      print(out$summary)
      
    }else{
      cat(sprintf(paste0(i18n("ERROR_EXECUTING_INDICATORS_LABEL"),"'%s'\n"), indicator$id))
      progress$set(message = errMsg, detail = i18n("ERROR_DURING_COMPUTATION"), value = 100)
    }

    enable("computeButton")
    
  })
  
  #observe for computation / download
  observe({
    req(!is.null(input$computation_indicator)&input$computation_indicator!="")
    #get indicator
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$label == input$computation_indicator})][[1]]
    out$indicator <- indicator
    out$year <- input$computation_year
    out$filename <- paste0(indicator$value, "_", input$computation_year, ".xlsx")
    out$computation <- NULL
    out$summary <- getComputationStatus(indicator)
    
    #DOWNLOAD CONTROLLERS
    #-------------------------------------------------------------------------------------------
    #downloadReportShortcut
    output$downloadReportShortcut <- downloadHandler(
      filename = function(){paste0(out$indicator$value, "_", out$year, "_report_RELEASE.xlsx")},
      content = function(con){
        disable("downloadReportShortcut")
        filename <- paste0(out$indicator$value, "_", out$year, ".xlsx")
        data <- as.data.frame(readxl::read_excel(file.path("out/release", filename)))
        generateReport(session, out$indicator,  out$year, data, con)
        enable("downloadReportShortcut")
      }
    )
    
    #downloadRawData
    output$downloadRawData <- downloadHandler(
      filename = function(){
        cat(paste0(i18n("FILENAME_LABEL"),":"),out$filename)
        out$filename 
      },
      content = function(con){
        disable("downloadRawData")
        write_xlsx(out$computation, con)
        enable("downloadRawData")
      }
    )
    #downloadReportInStaging
    output$downloadReportInStaging <- downloadHandler(
      filename = function(){ paste0(unlist(strsplit(out$filename, "\\."))[1], "_report_STAGING.xlsx")  },
      content = function(con){
        disable("downloadReportInStaging")
        generateReport(session, out$indicator, out$year, out$computation, con)
        enable("downloadReportInStaging")
      }
    )
    #downloadReportInRelease
    output$downloadReportInRelease <- downloadHandler(
      filename = function(){ paste0(unlist(strsplit(out$filename, "\\."))[1], "_report_RELEASE.xlsx")  },
      content = function(con){
        disable("downloadReportInRelease")
        filename <- paste0(out$indicator$value, "_", out$year, ".xlsx")
        data <- as.data.frame(readxl::read_excel(file.path("out/release", filename)))
        generateReport(session, out$indicator, out$year, data, con)
        enable("downloadReportInRelease")
      }
    )
    
    #UI RENDERERS
    #-----------------------------------------------------------------------------------------------------
    
    #computation_by
    output$computation_by <- renderUI({
      tagList(
        uiOutput(ns("computation_year_wrapper")),
        uiOutput(ns("computation_month_wrapper")),
        uiOutput(ns("computation_quarter_wrapper"))
      )
    })
      
    observeEvent(input$computation_indicator,{
      req(!is.null(input$computation_indicator)&input$computation_indicator!="")
      print(input$computation_indicator)
      indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$label == input$computation_indicator})][[1]]
      available_periods_parts <- unlist(strsplit(indicator$compute_by$available_periods[1], ":"))
      available_periods_key <- available_periods_parts[1]
      available_periods_value <- available_periods_parts[2]
      available_periods <- available_periods(eval(parse(text=paste0(available_periods_value, "(con = pool)"))))
      print(available_periods())
    })
    
      output$computation_year_wrapper <- renderUI({
        req(!is.null(available_periods()))
        choices=unique(available_periods()$year)
        print(choices)
        selectizeInput(
          ns("computation_year"), label = i18n("COMPUTATION_YEAR_LABEL"), 
          choices = choices[order(choices)] , selected = if(!is.null(input$computation_year)){input$computation_year}else{max(choices)}, 
          options = list(placeholder = i18n("COMPUTATION_YEAR_PLACEHOLDER_LABEL")))
      })
      
      observeEvent(input$computation_year,{
        req(!is.null(input$computation_year))
        
        output$computation_month_wrapper <- renderUI({
          if("month"%in%indicator$compute_by$period){
            choices=unique(subset(available_periods(),year==input$computation_year)$month)
            print(choices)
            selectizeInput(
              ns("computation_month"), label = i18n("COMPUTATION_MONTH_LABEL"), 
              choices = choices[order(choices)], selected = NULL, 
              options = list(placeholder = i18n("COMPUTATION_MONTH_PLACEHOLDER_LABEL"))
            )
          }else{
            NULL
          }
        })
        
        output$computation_quarter_wrapper <- renderUI({
          if("quarter"%in%indicator$compute_by$period){
            choices=unique(subset(available_periods(),year==input$computation_year)$quarter)
          selectizeInput(
            ns("computation_quarter"), label = i18n("COMPUTATION_QUARTER_LABEL"), 
            choices = choices[order(choices)], selected = NULL, 
            options = list(placeholder = i18n("COMPUTATION_QUARTER_PLACEHOLDER_LABEL"))
          )
          }else{
            NULL
          }
        })
      })
    
    #releaseInfoShortcut (if indicator yet released)
    output$releaseInfoShortcut <- renderUI({
      filename <- paste0(out$indicator$value, "_", out$year, ".xlsx")
      if(!dir.exists("out/release")) dir.create("out/release", recursive = TRUE)
      if (file.exists(file.path("out/release", filename))){
        cat(sprintf(paste0(i18n("FILE_LABEL")," %s ", i18n("RELEASE_SHORTCUT_BUTTON_LABEL"),"\n"), file.path("out/release", filename)))
        tags$div(
          tags$div(
            class = "row",
            if(!is.null(out$indicator$report_with)) {
              downloadButtonCustom(
                session$ns("downloadReportShortcut"),
                i18n("GENERATE_DOWNLOAD_REPORT_LABEL"),
                icon = icon("file-excel"),
                class = "btn-lg btn-light"
              )
            }else{tags$div()}
          )
        )
      }
    })
    
    #computation results
    output$results <- renderUI({
      if(out$computing){
        tags$div(
          id = "computation-results",
          tags$span(tags$b(i18n("COMPUTATION_IN_PROGRESS")))
        )
      }else{
        if(nrow(out$summary)>0){
          tags$div(
            id = "computation-results",
            h3(tags$b(out$indicator$label), " - ", tags$small(out$year)), hr(),
            p(em(paste0(i18n("COMPUTATION_RESULTS_WITH_LABEL")," ", nrow(out$computation)," ",i18n("RECORDS_STORED_IN_STAGING_LABEL")))),
            tags$div(class = "col-md-6",
                     h4(
                       tags$b(i18n("DRAFT_LABEL")), " ", 
                       tags$span(
                         class = "glyphicon glyphicon-info-sign tooltip-info", 
                         title = i18n("COMPUTATION_SECTION_TITLE")
                       )
                     ), hr(),
                     downloadButtonCustom(
                       session$ns("downloadRawData"),
                       i18n("COMPUTATION_DOWNLOAD_RESULT_LABEL"),
                       icon = icon("file-excel"),
                       class = "btn-lg btn-light"
                     ),
                     if(!is.null(out$indicator$report_with)) {
                       downloadButtonCustom(
                         session$ns("downloadReportInStaging"),
                         i18n("GENERATE_DOWNLOAD_REPORT_LABEL"),
                         icon = icon("file-excel"),
                         class = "btn-lg btn-light"
                       )
                     }else{ tags$div() }
            ),
            tags$div(class = "col-md-6",
                     h4(
                       tags$b(i18n("RELEASE_LABEL"))," ",
                       tags$span(
                         class = "glyphicon glyphicon-info-sign tooltip-info", 
                         title = i18n("RELEASE_COMPUTATION_MESSAGE")
                       )
                     ), hr(),
                     actionButton(session$ns("showReleaseModal"), i18n("CREATE_RELEASE_LABEL"), class = "btn-primary"),
                     uiOutput(session$ns("releaseInfo"))
            )
          )
        }else{
          tags$div(
            id = "computation-results",
            tags$span(em(i18n("NO_COMPUTATION_RUN_LABEL")))
          )
        }
      }
    })
    
  })
  
  #-------------------
  #RELEASE MANAGEMENT
  #-------------------
  released_vals <- reactiveValues(filename = NULL)
  
  #releaseModal
  releaseModal <- function(session, warning = FALSE) {
    modalDialog(
      if (warning){
        div(tags$b(i18n("DATASET_RELEASED_LABEL"), style = "color: orange; font-weight:bold;"))
      }else{ 
        div(tags$b(i18n("CONFIRMATION_TO_CREATE_RELEASE")))
      },
      footer = tagList(
        modalButton(i18n("TO_CANCEL_RELEASE_LABEL")),
        actionButton(session$ns("releaseButton"), i18n("TO_CREATE_RELEASE_LABEL"))
      )
    )
  }
  
  # Show modal when button is clicked.
  observeEvent(input$showReleaseModal, {
    alreadyReleased <- file.exists(file.path("out/release", out$filename))
    showModal(releaseModal(session, warning = alreadyReleased))
  })
  
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$releaseButton, {
    file.copy(
      from = file.path("out/staging", out$filename),
      to = file.path("out/release", out$filename),
      overwrite = TRUE
    )
    if(file.exists(file.path("out/release", out$filename))){
      released_vals$filename <- out$filename
      out$summary <- getComputationStatus(out$indicator)
      removeModal()
    }
  })
  
  #UI RENDERERS
  #----------------
  # Display information about released file
  observe({
    output$releaseInfo <- renderUI({
      if (is.null(released_vals$filename) & !file.exists(file.path("out/release", out$filename))){
        tags$div(tags$b(i18n("RELEASE_NO_DATA_LABEL")), style="margin-left:5px;padding:12px;")
      }else{
        tags$div(
          tags$div(
            class = "row",
            if(!is.null(out$indicator$report_with)) {
              downloadButtonCustom(
                session$ns("downloadReportInRelease"),
                i18n("GENERATE_DOWNLOAD_REPORT_LABEL"),
                icon = icon("file-excel"),
                class = "btn-lg btn-light"
              )
            }else{ tags$div()}
          ),br(),
          tags$div(
            class = "row", style = "padding-left: 15px",
            p(tags$b(paste0(i18n("LAST_RELEASE_LABEL"),":")), tags$span(file.info(file.path("out/release", out$filename))$mtime))
          )
        )
      }
    })
  })
  
 })
}
