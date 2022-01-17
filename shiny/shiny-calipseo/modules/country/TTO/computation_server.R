#computation_server
computation_server <- function(input, output, session, pool) {
  
  ns <- session$ns
  print(names(session))
  
  AVAILABLE_INDICATORS <- getLocalCountryDataset("statistical_indicators")
  DATASETS <- getLocalCountryDatasets(appConfig)
  print(names(DATASETS))
  
  output$computation_info <- renderText({
    session$userData$page("computation")
    updatePageUrl("computation", session)
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
    computation = NULL,
    computing = FALSE,
    summary = data.frame(
      Year = character(0),
      Status = character(0),
      File = character(0),
      Actions = character(0),
      stringsAsFactors = FALSE
    ),
    filename = NULL
  )
 
  #getComputationStatus
  getComputationStatus <- function(indicator){
    
    staging <- list.files(path = "./out/staging", pattern = indicator$value)
    released <- list.files(path = "./out/release", pattern = indicator$value)
    values <- unique(c(unlist(strsplit(staging, ".xlsx")), unlist(strsplit(released, ".xlsx"))))
    years <- as.vector(sapply(values, function(x){ 
      x.splits <- unlist(strsplit(x,"_"))
      return(as.numeric(x.splits[length(x.splits)]))
    }))

    df <- data.frame(
      Year = character(0),
      Status = character(0),
      File = character(0),
      Actions = character(0),
      stringsAsFactors = FALSE
    )
    if(length(years)>0){
      years <- years[order(years)]
      status <- sapply(years, function(x){
        if(any(regexpr(x, released) > 0)){
          return("release")
        }else{
          return("staging")
        }
      })
      uuids <- NULL
      for(i in 1:length(years)){
        one_uuid = uuid::UUIDgenerate() 
        uuids <- c(uuids, one_uuid)
      }
      #print(uuids)
      df <- tibble::tibble(
        Year = years,
        Status = status,
        File = sapply(1:length(years), function(i){file.path("out", status[i], paste0(indicator$value, "_", years[i], ".xlsx"))}),
        Actions = paste0(
          shinyInput(downloadButtonCustom, length(years), indexes = uuids, id = 'button_result_', ns = ns, 
                     title = i18n("DOWNLOAD_RESULT_TITLE"), label = "", icon = icon("file-alt"),
                     onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))),
          if(!is.null(indicator$report_with)){
            shinyInput(downloadButtonCustom, length(years), indexes = uuids, id = 'button_report_', ns = ns, 
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
              paste0(type, "_", out$indicator$value, "_", df[i,"Year"],"_", toupper(df[i,"Status"]),".xlsx")
            },
            content = function(con) {
              filename <- paste0(out$indicator$value, "_", df[i,"Year"], ".xlsx")
              data <- as.data.frame(readxl::read_excel(file.path("out", df[i,"Status"], filename)))
              print(head(data))
              print(type)
              if(type == "report"){
                generateReport(session, out$indicator,  df[i,"Year"], data, con)
              }else{
                writexl::write_xlsx(data, con)
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
    indicator_msg <- sprintf(paste0(i18n("COMPUTATION_ACTIONBUTTON_LABEL")," %s - %s"), indicator$label, input$computation_year)
    
    cat(sprintf(paste0(i18n("QUERY_DATA_YEAR_LABEL")," %s\n"), input$computation_year))
    data_is_loaded <- TRUE
    progress$set(message = indicator_msg, detail = i18n("COMPUTATION_PROGRESS_SUB_LABEL"), value = 0)
    #required for 1st and 2d raised
    raw_data <- accessLandingForms(pool, year = input$computation_year)
    raised_1 <- DATASETS$raised_1[DATASETS$raised_1$year == input$computation_year,]
    #required for 2d raised
    raised_2 <- DATASETS$raised_2[DATASETS$raised_2$year == input$computation_year,]
    zones <- DATASETS$beach_to_beachzone
    species_groups <- DATASETS$species_groups
    
    #dependency of indicators? --> TODO 
    landings_1 <- NULL
    if("landings_1" %in% indicator$compute_with$fun_args){
      landings_1_release <- sprintf(paste0("out/",i18n("RELEASE_LANDING1_FILE_NAME"),"_%s.xlsx"), input$computation_year)
      if(file.exists(landings_1_release)){
        landings_1 <- as.data.frame(readxl::read_excel(landings_1_release))
      }else{
        data_is_loaded <- FALSE
      }
    }
    #required for FAO reporting
    landings_2 <- NULL
    if("landings_2" %in% indicator$compute_with$fun_args){
      landings_2_release <- sprintf(paste0("out/",i18n("RELEASE_LANDING2_FILE_NAME"),"_%s.xlsx"), input$computation_year)
      if(file.exists(landings_2_release)){
        landings_2 <- as.data.frame(readxl::read_excel(landings_2_release))
      }else{
        data_is_loaded <- FALSE
      }
    }
    ref_species <- accessRefSpecies(pool)
    
    if(data_is_loaded){
      cat(paste0(i18n("NOTIFICATION_DATALOAD_FOR_COMPUTATION"),"\n"))
      cat(sprintf(paste0(i18n("LOAD_R_COMPUTE_SCRIPT_LABEL"),"'%s'\n"), indicator$compute_with$script))
      progress$set(message = indicator_msg, detail = i18n("LOADING_R_COMPUTE_SCRIPT_PROGRESS_LABEL"), value = 20)
      source(indicator$compute_with$script)
      cat(sprintf(paste0(i18n("EXECUTE_INDICATOR_LABEL"),"'%s'\n"), indicator$value))
      args <- names(formals(indicator$compute_with$fun))
      fun_statement <- paste0("raw_output <- ", indicator$compute_with$fun, 
                              "(", paste0(sapply(args, function(arg){paste0(arg, " = ", indicator$compute_with$fun_args[[arg]])}), collapse=","), ")"
      )
      cat(sprintf(paste0(i18n("FUNCTION_STATEMENT_LABEL"),": %s\n"), fun_statement))
      progress$set(message = indicator_msg, detail = i18n("EXECUTE_R_SCRIPT_LABEL"), value = 40)
      eval(parse(text = fun_statement))
      if(is.null(raw_output)){
        cat(sprintf(paste0(i18n("ERROR_EXECUTING_INDICATORS_LABEL"),"'%s'\n"), indicator$value))
      }else{
        cat(sprintf(paste0(i18n("SUCCESS_COMPUTATION_INDICATOR_LABEL"),"'%s': %s results\n"), indicator$value, nrow(raw_output)))
      }
      
      progress$set(message = indicator_msg, detail = i18n("EXPORT_RESULTS_STAGING_LABEL"), value = 90)
      out$computation <- raw_output
      out$computing <- FALSE
      out$indicator <- indicator
      out$year <- input$computation_year
      out$filename <- paste0(indicator$value, "_", input$computation_year, ".xlsx")
      
      if(!dir.exists("out/staging")) dir.create("out/staging", recursive = TRUE)
      outputfilepath <- sprintf("out/staging/%s",out$filename)
      writexl::write_xlsx(raw_output, outputfilepath)
      
      progress$set(message = indicator_msg, detail = i18n("COMPUTATION_SUCCESSFUL_LABEL"), value = 100)
      out$summary <- getComputationStatus(indicator)
      print(out$summary)
      
    }else{
      errMsg <- sprintf(paste0(i18n("LANDING1_RELEASE_RESULT_MISSIG_YEAR"),"%s"), input$computation_year)
      cat(paste0(errMsg,"\n"))
      progress$set(message = errMsg, detail = i18n("ERROR_DURING_COMPUTATION"), value = 100)
    }
    
    enable("computeButton")
    
  })
  
  #observe for computation / download
  observe({
    
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
}
