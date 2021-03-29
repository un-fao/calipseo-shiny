#computation_server
computation_server <- function(input, output, session, pool) {
  
  AVAILABLE_INDICATORS <- getLocalCountryDataset("statistical_indicators")
  RAISED_1 <- getLocalCountryDataset("raised_1")
  RAISED_2 <- getLocalCountryDataset("raised_2")
  BEACH_TO_BEACHZONE <- getLocalCountryDataset("beach_to_beachzone")
  SPECIES_GROUPS <- getLocalCountryDataset("species_groups")
  
  output$computation_info <- renderText({
    session$userData$page("compute-indicators")
    updatePageUrl("compute-indicators", session)
    text <- "<h2>Statistics "
    text <- paste0(text, "<small>Compute statistics and download statistical reports</small>")
    text <- paste0(text, userTooltip("This section lets you compute the different statistical descriptors by year including the 1st raised landings (LAN), value (VAL), number of fishing trips (TRP) and ratios such as Landings/Trip (L/T), Value/Trip (V/T), and Value/Landing (P/K)",
                                     style = "font-size: 75%;"))
    text <- paste0(text, "</h2>")
    text <- paste0(text, "<hr>")
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
    filename = NULL
  )
  
  #observe on 'compute' button
  observeEvent(input$computeButton,{
    
    disable("computeButton")
    
    progress <- shiny::Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())
    
    out$computing <- TRUE
    raw_output <- NULL
    out$computation <- NULL
    cat(sprintf("Retrieve indicator for '%s'\n", input$computation_indicator))
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$label == input$computation_indicator})][[1]]
    indicator_msg <- sprintf("Compute %s - %s", indicator$label, input$computation_year)
    
    cat(sprintf("Query data for year %s\n", input$computation_year))
    data_is_loaded <- TRUE
    progress$set(message = indicator_msg, detail = "Initialize & read data...", value = 0)
    #required for 1st and 2d raised
    raw_data <- accessLandingForms(pool, year = input$computation_year)
    raised_1 <- RAISED_1[RAISED_1$year == input$computation_year,]
    #required for 2d raised
    raised_2 <- RAISED_2[RAISED_2$year == input$computation_year,]
    zones <- BEACH_TO_BEACHZONE
    species_groups <- SPECIES_GROUPS
    
    landings_1 <- NULL
    if("landings_1" %in% indicator$compute_with$fun_args){
      landings_1_release <- sprintf("out/release/artisanal_fisheries_landings_%s.xlsx", input$computation_year)
      if(file.exists(landings_1_release)){
        landings_1 <- as.data.frame(readxl::read_excel(landings_1_release))
      }else{
        data_is_loaded <- FALSE
      }
    }
    
    if(data_is_loaded){
      cat("All data assets are loaded and ready for computation\n")
      cat(sprintf("Load R 'compute' script '%s'\n", indicator$compute_with$script))
      progress$set(message = indicator_msg, detail = "Prepare R script...", value = 20)
      source(indicator$compute_with$script)
      cat(sprintf("Execute indicator '%s'\n", indicator$value))
      args <- names(formals(indicator$compute_with$fun))
      fun_statement <- paste0("raw_output <- ", indicator$compute_with$fun, 
                              "(", paste0(sapply(args, function(arg){paste0(arg, " = ", indicator$compute_with$fun_args[[arg]])}), collapse=","), ")"
      )
      cat(sprintf("Function statement: %s\n", fun_statement))
      progress$set(message = indicator_msg, detail = "Execute R script...", value = 40)
      eval(parse(text = fun_statement))
      if(is.null(raw_output)){
        cat(sprintf("Error while executing indicator '%s'\n", indicator$value))
      }else{
        cat(sprintf("Successful computation of indicator '%s': %s results\n", indicator$value, nrow(raw_output)))
      }
      
      progress$set(message = indicator_msg, detail = "Export result to 'staging' area...", value = 90)
      out$computation <- raw_output
      out$computing <- FALSE
      out$indicator <- indicator
      out$year <- input$computation_year
      out$filename <- paste0(indicator$value, "_", input$computation_year, ".xlsx")
      
      if(!dir.exists("out/staging")) dir.create("out/staging", recursive = TRUE)
      outputfilepath <- sprintf("out/staging/%s",out$filename)
      writexl::write_xlsx(raw_output, outputfilepath)
      
      progress$set(message = indicator_msg, detail = "Successful computation!", value = 100)
      
    }else{
      errMsg <- sprintf("Landings 1 released results missing for year %s", input$computation_year)
      cat(paste0(errMsg,"\n"))
      progress$set(message = errMsg, detail = "Error during computation", value = 100)
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
        cat("Filename:",out$filename)
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
      filename = function(){ paste0(unlist(strsplit(out$filename, "\\."))[1], "_report_DRAFT.xlsx")  },
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
        print(out$indicator$value)
        generateReport(session, out$indicator, out$year, out$computation, con)
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
        cat(sprintf("File '%s' existing, enabling release button shortcut\n", file.path("out/release", filename)))
        tags$div(
          tags$div(
            class = "row",
            downloadButtonCustom(
              session$ns("downloadReportShortcut"),
              'Generate & download report',
              icon = icon("file-excel"),
              class = "btn-lg btn-light"
            )
          )
        )
      }
    })
    
    #computation results
    output$results <- renderUI({
      if(out$computing){
        tags$div(
          id = "computation-results",
          tags$span(tags$b("Computing..."))
        )
      }else{
        if(!is.null(out$computation)){
          tags$div(
            id = "computation-results",
            h3(tags$b(out$indicator$label), " - ", tags$small(out$year)), hr(),
            p(em(paste0("Computation results with ", nrow(out$computation), " records - stored in 'staging' environment"))),
            tags$div(class = "col-md-6",
                     h4(
                       tags$b("Draft"), " ", 
                       tags$span(
                         class = "glyphicon glyphicon-info-sign tooltip-info", 
                         title = "This section allows you to download computation results and reports"
                       )
                     ), hr(),
                     downloadButtonCustom(
                       session$ns("downloadRawData"),
                       'Download computation results',
                       icon = icon("file-excel"),
                       class = "btn-lg btn-light"
                     ),
                     downloadButtonCustom(
                       session$ns("downloadReportInStaging"),
                       'Generate & download report',
                       icon = icon("file-excel"),
                       class = "btn-lg btn-light"
                     )
            ),
            tags$div(class = "col-md-6",
                     h4(
                       tags$b("Release")," ",
                       tags$span(
                         class = "glyphicon glyphicon-info-sign tooltip-info", 
                         title = "This section allows you to release the computation. It is possible to overwrite a previous release but this action should done cautiously! You can generate/download a report based on the released computation result."
                       )
                     ), hr(),
                     actionButton(session$ns("showReleaseModal"), "Create a release", class = "btn-primary"),
                     uiOutput(session$ns("releaseInfo"))
            )
          )
        }else{
          tags$div(
            id = "computation-results",
            tags$span(em("No computation run, select an indicator and year to launch a statistical computation!"))
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
        div(tags$b("Dataset already released. Click OK to overwrite release, or cancel.", style = "color: orange; font-weight:bold;"))
      }else{ 
        div(tags$b("Are your sure to want to create a release? If yes click OK to confirm, or cancel."))
      },
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("releaseButton"), "OK")
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
      removeModal()
    }
  })
  
  #UI RENDERERS
  #----------------
  # Display information about released file
  observe({
    output$releaseInfo <- renderUI({
      if (is.null(released_vals$filename) & !file.exists(file.path("out/release", out$filename))){
        tags$div(tags$b("No data released yet!"), style="margin-left:5px;padding:12px;")
      }else{
        tags$div(
          tags$div(
            class = "row",
            downloadButtonCustom(
              session$ns("downloadReportInRelease"),
              'Generate & download report',
              icon = icon("file-excel"),
              class = "btn-lg btn-light"
            )
          ),br(),
          tags$div(
            class = "row", style = "padding-left: 15px",
            p(tags$b("Last release:"), tags$span(file.info(file.path("out/release", out$filename))$mtime))
          )
        )
      }
    })
  })
}
