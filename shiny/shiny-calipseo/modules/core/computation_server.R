#computation_server
computation_server <- function(id, pool) {

 moduleServer(id, function(input, output, session){  
  
  session$userData$computation_new <- reactiveVal(NULL)
   
  ns <- session$ns
  
  AVAILABLE_INDICATORS <- getLocalCountryDataset(appConfig,"statistical_indicators.json")
  
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
    results = data.frame(
      Id = character(0),
      Period = character(0),
      File = character(0),
      Status = character(0),
      Date = character(0),
      Actions = character(0),
      stringsAsFactors = FALSE
    ),
    filename = NULL,
    filepath = NULL,
    filepath_release = NULL
  )
  
  available_periods<-reactiveVal(NULL)
 
  #getComputationResults
  getComputationResults <- function(indicator){
    
    staging <- list.files(path = sprintf("%s/staging/%s", appConfig$store, indicator$id), recursive = TRUE)
    released <- list.files(path = sprintf("%s/release/%s", appConfig$store, indicator$id), recursive = TRUE)
    values <- unique(c(unlist(strsplit(staging, ".csv")), unlist(strsplit(released, ".csv"))))
    periods <- as.vector(sapply(values, function(x){ 
      x.splits <- unlist(strsplit(x,"_"))
      period <- x.splits[length(x.splits)]
      return(period)
    }))
    
    df <- data.frame(
      Id = character(0),
      Period = character(0),
      File = character(0),
      Status = character(0),
      Date = character(0),
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
      df <- do.call("rbind", lapply(1:length(periods), function(i){
        filepath <- file.path(appConfig$store, status[i], indicator$id, paste0( values[i], ".csv"))
        tibble::tibble(
          uuid = uuids[i],
          Id = indicator$id,
          Period = periods[i],
          File = filepath,
          Status = status[i],
          Date = file.info(filepath)$mtime,
          Actions = as(
            tagList(
              #download result button
              downloadButtonCustom(
                  ns(paste0("button_download_result_", uuids[i])), 
                  title = i18n("BUTTON_DOWNLOAD_RESULT_TITLE"), label = "", icon = icon("file-alt"),
                  onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))               
              ), 
              #download report button
              if(!is.null(indicator$report_with)){
                downloadButtonCustom(
                  ns(paste0("button_download_report_", uuids[i])), 
                  title = i18n("BUTTON_DOWNLOAD_REPORT_TITLE"), label = "", icon = icon("file-contract"),
                  onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))               
                )
              }else{
                ""
              },
              #release button
              actionButton(inputId = ns(paste0('button_release_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                           title = i18n("BUTTON_RELEASE_TITLE"), label = "", icon = icon("upload"))
            )
          ,"character")
        )
      }))
      df <- df[order(df$Period),]
      
      #function to manage button server outputs
      manageButtonServerOutputs <- function(prefix, type){
        #clean button outputs reactives
        outs <- outputOptions(output)
        button_outs <- names(outs)[startsWith(names(outs), ns(prefix))]
        lapply(button_outs, function(name) {
          output[[name]] <<- NULL
        })
        lapply(1:nrow(df), function(i){
          idx = df[i,"uuid"]
          button_id <- paste0(prefix,idx)
          switch(type,
           #download result handler
           "result" = {
            output[[button_id]] <<- downloadHandler(
              filename = function() {
                paste0(type, "_", out$indicator$id, "_", df[i,"Period"],"_", toupper(df[i,"Status"]), ".csv")
              },
              content = function(con) {
                filename <- paste0(out$indicator$id, "_", df[i,"Period"], ".csv")
                filepath<-file.path(appConfig$store, df[i,"Status"], df[i,"Id"], gsub("-","/",df[i,"Period"]), filename)
                data <- as.data.frame(readr::read_csv(filepath))
                readr::write_csv(data, con)
              }
            )
           },
           #download report handler
           "report" = {
             output[[button_id]] <<- downloadHandler(
               filename = function() {
                 paste0(type, "_", out$indicator$id, "_", df[i,"Period"],"_", toupper(df[i,"Status"]), ".xlsx")
               },
               content = function(con) {
                 filename <- paste0(out$indicator$id, "_", df[i,"Period"], ".csv")
                 filepath<-file.path(appConfig$store, df[i,"Status"], df[i,"Id"], gsub("-","/",df[i,"Period"]), filename)
                 data <- as.data.frame(readr::read_csv(filepath))
                 generateReport(session, out$indicator,  df[i,"Period"], data, con)
               }
             )
           },
           #release handler
           "release" = {
             observeEvent(input[[button_id]],{
               filename <- paste0(out$indicator$id, "_", df[i,"Period"], ".csv")
               filepath_staging <- file.path(appConfig$store, "staging", df[i,"Id"], gsub("-","/",df[i,"Period"]), filename)
               filepath <- file.path(appConfig$store, "release", df[i,"Id"], gsub("-","/",df[i,"Period"]), filename)
               torelease(filepath_staging)
               alreadyReleased <- file.exists(filepath)
               showModal(releaseModal(session, warning = alreadyReleased))
             })
           }
          )
        })
      }
      
      manageButtonServerOutputs("button_download_result_", "result")
      manageButtonServerOutputs("button_download_report_", "report")
      manageButtonServerOutputs("button_release_", "release")

    }
    return(df)
  }
  
  #computation results
  output$computation_results <- renderDataTable(
    out$results,
    server = FALSE,
    escape = FALSE,
    rownames = FALSE,
    colnames = c("Id", i18n("COMPUTATION_RESULTS_TABLE_COLNAME_PERIOD"),i18n("COMPUTATION_RESULTS_TABLE_COLNAME_FILE"),
                 i18n("COMPUTATION_RESULTS_TABLE_COLNAME_STATUS"),i18n("COMPUTATION_RESULTS_TABLE_COLNAME_DATE"),i18n("COMPUTATION_RESULTS_TABLE_COLNAME_ACTIONS")),
    options = list(
      columnDefs = list(list(visible=FALSE, targets=0)),
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
    indicator_msg <- sprintf(paste0(i18n("COMPUTATION_ACTIONBUTTON_LABEL")," %s - %s"), 
                             indicator$label, paste0(input$computation_year,if(!is.null(input$computation_quarter)|!is.null(input$computation_month)){"-"}else{""},paste0(c(input$computation_quarter,input$computation_month),collapse="")))
    
    progress$set(message = indicator_msg, detail = i18n("COMPUTATION_PROGRESS_SUB_LABEL"), value = 0)
    
    cat(sprintf(paste0(i18n("LOAD_R_COMPUTE_SCRIPT_LABEL"),"'%s'\n"), indicator$compute_with$script))
    progress$set(message = indicator_msg, detail = i18n("LOAD_R_COMPUTE_SCRIPT_PROGRESS_LABEL"), value = 20)
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
        key <- ""
        value <- ""
        if(length(parts)==2){
          key <- parts[1]
          value <- parts[2]
        }
        fun_arg_eval <- switch(key,
          "data" = paste0(value, "(con = pool, ",paste0(indicator_args, sprintf(" = input$computation_%s", indicator_args), collapse = ", "),")"),
          #TODO add mode (release/staging) to getProcessOutput
          "process" = paste0("getProcessOutput(config = appConfig, id = \"", value,"\", ","mode = \"",input$computation_mode,"\", ", paste0(indicator_args, sprintf(" = input$computation_%s", indicator_args), collapse = ", "),")"),
          "local" = getLocalCountryDataset(appConfig,value),
          fun_arg_value
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
      out$filename <- paste0(indicator$id, "_", input$computation_year,if(!is.null(input$computation_quarter)|!is.null(input$computation_month)){"-"}else{""}, paste0(c(out$quarter, out$month), collapse=""), ".csv")
      out$filepath <- file.path(appConfig$store, "staging", indicator$id, input$computation_year, paste0(c(out$quarter, out$month), collapse=""), out$filename)
      out$filepath_release <- gsub("staging", "release", out$filepath)
      
      if(!dir.exists(dirname(out$filepath))) dir.create(dirname(out$filepath), recursive = TRUE)
      if(!dir.exists(dirname(out$filepath_release))) dir.create(dirname(out$filepath_release), recursive = TRUE)
      
      readr::write_csv(indicator_output, out$filepath, )
      
      progress$set(message = indicator_msg, detail = i18n("COMPUTATION_SUCCESSFUL_LABEL"), value = 100)
      session$userData$computation_new(Sys.time())
      out$results <- getComputationResults(indicator)
      
    }else{
      cat(sprintf(paste0(i18n("ERROR_EXECUTING_INDICATORS_LABEL"),"'%s'\n"), indicator$id))
      progress$set(message = i18n("ERROR_DURING_COMPUTATION"), value = 100)
      out$computing <- FALSE
    }

    enable("computeButton")
    
  })
  
  #observe for computation / download
  observe({
    req(!is.null(input$computation_indicator)&input$computation_indicator!="")
    #get indicator
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$label == input$computation_indicator})][[1]]
    out$indicator <- indicator
    out$computation <- NULL
    out$results <- getComputationResults(indicator)
    
    out$year <- input$computation_year
    out$quarter <- if("quarter"%in%indicator$compute_by$period &!is.null(input$computation_quarter)) paste0("Q",input$computation_quarter) else NULL
    out$month <- if("month"%in%indicator$compute_by$period &!is.null(input$computation_month)) paste0("M",input$computation_month) else NULL
    out$filename <- paste0(indicator$id, "_", input$computation_year,if(("quarter"%in%indicator$compute_by$period &!is.null(input$computation_quarter))|("month"%in%indicator$compute_by$period &!is.null(input$computation_month))){"-"}else{""}, paste0(c(out$quarter, out$month), collapse=""), ".csv")
    out$filepath <- file.path(appConfig$store, "staging", indicator$id, input$computation_year, paste0(c(out$quarter, out$month), collapse=""), out$filename)
    out$filepath_release <- gsub("staging", "release", out$filepath)
    
    #DOWNLOAD CONTROLLERS
    #-------------------------------------------------------------------------------------------
    #downloadReportShortcut
    output$downloadReportShortcut <- downloadHandler(
      filename = function(){paste0(out$indicator$value, "_", out$year, "_report_RELEASE.xlsx")},
      content = function(con){
        disable("downloadReportShortcut")
        filename <- paste0(out$indicator$value, "_", out$year, ".xlsx")
        data <- as.data.frame(readxl::read_excel(file.path(appConfig$store, "release", filename)))
        generateReport(session, out$indicator,  out$year, data, con)
        enable("downloadReportShortcut")
      }
    )
  })
  
  #UI RENDERERS
  #-----------------------------------------------------------------------------------------------------
  
  #computation_by
  output$computation_by <- renderUI({
    tagList(
      uiOutput(ns("computation_mode_wrapper")),
      uiOutput(ns("computation_year_wrapper")),
      uiOutput(ns("computation_month_wrapper")),
      uiOutput(ns("computation_quarter_wrapper"))
    )
  })
  
  observeEvent(c(input$computation_indicator,input$computation_mode),{
    req(!is.null(input$computation_indicator)&input$computation_indicator!="")
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$label == input$computation_indicator})][[1]]
    out$results <- getComputationResults(indicator)
    available_periods_parts <- unlist(strsplit(indicator$compute_by$available_periods[1], ":"))
    available_periods_key <- available_periods_parts[1]
    if(available_periods_key=="process"){
      req(!is.null(input$computation_mode)&input$computation_mode!="")
    }
    available_periods_value <- available_periods_parts[2]
    available_periods(switch(available_periods_key,
                             "data" = eval(parse(text=paste0(available_periods_value, "(con = pool)"))),
                             "process" = eval(parse(text=paste0("getStatPeriods(config = appConfig ,id = \"",available_periods_value,"\",mode = \"",input$computation_mode,"\")")))
    ))
    
  })
  
  observeEvent(input$computation_indicator,{
    req(!is.null(input$computation_indicator)&input$computation_indicator!="")
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$label == input$computation_indicator})][[1]]
    available_periods_parts <- unlist(strsplit(indicator$compute_by$available_periods[1], ":"))
    available_periods_key <- available_periods_parts[1]
    output$computation_mode_wrapper <- renderUI({
      if(available_periods_key=="process"){
        choices=c("release","staging")
        selectizeInput(
          ns("computation_mode"), label = i18n("COMPUTATION_MODE_LABEL"), 
          choices = choices, selected = "release"
        )
      }else{
        NULL
      }
    })
  })
    
  output$computation_year_wrapper <- renderUI({
    req(!is.null(available_periods()))

    selectizeInput(
      ns("computation_year"), label = i18n("COMPUTATION_YEAR_LABEL"), 
      choices = choices[order(choices)] , selected = if(!is.null(input$computation_year)){input$computation_year}else{max(choices)}, 
      options = list(placeholder = if(length(choices)>0){i18n("COMPUTATION_YEAR_PLACEHOLDER_LABEL")}else{i18n("COMPUTATION_YEAR_PLACEHOLDER_LABEL_EMPTY")}))
  })
  
  observeEvent(input$computation_year,{
    req(!is.null(input$computation_year))
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$label == input$computation_indicator})][[1]]
    output$computation_month_wrapper <- renderUI({
      if("month"%in%indicator$compute_by$period){
        choices=unique(subset(available_periods(),year==input$computation_year)$month)
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
    
    #TODO additional renderUI/dropdownlist (mode staging/release) conditioned by the indicator definition 
    #(to display if input is the result of another indicator)
    
    
  })
    
  #-------------------
  #RELEASE MANAGEMENT
  #-------------------
  torelease <- reactiveVal(NULL)
  
  #releaseModal
  releaseModal <- function(session, warning = FALSE) {
    modalDialog(
      if (warning){
        div(tags$b(i18n("DATASET_RELEASED_LABEL"), style = "color: orange; font-weight:bold;"))
      }else{ 
        div(tags$b(i18n("CONFIRMATION_TO_CREATE_RELEASE")))
      },
      footer = tagList(
        actionButton(session$ns("cancelRelease"),i18n("TO_CANCEL_RELEASE_LABEL")),
        actionButton(session$ns("goRelease"), i18n("TO_CREATE_RELEASE_LABEL"))
      )
    )
  }
  
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$goRelease, {
    file.copy(
      from = torelease(),
      to = gsub("staging", "release", torelease()),
      overwrite = TRUE
    )
    file.remove(torelease())
    session$userData$computation_new(Sys.time())
    if(file.exists(gsub("staging", "release", torelease()))){
      torelease(NULL)
      out$results <- getComputationResults(out$indicator)
      removeModal()
    }
  })
  observeEvent(input$cancelRelease,{
    torelease(NULL)
    removeModal()
  })
  
 })
}
