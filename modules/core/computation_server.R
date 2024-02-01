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
  selected_indicator<-reactiveValues(
    indicator = NULL,
    period_key = NULL,
    period_value = NULL
  )
 
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
               filename <- paste0(df[i,"Id"], "_", df[i,"Period"], ".csv")
               filepath_staging <- file.path(appConfig$store, "staging", df[i,"Id"], gsub("-","/",df[i,"Period"]), filename)
               filepath <- file.path(appConfig$store, "release", df[i,"Id"], gsub("-","/",df[i,"Period"]), filename)
               torelease(filepath_staging)
               print(filepath_staging)
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
      paging = TRUE,
      pageLength=12,
      searching = FALSE,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
      language = list(url = i18n("TABLE_LANGUAGE"))
    )
  )
  
  computeIndicator<-function(out,session,computation_indicator,computation_target,computation_year,computation_quarter=NULL,computation_month=NULL,compute_dependent_indicators=FALSE){
    
    if(compute_dependent_indicators){
      
      indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == computation_indicator})][[1]]
      indicators<-unlist(sapply(names(indicator$compute_with$fun_args), function(x){
        fun_arg_value <- indicator$compute_with$fun_args[[x]]
        parts <- unlist(strsplit(fun_arg_value, ":"))
        key <- ""
        value <- ""
        if(length(parts)==2){
          key <- parts[1]
          value <- parts[2]
        }
        if(key=="process")return(value)}))
      
      for(indicator in indicators){
        process_def = AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == indicator})][[1]]
        
        release_periods <- getStatPeriods(config = appConfig, id = indicator, target = "release")
        #release_periods df(year, month)
        year_already_computed <- release_periods[release_periods$year == computation_year, ]

        if(nrow(year_already_computed)>0){
          periods_already_computed <- year_already_computed[,process_def$compute_by$period]
          periods_to_compute <- switch(process_def$compute_by$period,
                                       "month" = setdiff(rep(1:12), periods_already_computed),
                                       "quarter" = setdiff(rep(1:4), periods_already_computed)
          )
        }else{
          periods_to_compute <- switch(process_def$compute_by$period,
                                       "month" = rep(1:12),
                                       "quarter" = rep(1:4)
          )
        }
        
        print("THIS IS THE TEST-START")
        print(periods_to_compute)
        print("THIS IS THE TEST-END")
        

          
          if(length(periods_to_compute)>0){
          
          for(period in periods_to_compute){
            computeIndicator(
              out = out,
              session = session,
              computation_indicator = indicator,
              computation_target = computation_target,
              computation_year = computation_year,
              computation_quarter = if(process_def$compute_by$period == "quarter") period else NULL ,
              computation_month = if(process_def$compute_by$period == "month") period else NULL,
              compute_dependent_indicators = TRUE
            )
          }
          }
        
      }
      
    }
    
    progress <- shiny::Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())
    
    out$computing <- TRUE
    raw_output <- NULL
    out$computation <- NULL
    cat(sprintf(paste0(i18n("RETRIEVE_INDICATOR_FOR_LABEL")," '%s'\n"), computation_indicator))
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == computation_indicator})][[1]]
    indicator_msg <- sprintf(paste0(i18n("COMPUTATION_ACTIONBUTTON_LABEL")," %s - %s"), 
                             indicator$label, paste0(computation_year,if(!is.null(computation_quarter)|!is.null(computation_month)){"-"}else{""},paste0(c(computation_quarter,computation_month),collapse="")))
    
    progress$set(message = indicator_msg, detail = i18n("COMPUTATION_PROGRESS_SUB_LABEL"), value = 0)
    
    cat(sprintf(paste0(i18n("LOAD_R_COMPUTE_SCRIPT_LABEL"),"'%s'\n"), indicator$compute_with$script))
    progress$set(message = indicator_msg, detail = i18n("LOAD_R_COMPUTE_SCRIPT_PROGRESS_LABEL"), value = 20)
    source(indicator$compute_with$script) #TODO to check if still needed
    
    #possible inputs
    indicator_args <- switch(indicator$compute_by$period,
                             "year" = c("year"),
                             "quarter" = c("year","quarter"),
                             "month" = c("year", "month")
    )
    
    #compute indicator evaluating fun
    cat(sprintf(paste0(i18n("EXECUTE_INDICATOR_LABEL"),"'%s'\n"), indicator$value))
    progress$set(message = indicator_msg, detail = i18n("EXECUTE_R_SCRIPT_LABEL"), value = 40)
    
    indicator_script_command<-paste0(indicator$compute_with$fun, "(",
                                     paste0("con = pool, ",paste0(indicator_args, sprintf(" = computation_%s", indicator_args), collapse = ", "),
                                            if(length(indicator$compute_with$fun_args)>0)","),
                                     if(length(indicator$compute_with$fun_args)>0){
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
                                                              "data" = paste0(value, "(con = pool, ",paste0(indicator_args, sprintf(" = computation_%s", indicator_args), collapse = ", "),")"),
                                                              #TODO add mode (release/staging) to getProcessOutput
                                                              "process" = paste0("getProcessOutputs(config = appConfig, id = \"", value,"\", ","target = \"",computation_target,"\", ", paste0(indicator_args, sprintf(" = computation_%s", indicator_args), collapse = ", "),")"),
                                                              "local" = paste0("getLocalCountryDataset(appConfig, \"",value,"\")"),
                                                              fun_arg_value
                                       )
                                       return(fun_arg_eval)
                                     }), collapse = ", ")}
                                     ,")")
    print(indicator_script_command)
    
    indicator_output <- try(eval(parse(text = indicator_script_command)))
    
    if(!is(indicator_output, "try-error")){
      cat(sprintf(paste0(i18n("SUCCESS_COMPUTATION_INDICATOR_LABEL"),"'%s': %s results\n"), indicator$value, nrow(raw_output)))
      
      #export to computation directory
      progress$set(message = indicator_msg, detail = i18n("EXPORT_RESULTS_STAGING_LABEL"), value = 90)
      out$computation <- indicator_output
      out$computing <- FALSE
      out$indicator <- indicator
      out$year <- computation_year
      out$quarter <- NULL
      out$quarter <- if("quarter"%in%indicator$compute_by$period)if(!is.null(computation_quarter))if(!computation_quarter!="")if(startsWith(as.character(computation_quarter),"Q")){computation_quarter}else{paste0("Q",computation_quarter)}
      out$month <- NULL
      out$month <- if("month"%in%indicator$compute_by$period)if(!is.null(computation_month))if(computation_month!="")if(startsWith(as.character(computation_month),"M")){computation_month}else{paste0("M",computation_month)}
      
      out$filename <- paste0(indicator$id, "_", computation_year,if(!is.null(out$quarter)|!is.null(out$month)){"-"}else{""}, paste0(c(out$quarter, out$month), collapse=""), ".csv")
      out$filepath <- file.path(appConfig$store, "staging", indicator$id, computation_year, paste0(c(out$quarter, out$month), collapse=""), out$filename)
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
    
    return(out)
  }
  
  #observe on 'compute' button
  observeEvent(input$computeButton,{
    
    disable("computeButton")
    
      out<-computeIndicator(out=out,session=session,computation_indicator=input$computation_indicator,computation_target=input$computation_target,computation_year=input$computation_year,computation_quarter=input$computation_quarter,computation_month=input$computation_month,compute_dependent_indicators=if(!is.null(input$computation_target))if(input$computation_target=="release+staging"){TRUE}else{FALSE}else{FALSE})
      
    enable("computeButton")
    
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
      uiOutput(ns("computation_indicator_wrapper")),
      uiOutput(ns("computation_target_wrapper")),
      uiOutput(ns("computation_year_wrapper")),
      uiOutput(ns("computation_month_wrapper")),
      uiOutput(ns("computation_quarter_wrapper")),
      uiOutput(ns("computeButton_wrapper"))
    )
  })
  
  #UI OF INDICATORS LIST SELECTION
  output$computation_indicator_wrapper<-renderUI({
    req(AVAILABLE_INDICATORS)
  selectizeInput(
    ns("computation_indicator"), label = i18n("COMPUTATION_INDICATOR_LABEL"), 
    choices = setNames(sapply(AVAILABLE_INDICATORS, function(x){x$id}),sapply(AVAILABLE_INDICATORS, function(x){x$label})), selected = NULL,
    options = list(
      placeholder = i18n("COMPUTATION_INDICATOR_PLACEHOLDER_LABEL"),
      onInitialize = I('function() { this.setValue(""); }'),
       render = I('{
                 option: function(item, escape) {
                 return "<div><strong>" + escape(item.label) + "</strong>"
                 }
               }')
    )
  )
  })
  
  observeEvent(input$computation_indicator,{
    req(!is.null(input$computation_indicator)&input$computation_indicator!="")
    
    available_periods(NULL)
    selected_indicator$indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == input$computation_indicator})][[1]]
    available_periods_parts <- unlist(strsplit(selected_indicator$indicator$compute_by$available_periods[1], ":"))
    selected_indicator$period_key <- available_periods_parts[1]
    selected_indicator$period_value <- available_periods_parts[2]
    
    out$results <- getComputationResults(selected_indicator$indicator)
    out$computation <- NULL
    out$indicator <- selected_indicator$indicator
    
    if(selected_indicator$period_key=="data"){
      available_periods(eval(parse(text=paste0(selected_indicator$period_value, "(con = pool)"))))
      
    }else{
      req(!is.null(input$computation_target)&input$computation_target!="")
      available_periods(eval(parse(text=paste0("getStatPeriods(config = appConfig ,id = \"",selected_indicator$period_value,"\",target = \"",input$computation_target,"\")"))))
    }

  })
  
  observeEvent(c(input$computation_target),{
    req(!is.null(input$computation_target)&input$computation_target!="")
    req(selected_indicator$period_key=="process")
    available_periods(eval(parse(text=paste0("getStatPeriods(config = appConfig ,id = \"",selected_indicator$period_value,"\",target = \"",input$computation_target,"\")"))))
    
  })
  
  observeEvent(input$computation_indicator,{
    req(!is.null(input$computation_indicator)&input$computation_indicator!="")
    req(!is.null(selected_indicator$period_key))
    output$computation_target_wrapper <- renderUI({
      
      if(selected_indicator$period_key=="process"){
        choices=c(setNames(c("release","release+staging"),c(i18n("COMPUTATION_TARGET_RELEASE_ITEM"),i18n("COMPUTATION_TARGET_RELEASE_AND_STAGING_ITEM"))))
        fluidRow(
          column(6,
        selectizeInput(
          ns("computation_target"), label = i18n("COMPUTATION_TARGET_LABEL"), 
          choices = choices, selected = "release+staging"
        )),
        column(6,
          uiOutput(ns("info_target_message"))
        )
        )
      }else{
        NULL
      }
    })
  })
  
  observeEvent(input$computation_target,{
    req(!is.null(input$computation_target))
    output$info_target_message<-renderUI({
      tags$span(shiny::icon(c('circle-info')),ifelse(input$computation_target=="release","Only already released indicators will be use in the computation","The missing dependent indicators will be automatically computed"), style="color:blue")
    })
  })
  
  observeEvent(available_periods(),{
    req(!is.null(input$computation_year)&input$computation_year!="")
    req(!is.null(selected_indicator$indicator))

    output$computed_indicators_message<-renderUI({
      if(selected_indicator$period_key=="process"&nrow(subset(available_periods(),year==input$computation_year))>0){
        period_computed=subset(available_periods(),year==input$computation_year)
        nb_computed<-nrow(period_computed)
        if(any("month" %in% names(period_computed))){
          if(nb_computed==12){
            tags$span(shiny::icon(c('check-circle')), sprintf("All months are available in %s for this indicator",input$computation_year), style="color:green;")
          }else{
            tagList(
            tags$span(shiny::icon(c('triangle-exclamation')), sprintf("Only %s %s available in %s for this indicator",nb_computed,ifelse(nb_computed>1, "months are","month is"),input$computation_year), style="color:orange;")
            )
          }
        }else if(any("quarter" %in% names(period_computed))){
          if(nb_computed==4){
            tags$span(shiny::icon(c('check-circle')), sprintf("All months are available in %s for this indicator",input$computation_year), style="color:green;")
          }else{
            tagList(
            tags$span(shiny::icon(c('triangle-exclamation')), sprintf("Only %s %s available in %s for this indicator",nb_computed,ifelse(nb_computed>1, "quarters are","quarter is"),input$computation_year), style="color:orange;")
            )
          }
        }else{
          NULL
        }
      }else{
        NULL
      }
    })
  })
    
  observeEvent(available_periods(),{
  output$computation_year_wrapper <- renderUI({
    req(!is.null(available_periods()))
    choices=unique(available_periods()$year)
    if(!is.null(input$computation_target)){
      fluidRow(
        column(6,
               selectizeInput(
                 ns("computation_year"), label = i18n("COMPUTATION_YEAR_LABEL"), 
                 choices = choices[order(choices,decreasing = T)] , selected = NULL, 
                 options = list(placeholder = if(length(choices)>0){i18n("COMPUTATION_YEAR_PLACEHOLDER_LABEL")}else{i18n("COMPUTATION_YEAR_PLACEHOLDER_LABEL_EMPTY")},
                                onInitialize = I('function() { this.setValue(""); }')))),
        column(6,
               uiOutput(ns("computed_indicators_message"))
        )
      )
      }else{
    selectizeInput(
      ns("computation_year"), label = i18n("COMPUTATION_YEAR_LABEL"), 
      choices = choices[order(choices,decreasing = T)] , selected = NULL, 
      options = list(placeholder = if(length(choices)>0){i18n("COMPUTATION_YEAR_PLACEHOLDER_LABEL")}else{i18n("COMPUTATION_YEAR_PLACEHOLDER_LABEL_EMPTY")},
                     onInitialize = I('function() { this.setValue(""); }')))
      }
  })
  })
  
  observeEvent(c(input$computation_indicator,input$computation_year),{
    #req(!is.null(input$computation_year)&input$computation_year!="")
    output$computation_month_wrapper <- renderUI({
      if("month"%in%selected_indicator$indicator$compute_by$period){
        if(!is.null(input$computation_year))if(input$computation_year!=""){
        
        choices=unique(subset(available_periods(),year==input$computation_year)$month)
        selectizeInput(
          ns("computation_month"), label = i18n("COMPUTATION_MONTH_LABEL"), 
          choices = choices[order(choices)], selected = NULL, 
          options = list(placeholder = i18n("COMPUTATION_MONTH_PLACEHOLDER_LABEL"),
                         onInitialize = I('function() { this.setValue(""); }'))
        )
        }else{
          NULL
        }
      }else{
        NULL
      }
    })
    
    output$computation_quarter_wrapper <- renderUI({
      if("quarter"%in%selected_indicator$indicator$compute_by$period){
        if(!is.null(input$computation_year))if(input$computation_year!=""){
        choices=unique(subset(available_periods(),year==input$computation_year)$quarter)
        selectizeInput(
          ns("computation_quarter"), label = i18n("COMPUTATION_QUARTER_LABEL"), 
          choices = choices[order(choices)], selected = NULL, 
          options = list(placeholder = i18n("COMPUTATION_QUARTER_PLACEHOLDER_LABEL"),
                         onInitialize = I('function() { this.setValue(""); }'))
        )
      }else{
        NULL
      }
      }else{
        NULL
      }
    })
    
    #TODO additional renderUI/dropdownlist (mode staging/release) conditioned by the indicator definition 
    #(to display if input is the result of another indicator)
    
    
  })
  
  output$computeButton_wrapper<-renderUI({
    req(!is.null(input$computation_indicator)&input$computation_indicator!="")
    req(!is.null(input$computation_year)&input$computation_year!="")
    if("quarter"%in%selected_indicator$indicator$compute_by$period){
    req(!is.null(input$computation_quarter)&input$computation_quarter!="")
    }
    if("month"%in%selected_indicator$indicator$compute_by$period){
      req(!is.null(input$computation_month)&input$computation_month!="")
    }
    actionButton(ns("computeButton"), label = i18n("COMPUTATION_ACTIONBUTTON_LABEL"), class = "btn-primary")
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
        tagList(
        div(tags$b(i18n("CONFIRMATION_TO_CREATE_RELEASE"))),
        checkboxInput(ns("releaseDependent"), label=i18n("RELEASE_DEPENDENT_LABEL"), value = FALSE, width = NULL)
        )
        
      },
      footer = tagList(
        actionButton(session$ns("cancelRelease"),i18n("TO_CANCEL_RELEASE_LABEL")),
        actionButton(session$ns("goRelease"), i18n("TO_CREATE_RELEASE_LABEL"))
      )
    )
  }
  
  
  releaseIndicator<-function(out,session,target,release_dependent_indicators=FALSE){
    
    if(release_dependent_indicators){
      decode_target<-unlist(strsplit(target ,"/staging/"))[2]
      decode_target<-unlist(strsplit(decode_target,"/"))
      computation_indicator<-decode_target[1]
      computation_year<-decode_target[2]
      indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == computation_indicator})][[1]]
      indicators<-unlist(sapply(names(indicator$compute_with$fun_args), function(x){
        fun_arg_value <- indicator$compute_with$fun_args[[x]]
        parts <- unlist(strsplit(fun_arg_value, ":"))
        key <- ""
        value <- ""
        if(length(parts)==2){
          key <- parts[1]
          value <- parts[2]
        }
        if(key=="process")return(value)}))
      
      for(indicator in indicators){
        process_def = AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == indicator})][[1]]
        
        staging_indicators <- getStatPeriods(config = appConfig, id = indicator, target = "staging")
        #r
        indicators_to_release <- staging_indicators[staging_indicators$year == computation_year, ]$file
        
        if(length(indicators_to_release)>0){
          
          for(dependent_target in indicators_to_release){
            releaseIndicator(
              out = out,
              session = session,
              target = dependent_target,
              release_dependent_indicators = TRUE
            )
          }
        }
        
      }
      
    }
    
    file.copy(
      from = target,
      to = gsub("staging", "release", target),
      overwrite = TRUE
    )
    file.remove(target)
    
    session$userData$computation_new(Sys.time())
    if(file.exists(gsub("staging", "release", target))){
      target<-NULL
      out$results <- getComputationResults(out$indicator)
    }
    torelease(target)
    
    return(out)
    
  }
  
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$goRelease, {
    out<-releaseIndicator(out=out,session=session,target=torelease(),release_dependent_indicators=input$releaseDependent)
    removeModal()
  })
  observeEvent(input$cancelRelease,{
    torelease(NULL)
    removeModal()
  })
  
 })
}
