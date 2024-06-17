#computation2_server
computation2_server <- function(id, pool) {

 moduleServer(id, function(input, output, session){  
  
  session$userData$computation_new <- reactiveVal(NULL)
   
  ns <- session$ns

  #--------------------------------
  #REACTIVE VARIABLES
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
  full_periods<-reactiveVal(NULL)
  selected_indicator<-reactiveValues(
    indicator = NULL,
    period_key = NULL,
    period_value = NULL
  )
  
  indicator<-reactiveVal(NULL)
  indicator_status<-reactiveVal(NULL)
  indicator_first_compute<-reactiveVal(TRUE)  
  
  torelease <- reactiveVal(NULL)
  tostaging <- reactiveVal(NULL)
  
  #--------------------------------
  #DATA
  #--------------------------------
  
  AVAILABLE_INDICATORS <- getLocalCountryDataset(appConfig,"statistical_indicators.json")
  
  #--------------------------------
  #FUNCTIONS
  #--------------------------------
  
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
    }
    return(df)
  }
  
  #computeIndicator
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
  
  
  #releaseIndicator
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
  
  #UI RENDERERS
  #----------------------------------------------------------------------------------------------------
  
  #indicator selector
  
  output$indicator_wrapper<-renderUI({
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
  
  output$computation_by <- renderUI({
    tagList(
      uiOutput(ns("indicator_wrapper")),
      uiOutput(ns("description_wrapper")),
      uiOutput(ns("show_notice_wrapper")),
      uiOutput(ns("select_indicator_wrapper"))
    )
  })
  
  #Selector Block
  #---------------------------------------------------
  
  
  #Process to react to selection and notice button of each indicator (require in carousel logic) 
    
    observeEvent(input$computation_indicator,{
      req(!is.null(input$computation_indicator)&input$computation_indicator!="")
      
      x<- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == input$computation_indicator})][[1]]
      print(x)
      
      output$description_wrapper<-renderUI({
        if(!is.null(x$description)){
          p(x$description)
        }else{
          NULL
        }
           
      })
      
      output$show_notice_wrapper<-renderUI({
        if(!is.null(x$notice)){
          actionButton(ns("show_notice"),i18n("LABEL_SHOW_NOTICE"),style="margin-bottom:30px;")
        }else{
          NULL
        }
      })
      
      output$select_indicator_wrapper<-renderUI({
          actionButton(ns("select_indicator"),i18n("LABEL_SELECT_INDICATOR"),style="margin-right:10px;margin-bottom:30px;")
      })
    
    })
    
    observeEvent(input$select_indicator,{
    
    INFO("Selection of indicator : %s",input$computation_indicator)
    indicator<-indicator(input$computation_indicator)
    indicator_first_compute<-indicator_first_compute(TRUE)
    
    })
    
    observeEvent(input$show_notice,{
      
      x<-AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == input$computation_indicator})][[1]]
      
      req(!is.na(x$notice))
      showModal(
        modalDialog(
          tags$iframe(style="height:600px; width:100%", src=x$notice),
          easyClose = TRUE, footer = NULL,size="l" 
        )
      )
      
    })
  
  #Bar plot block
  #--------------------------------------------
  #Bar plot box
  output$plot_wrapper<-renderUI({
    if(is.null(indicator_status())){
      p(i18n("EMPTY_PLOT_LABEL"))
    }else{
      plotlyOutput(ns("plot"))
    }
  })
  
  #Bar plot process
  output$plot<-renderPlotly({
    
    req(!is.null(indicator_status()))
    
    df<-indicator_status()%>%
      group_by(Status,year)%>%
      count(Status)%>%
      group_by(year)%>%
      mutate(percent=n/sum(n)*100)%>%
      rowwise()%>%
      mutate(Status=switch (Status,
                            "release" = i18n("STATUS_APPROVED"),
                            "staging" = i18n("STATUS_COMPUTED"),
                            "available" = i18n("STATUS_TO_COMPUTE"),
                            "not available" = i18n("STATUS_NOT_AVAILABLE")
      ))%>%
      select(-n)%>%
      complete(nesting(year),Status=c(i18n("STATUS_APPROVED"),i18n("STATUS_COMPUTED"),i18n("STATUS_TO_COMPUTE"),i18n("STATUS_NOT_AVAILABLE")),fill=list(percent=0))%>%
      mutate(Status=factor(Status,levels=c(i18n("STATUS_APPROVED"),i18n("STATUS_COMPUTED"),i18n("STATUS_TO_COMPUTE"),i18n("STATUS_NOT_AVAILABLE"))))%>%
      ungroup()
    
    colormap <- setNames(object = c("#008000", "#32cd32", "#ffa500","gray"),
                         nm = c(i18n("STATUS_APPROVED"),i18n("STATUS_COMPUTED"),i18n("STATUS_TO_COMPUTE"),i18n("STATUS_NOT_AVAILABLE")))
    
    plot_ly(df, 
            x = ~year,
            y = df$percent,
            type = 'bar',
            name = ~Status,
            text = paste(df$percent,"%"),
            textposition = 'top',
            hoverinfo = 'text',
            hovertext = paste(df$year,
                              '<br>', df$Status,': ',paste(round(df$percent,0),"%")),
            color = ~Status, 
            colors = colormap) %>%
      layout(yaxis = list(title = NULL, zeroline = FALSE,
                          showline = FALSE, ticksuffix = "%"), 
             xaxis = list (title = "",type="category"),
             barmode = 'stack',hoverlabel = list(bgcolor= 'white'),legend = list(traceorder = "reversed"))
    
  })
  #--------------------------------------------
  
  
  #Dependent indicators management Block
  #------------------------------------------------------------------
  
  #Adjustement of available periods proposed based on computed dependent indicators 
  observeEvent(c(input$computation_target),{
    req(!is.null(input$computation_target)&input$computation_target!="")
    req(selected_indicator$period_key=="process")
    available_periods(eval(parse(text=paste0("getStatPeriods(config = appConfig ,id = \"",selected_indicator$period_value,"\",target = \"",input$computation_target,"\")"))))
    
  })
  
  #Target mode selector logic if dependent indicators
  observeEvent(indicator(),{
    req(!is.null(indicator())&indicator()!="")
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
  
  #Target mode informative message
  observeEvent(input$computation_target,{
    req(!is.null(input$computation_target))
    output$info_target_message<-renderUI({
      tags$span(shiny::icon(c('circle-info')),ifelse(input$computation_target=="release","Only already released indicators will be use in the computation","The missing dependent indicators will be automatically computed"), style="color:blue")
    })
  })
  
  
  #--------------------------
  #Events
  #--------------------------
  
  #This one is the major part of process
  observeEvent(indicator(),{
    
    req(!is.null(indicator())&indicator()!="")
    req(!is.null(indicator_first_compute()))
    req(indicator_first_compute()==TRUE)
    
    available_periods<-available_periods(NULL)
    full_periods<-full_periods(NULL)
    indicator_status<-indicator_status(NULL)
    
    selected_indicator$indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == indicator()})][[1]]
    available_periods_parts <- unlist(strsplit(selected_indicator$indicator$compute_by$available_periods[1], ":"))
    selected_indicator$period_key <- available_periods_parts[1]
    selected_indicator$period_value <- available_periods_parts[2]
    
    out$results <- getComputationResults(selected_indicator$indicator)
    out$computation <- NULL
    out$indicator <- selected_indicator$indicator
    
    
    #Get periods accessible in the data and clean it
    if(selected_indicator$period_key=="data"){
      available_periods_new<-eval(parse(text=paste0(selected_indicator$period_value, "(con = pool)")))
      
    }else{
      req(!is.null(input$computation_target)&input$computation_target!="")
      available_periods_new<-eval(parse(text=paste0("getStatPeriods(config = appConfig ,id = \"",selected_indicator$period_value,"\",target = \"",input$computation_target,"\")")))
    }
    
    available_periods_new<-subset(available_periods_new,!is.na(year))
    if("month"%in%selected_indicator$indicator$compute_by$period){
      available_periods_new<-subset(available_periods_new,!is.na(month))
      available_periods_new$period<-paste0(available_periods_new$year,"-","M",available_periods_new$month)
      available_periods_new<- available_periods_new%>%arrange(desc(year),month)
      
    }
    if("quarter"%in%selected_indicator$indicator$compute_by$period){
      available_periods_new<-subset(available_periods_new,!is.na(quarter))
      available_periods_new$period<-paste0(available_periods_new$year,"-","Q",available_periods_new$quarter)
      available_periods_new<- available_periods_new%>%arrange(desc(year),quarter)%>%as.data.frame()
    }
    if("year"%in%selected_indicator$indicator$compute_by$period){
      available_periods_new$period<-available_periods_new$year
      available_periods_new<- available_periods_new%>%arrange(desc(year))%>%as.data.frame()
    }
    
    available_periods<-available_periods(available_periods_new)
    
    req(!is.null(available_periods))
    req(!is.null(available_periods()$period))
    
    #Create full period matrix based on typo of compute_by period
    if("month"%in%selected_indicator$indicator$compute_by$period){
      years<-unique(available_periods()$year)
      full_periods_new<-data.frame(
        year=rep(years,each=12),
        month=rep(1:12,length(years))
      )
      full_periods_new$Period<-paste0(full_periods_new$year,"-","M",full_periods_new$month)
    }
    if("quarter"%in%selected_indicator$indicator$compute_by$period){
      years<-unique(available_periods()$year)
      full_periods_new<-data.frame(
        year=rep(years,each=4),
        quarter=rep(1:4,length(years))
      )
      full_periods_new$Period<-paste0(full_periods_new$year,"-","Q",full_periods_new$quarter)
    }
    if("year"%in%selected_indicator$indicator$compute_by$period){
      years<-unique(available_periods()$year)
      full_periods_new<-data.frame(
        year=years,
        Period=years
      )
    }
    
    full_periods<-full_periods(full_periods_new)
    
    #Merge info of results, available period and full period matrix
    
    indicator_status_new<-available_periods()%>%
      left_join(out$results%>%select(Period,File,Status,Date),by=c("period"="Period"))%>%
      mutate(Status=ifelse(is.na(Status),"available",Status))%>%
      rename(Period=period)
    
    if(length(setdiff(full_periods()$Period,indicator_status_new$Period))>0){
      indicator_status_new<-full_periods()%>%
        left_join(indicator_status_new)%>%
        mutate(Status=ifelse(is.na(Status),"not available",Status))
    }
    
    indicator_status<-indicator_status(indicator_status_new)
    
    
    #Generate for each period the UIelements
    lapply(1:nrow(indicator_status_new), function(x){
      item<-subset(indicator_status_new)[x,]
      period<-item$Period
      
      #Status icon of year level summary
      
      output[[paste0("icon_summary_",period)]] <-renderUI({
        
        target<-subset(indicator_status(),Period==period)
        
        switch (target$Status,
                "release" = {
                  icon<-icon("square-check", class = "fas")
                  color<-"green"
                },
                "staging" = {
                  icon<-icon("square-check")
                  color<-"limegreen"
                },
                "available" = {
                  icon<-icon("square")
                  color<-"orange"
                },
                "not available" = {
                  icon<-icon("ban")
                  color<-"gray"
                },
        )
        
        tags$span(icon,style = sprintf("color:%s;padding-right:6.6px;",color))
      })
      
      #Status icon UI
      
      output[[paste0("icon_status_",period)]] <-renderUI({
        target<-subset(indicator_status(),Period==period)
        
        switch (target$Status,
                "release" = {
                  tags$span(tags$span(icon("lock"),style = "color:gray;padding-right:15px;"),tags$span(icon("square-check", class = "fas"),style = "color:green;"),style = "padding-right:24px;margin-left:3.5px;")
                },
                "staging" = {
                  tags$span(tags$span(icon("lock-open"),style = "color:gray;padding-right:10px;"),tags$span(icon("square-check"),style = "color:limegreen;"),style = "padding-right:20px;margin-left:3.5px;")
                },
                "available" = {
                  tags$span(icon("square"),style = "color:orange;padding-right:20px;margin-left:40px")
                },
                "not available" = {
                  tags$span(icon("ban"),style = "color:gray;padding-right:20px;margin-left:40px")
                }
        )
      })
      
      #Status label UI
      
      output[[paste0("status_label_",period)]] <-renderUI({
        target<-subset(indicator_status(),Period==period)
        
        switch (target$Status,
                "release" = {
                  tags$span(tags$b(sprintf("%s : %s ",i18n("STATUS"),i18n("STATUS_APPROVED"))),tags$em(sprintf("(%s : %s)",i18n("LAST_UPDATE"),target$Date)),style = "color:green;padding-left:200px;")
                },
                "staging" = {
                  tags$span(tags$b(sprintf("%s : %s ",i18n("STATUS"),i18n("STATUS_COMPUTED"))),tags$em(sprintf("(%s : %s)",i18n("LAST_UPDATE"),target$Date)),style = "color:limegreen;padding-left:200px;")
                },
                "available" = {
                  tags$span(tags$b(sprintf("%s : %s",i18n("STATUS"),i18n("STATUS_TO_COMPUTE"))),style = "color:orange;padding-left:200px;")
                },
                "not available" = {
                  tags$span(tags$b(sprintf("%s : %s",i18n("STATUS"),i18n("STATUS_NOT_AVAILABLE"))),style = "color:gray;padding-left:200px;")
                }
        )
      })
      
      #Action button UI
      output[[paste0("actions_",period)]] <-renderUI({
        target<-subset(indicator_status(),Period==period)
        
        switch (target$Status,
                "release" = {
                  
                  return(tags$span(
                    actionButton(inputId = ns(paste0('button_view_', target$Period)), class="btn btn-light", style = "border-color:transparent;padding-right:10px", title = i18n("ACTION_VIEW"), label = "", icon = icon("eye", class = "fas")),
                    downloadButtonCustom(ns(paste0("button_download_result_", target$Period)),style = "border-color:transparent;padding-right:10px", title = i18n("ACTION_DOWNLOAD_RESULT"), label = "", icon = icon("download"),onclick = sprintf("Shiny.setInputValue('%s', this.id)", ns("select_button"))),
                    style = "position: absolute; right: 98px;margin-top: -10px;"
                  ))
                },
                "staging" = {
                  
                  return(tags$span(
                    actionButton(inputId = ns(paste0('button_compute_', target$Period)), class="btn btn-light", style = "border-color:transparent;padding-right:10px", title = i18n("ACTION_UPDATE"), label = "", icon = icon("arrows-rotate")),
                    actionButton(inputId = ns(paste0('button_view_', target$Period)), class="btn btn-light", style = "border-color:transparent;padding-right:10px", title = i18n("ACTION_VIEW"), label = "", icon = icon("eye", class = "fas")),
                    downloadButtonCustom(ns(paste0("button_download_result_", target$Period)),style = "border-color:transparent;padding-right:10px",title = i18n("ACTION_DOWNLOAD_RESULT"), label = "", icon = icon("download"),onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))),
                    actionButton(inputId = ns(paste0('button_release_', target$Period)), class="btn btn-light", style = "border-color:transparent",title = i18n("ACTION_RELEASE"), label = "", icon = icon("thumbs-up", class = "fas")),
                    style = "position: absolute; right: 50px;margin-top: -10px;"))
                },
                "available" = {
                  
                  return(tags$span(
                    actionButton(inputId = ns(paste0('button_compute_', target$Period)), class="btn btn-light", style = "border-color:transparent",title = i18n("ACTION_STAGING"), label = "", icon = icon("file-pen")),
                    style = "position: absolute; right: 50px;margin-top: -10px;"))
                },
                "not available" = {
                  return(NULL)
                }
        )
      })
      
    })
    
    output$computation_summary<-renderUI({
      div(
        box(width=12,
            title = tags$b(selected_indicator$indicator$label),
            collapsible = T,
            collapsed = F,
            lapply(unique(indicator_status_new$year), function(i){
              fluidRow(
                box(width=12,
                    collapsible = T,
                    collapsed = T,
                    title = p(
                      tags$span(tags$b(i),style="margin-left:25px"),
                      tags$span(
                        lapply(1:nrow(subset(indicator_status_new,year==i)), function(x){
                          item<-subset(indicator_status_new,year==i)[x,]
                          label<-strsplit(item$Period,"-")[[1]][2]
                          
                          return(tagList(
                            tags$span(label,style = "color:black;padding-right:2px;font-size: 17.9px;"),
                            uiOutput(ns(paste0('icon_summary_', item$Period)),inline=T)
                          ))
                        }),
                        style = "position: absolute; left: 150px")
                    ),
                    lapply(1:nrow(subset(indicator_status_new,year==i)), function(x){
                      item<-subset(indicator_status_new,year==i)[x,]
                      name<-tags$span(tags$b(item$Period))
                      
                      return(fluidRow(
                        box(width=12,
                            collapsible = F,
                            collapsed = F,
                            title = p(
                              uiOutput(ns(paste0('icon_status_', item$Period)),inline=T),
                              name,
                              uiOutput(ns(paste0('status_label_', item$Period)),inline=T),
                              uiOutput(ns(paste0('actions_', item$Period)),inline=T),
                              uiOutput(ns(paste0('table_', item$Period,"_wrapper")),inline=F)
                            )
                        )
                      ))
                    })
                )
              )
            })
        )
      )
    })
    

    #Method 3
    #Create event assocate to each action button
    lapply(1:nrow(indicator_status()), function(i){
      idx = indicator_status()[i,"Period"]
      
      output[[paste0("button_download_result_",idx)]] <<- downloadHandler(
        filename = function() {
          paste0("result", "_", out$indicator$id, "_", indicator_status()[i,"Period"],"_", toupper(indicator_status()[i,"Status"]), ".csv")
        },
        content = function(con) {
          
          INFO("Click on %s result download button",idx)
          
          data <- as.data.frame(readr::read_csv(indicator_status()[i,"File"]))
          readr::write_csv(data, con)
        }
      )
      
      observeEvent(input[[paste0("button_release_",idx)]],{
        
        INFO("Click on %s release button",idx)
        
        filename <- paste0(out$indicator$id, "_", indicator_status()[i,"Period"], ".csv")
        print(filename)
        filepath_staging <- file.path(appConfig$store, "staging", out$indicator$id, gsub("-","/",indicator_status()[i,"Period"]), filename)
        print(filepath_staging)
        filepath <- file.path(appConfig$store, "release", out$indicator$id, gsub("-","/",indicator_status()[i,"Period"]), filename)
        print(filepath)
        torelease(filepath_staging)
        print(filepath_staging)
        alreadyReleased <- file.exists(filepath)
        showModal(releaseModal(session, warning = alreadyReleased))
      },ignoreInit = T)
      
      observeEvent(input[[paste0("button_view_",idx)]],{
        
        INFO("Click on %s view button",idx)
        
        #Result table logic
        output[[paste0("table_",idx,"_wrapper")]]<-renderUI({
          if (input[[paste0("button_view_",idx)]] %% 2 != 0) {
            
            output[[paste0("table_",idx)]]<-DT::renderDT(server = FALSE, {
              DT::datatable(
                readr::read_csv(indicator_status()[i,"File"]),
                escape = FALSE,
                filter = list(position = 'top',clear =FALSE),
                options = list(
                  dom = 'Bfrtip',
                  scrollX=TRUE,
                  pageLength=5,
                  orientation ='landscape'
                )
              )
            })
            
            DTOutput(ns(paste0("table_",idx)))%>%withSpinner(type = 4)
          }else{
            NULL
          }
        })
        
        
        #Update the icon of view icon eye (next action show table) or slashed eye(next action hide table) (not work)
        if (input[[paste0("button_view_",idx)]] %% 2 != 0) {
          updateActionButton(session, ns(paste0("button_view_",idx)), "", icon = icon("eye-slash", class = "fas" ))
        } else {
          updateActionButton(session, ns(paste0("button_view_",idx)), "", icon = icon("eye", class = "fas" ))
        }
        
        
      },ignoreInit = T)
      
      observeEvent(input[[paste0("button_compute_",idx)]],{
        
        INFO("Click on %s compute or update button",idx)
        
        period<-strsplit(idx,"-")[[1]]
        computation_year<-period[1]
        computation_month<-NULL
        computation_quarter<-NULL
        
        if(startsWith(period[2],"M")){
          computation_month<-gsub("M","",period[2])
        }
        
        if(startsWith(period[2],"Q")){
          computation_quarter<-gsub("Q","",period[2])
        }
        
        computeIndicator(out=out,session=session,computation_indicator=indicator(),computation_target=input$computation_target,computation_year=computation_year,computation_quarter=computation_quarter,computation_month=computation_month,compute_dependent_indicators=if(!is.null(input$computation_target))if(input$computation_target=="release+staging"){TRUE}else{FALSE}else{FALSE})
      },ignoreInit = T)
      
      
    })
    
    #allow to just update the content of the box and not alter box structure
    indicator_first_compute<-indicator_first_compute(FALSE)
  })
  
  #This event actualise the computations status
  observeEvent(out$results,{
    req(indicator_first_compute()==FALSE)
    
    indicator_status_new<-available_periods()%>%
      left_join(out$results%>%select(Period,File,Status,Date),by=c("period"="Period"))%>%
      mutate(Status=ifelse(is.na(Status),"available",Status))%>%
      rename(Period=period)
    
    if(length(setdiff(full_periods()$Period,indicator_status_new$Period))>0){
      indicator_status_new<-full_periods()%>%
        left_join(indicator_status_new)%>%
        mutate(Status=ifelse(is.na(Status),"not available",Status))
    }
    
    indicator_status<-indicator_status(indicator_status_new)
  })
  
  
  #Manage release of the indicator
  #--------------------------------------------------------
  
  #This one release the indicator and update the result
  observeEvent(input$goRelease, {
    out<-releaseIndicator(out=out,session=session,target=torelease(),release_dependent_indicators=input$releaseDependent)
    removeModal()
  })
  
  #This one cancel the release request and remove the modal
  observeEvent(input$cancelRelease,{
    torelease(NULL)
    removeModal()
  })
  
 })
}
