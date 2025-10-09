#computation_server
computation_server <- function(id, parent.session, pool, reloader) {

 moduleServer(id, function(input, output, session){  

  INFO("computation: START")
  MODULE_START_TIME <- Sys.time()    
  
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
    filepath_release = NULL,
    report = NULL
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
  toarchive <- reactiveVal(NULL)
  torecompute <- reactiveVal(NULL)
  
  #--------------------------------
  #DATA
  #--------------------------------
  
  AVAILABLE_INDICATORS <- getLocalCountryDataset(appConfig,"statistical_indicators.json")
  
  #--------------------------------
  #FUNCTIONS
  #--------------------------------
  
  #computeIndicator
  computeIndicator <- function(con,
    out, session, computation_indicator, computation_target,
    computation_year, computation_quarter = NULL, computation_month = NULL,
    compute_dependent_indicators = FALSE,
    archive_previous_release = NULL, archive_reason = ""){
    
    INFO(paste0(i18n("RETRIEVE_INDICATOR_FOR_LABEL")," '%s'\n"), computation_indicator)
    indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == computation_indicator})][[1]]
    print(indicator)
    
    #compute dependent indicators?
    if(compute_dependent_indicators){
      target_period <-indicator$compute_by$period
      
      #list dependent indicators
      dep_indicators <- unlist(sapply(names(indicator$compute_with$fun_args), function(x){
        fun_arg_value <- indicator$compute_with$fun_args[[x]]$source
        parts <- unlist(strsplit(fun_arg_value, ":"))
        key <- ""
        value <- ""
        if(length(parts)==2){
          key <- parts[1]
          value <- parts[2]
        }
        if(key=="process")return(value)}))
      
      if(length(dep_indicators)>0) for(dep_indicator in dep_indicators){
        
        process_def = AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == dep_indicator})][[1]]
        
        process_period<-process_def$compute_by$period
        
        if(target_period=="year"&process_period=="year")period_to_compute<-data.frame(year=computation_year,month=NA,quarter=NA)
        if(target_period=="year"&process_period=="quarter")period_to_compute<-data.frame(year=rep(computation_year,4),month=NA,quarter=c(1:4))
        if(target_period=="year"&process_period=="month")period_to_compute<-data.frame(year=rep(computation_year,12),month=c(1:12),quarter=NA)
        if(target_period=="quarter"&process_period=="year")period_to_compute<-data.frame(year=computation_year,month=NA,quarter=NA)
        if(target_period=="quarter"&process_period=="quarter")period_to_compute<-data.frame(year=computation_year,month=NA,quarter=computation_quarter)
        if(target_period=="quarter"&process_period=="month")period_to_compute<-data.frame(year=rep(computation_year,3),month=switch(as.character(computation_quarter),
                                                                                                                   "1"=c(1:3),
                                                                                                                   "2"=c(4:6),
                                                                                                                   "3"=c(7,9),
                                                                                                                   "4"=c(10,12)),quarter=NA)
        if(target_period=="month"&process_period=="year")period_to_compute<-data.frame(year=computation_year,month=NA,quarter=NA)
        if(target_period=="month"&process_period=="quarter")period_to_compute<-data.frame(year=computation_year,month=NA,quarter=quarter(computation_month))
        if(target_period=="month"&process_period=="month")period_to_compute<-data.frame(year=computation_year,month=computation_month,quarter=NA)
        
        period_to_compute<-period_to_compute%>%mutate(year=as.numeric(year),month=as.numeric(month),quarter=as.numeric(quarter))%>%inner_join(getAvailablePeriods(id= dep_indicator,config = appConfig, indicators = AVAILABLE_INDICATORS))
        
        release_periods<-getStatPeriods(config = appConfig, id = dep_indicator, target = "release")
        
        if(process_period == "month") release_periods$month=as.numeric(gsub("M","",release_periods$month))
        if(process_period == "quarter") release_periods$quarter=as.numeric(gsub("Q","",release_periods$quarter))
        
        period_to_compute<-period_to_compute%>%mutate(year=as.numeric(year),month=as.numeric(month),quarter=as.numeric(quarter))%>%anti_join(release_periods%>%mutate(year=as.numeric(year)))

        if(nrow(period_to_compute)>0){
          
          for(i in 1:nrow(period_to_compute)){
            
            period<-period_to_compute[i,]
            
            computeIndicator(
              con = con,
              out = out,
              session = session,
              computation_indicator = dep_indicator,
              computation_target = computation_target,
              computation_year = computation_year,
              computation_quarter = if(process_period == "quarter") period$quarter else NULL ,
              computation_month = if(process_period == "month") period$month else NULL,
              compute_dependent_indicators = TRUE,
              archive_previous_release = archive_previous_release,
              archive_reason = archive_reason
            )
          }
        }
        
      }
      
    }
    
    progress <- shiny::Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())
    
    #in case the indicator was previously released
    if(!is.null(archive_previous_release)){
      INFO("Indicator '%s' is going to be recomputed, with release archival", indicator$id)
      INFO("File '%s' is going to be archived", archive_previous_release)
      archive_dir = file.path(dirname(archive_previous_release), "archive")
      if(!dir.exists(archive_dir)){
        INFO("Archive subfolder is going to be created at '%s'", archive_dir)
        dir.create(archive_dir)
      }
      #copy release
      time = Sys.time()
      timestr = format(time, "%Y%m%dT%H%M%S")
      archived_file = file.path(archive_dir, paste0(unlist(strsplit(basename(archive_previous_release),"\\.csv")), "_", timestr, ".csv"))
      file.copy(
        from = archive_previous_release,
        to = archived_file
      )
      file.remove(archive_previous_release)
      INFO("File '%s' successfuly archived as '%s'", archive_previous_release, archived_file)
      INFO("Writing archive history file")
      readr::write_csv(
        data.frame(
          date = time,
          archive = archived_file,
          reason = archive_reason
        ), 
        file.path(archive_dir, "archive.csv"), 
        append = file.exists(file.path(archive_dir, "archive.csv"))
      )
      INFO("Successfuly file archiving!")
    }
    
    #computation
    out$computing <- TRUE
    raw_output <- NULL
    out$computation <- NULL
    indicator_msg <- sprintf(paste0(i18n("COMPUTATION_ACTIONBUTTON_LABEL")," %s - %s"), 
                             indicator$label, paste0(computation_year,if(!is.null(computation_quarter)|!is.null(computation_month)){"-"}else{""},paste0(c(computation_quarter,computation_month),collapse="")))
    INFO(indicator_msg)
    progress$set(message = indicator_msg, detail = i18n("COMPUTATION_PROGRESS_SUB_LABEL"), value = 0)
    INFO(paste0(i18n("LOAD_R_COMPUTE_SCRIPT_LABEL"),"'%s'\n"), indicator$compute_with$script)
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
                                         fun_arg_value <- indicator$compute_with$fun_args[[x]]$source
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
                                                                "text" = paste0("\"",value,"\""),
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
      
      readr::write_csv(indicator_output, out$filepath)
      
      progress$set(message = indicator_msg, detail = i18n("COMPUTATION_SUCCESSFUL_LABEL"), value = 100)
      out$results <- getComputationResults(indicator, config = appConfig)
      
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
  
  #recomputeModal
  recomputeModal <- function(session){
    modalDialog(
      tagList(
        div(tags$b(i18n("CONFIRMATION_TO_RECOMPUTE"))),
        textInput(ns("recomputeReason"), label=NULL, placeholder = i18n("RECOMPUTE_REASON"), width = NULL)
      ),
      footer = tagList(
        actionButton(session$ns("cancelRecompute"),i18n("TO_CANCEL_RECOMPUTE_LABEL")),
        actionButton(session$ns("goRecompute"), i18n("TO_CREATE_RECOMPUTE_LABEL"))
      )
    )
  }
  
  #releaseIndicator
  releaseIndicator<-function(out,session,target,release_dependent_indicators=FALSE){
    
    req(!is.null(torelease()))
    
    if(release_dependent_indicators){
      decode_target<-unlist(strsplit(target ,"/staging/"))[2]
      decode_target<-unlist(strsplit(decode_target,"/"))
      computation_indicator<-decode_target[1]
      computation_year<-decode_target[2]
      indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == computation_indicator})][[1]]
      indicators<-unlist(sapply(names(indicator$compute_with$fun_args), function(x){
        fun_arg_value <- indicator$compute_with$fun_args[[x]]$source
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

        indicators_to_release <- staging_indicators[staging_indicators$year == computation_year, ]$file
        
        if(length(indicators_to_release)>0){
          
          for(dependent_target in indicators_to_release){
            print(dependent_target)
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

    if(file.exists(gsub("staging", "release", target))){
      out$results <- getComputationResults(out$indicator, config = appConfig)
      torelease(NULL) #reinitialize reactive
    }
    
    return(out)
    
  }
  
  #getIndicatorInfo (not work -> extension of getIndicatorHierarchy)
  #complete hierarchy can't be provide due to problem when multiple root
  getIndicatorInfo<-function(id,target=F,indicators=AVAILABLE_INDICATORS,getParent=T,getChild=T){
  #   
  #   indicator<-indicators[sapply(indicators, function(x){x$id == id})][[1]]
  #   print(indicator$id)
  #   
  #   label<-indicator$id
  #   if(!is.null(indicator$label)) label<-indicator$label
  #   label<-paste0(label,"\n","[computed by :",indicator$compute_by$period,"]\n","(PROCESS)")
  #   
  #   result<-data.frame("target"=target,"type"="process","id"=indicator$id,"label"=label)
  #   
  #   if(getParent){
  #     
  #     parent<-depends<-do.call("rbind",lapply(names(indicator$compute_with$fun_args), function(x){
  #       fun_arg_value <- indicator$compute_with$fun_args[[x]]$source
  #       fun_arg_info <- indicator$compute_with$fun_args[[x]]$info
  #       parts <- unlist(strsplit(fun_arg_value, ":"))
  #       key <- ""
  #       value <- ""
  #       if(length(parts)==2){
  #         key <- parts[1]
  #         value <- parts[2]
  #       }
  #       label<-if(is.null(fun_arg_info)){value}else{fun_arg_info}
  #       label<-paste0(label,"\n","(",toupper(key),")")
  #       return(data.frame("target"=F,"type"=key,"id"=value,"label"=label))
  #     }))
  #     
  #     if(length(parent)>0){
  #       
  #       parent_process<-subset(parent,type=="process")
  #       parent_other<-subset(parent,type!="process")
  #       
  #       if(length(parent_other)>0){
  #         parent_result<-parent_other
  #       }
  #       
  #       if(length(parent_process)>0){
  #         parent_all<-do.call("rbind",lapply(parent_process$id, function(x){
  #           getIndicatorInfo(id=x,getParent = T,getChild = F)
  #         }))
  #         parent_result<-rbind(parent_all,parent_result)
  #       }
  #       
  #     }
  #     
  #     result<-rbind(parent_result,result)
  #     
  #   }
  #   
  #   print("HERE")
  #   
  #   child<-unlist(sapply(AVAILABLE_INDICATORS, function(x){
  #     sapply(names(x$compute_with$fun_args), function(y){
  #       fun_arg_value <- x$compute_with$fun_args[[y]]$source
  #       parts <- unlist(strsplit(fun_arg_value, ":"))
  #       key <- ""
  #       value <- ""
  #       if(length(parts)==2){
  #         key <- parts[1]
  #         value <- parts[2]
  #       }
  #       
  #       if(value==id)return(x$id)
  #     })
  #   }))
  #   
  #   if(length(child)>0 & getChild){
  #     
  #     child<-do.call("rbind",lapply(child, function(x){
  #       target<-AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(y){y$id == x})][[1]]
  #       label<-target$id
  #       if(!is.null(target$label)) label<-target$label
  #       label<-paste0(label,"\n","[computed by :",target$compute_by$period,"]\n","(PROCESS)")
  #       return(data.frame("target"=F,"type"="process","id"=target$id,"label"=label))
  #     }))
  #     
  #     if(length(child)>0){
  #       
  #       print("HAS CHILD")
  #       child_process<-subset(child,type=="process")
  #       child_other<-subset(child,type!="process")
  #       
  #       if(length(child_other)>0){
  #         child_result<-child_other
  #       }
  #       
  #       if(length(child_process)>0){
  #         child_all<-do.call("rbind",lapply(child_process$id, function(x){
  #           getIndicatorInfo(id=x,getParent = F,getChild = T)
  #         }))
  #         child_result<-rbind(child_result,child_all)
  #       }
  #     }
  #     result<-rbind(result,child_result)
  #   }
  #   return(result)
   }
  
  #getIndicatorHierarchy
  getIndicatorHierarchy<-function(id,target=F,hierarchyTree=NULL,indicators=AVAILABLE_INDICATORS){
    
    indicator<-indicators[sapply(indicators, function(x){x$id == id})][[1]]
    
    label<-indicator$id
    if(!is.null(indicator$label)) label<-indicator$label
    label<-paste0(label,"\n","[computed by :",indicator$compute_by$period,"]\n","(PROCESS)")
    
    result<-data.frame("target"=target,"type"="process","id"=indicator$id,"label"=label)
    
    tmpTree<-Node$new(label,id=result$id,type=result$type,target=result$target)
    
    parent<-depends<-do.call("rbind",lapply(names(indicator$compute_with$fun_args), function(x){
      fun_arg_value <- indicator$compute_with$fun_args[[x]]$source
      fun_arg_info <- indicator$compute_with$fun_args[[x]]$info
      parts <- unlist(strsplit(fun_arg_value, ":"))
      key <- ""
      value <- ""
      if(length(parts)==2){
        key <- parts[1]
        value <- parts[2]
      }
      label<-if(is.null(fun_arg_info)){value}else{fun_arg_info}
      label<-paste0(label,"\n","(",toupper(key),")")
      
      return(data.frame("target"=F,"type"=key,"id"=value,"label"=label))
    }))
    
    if(length(parent)>0){
      parent_process<-subset(parent,type=="process")
      parent_other<-subset(parent,type!="process")
      
      if(nrow(parent_other)>0){
        parent_result<-parent_other
        lapply(parent_other$label, function(x){
          subTree<-Node$new(x,id=parent_other$id,type=parent_other$type,target=parent_other$target)
          tmpTree$AddChildNode(subTree)
        })
      }
      
      if(nrow(parent_process)>0){
        lapply(parent_process$id, function(x){
          tmpTree<-getIndicatorHierarchy(id=x,target=F,hierarchyTree=tmpTree)
        })
      }
    }
    
    if(!is.null(hierarchyTree)){
      hierarchyTree$AddChildNode(tmpTree)
    }else{
      hierarchyTree<-tmpTree
    }
    
    return(hierarchyTree)
  }
  
  
  
  #UI RENDERERS
  #----------------------------------------------------------------------------------------------------
  
  #Selector Block
  #---------------------------------------------------
  output$computation_by <- renderUI({
    tagList(
      uiOutput(ns("indicator_wrapper")),
      uiOutput(ns("description_wrapper")),
      uiOutput(ns("show_notice_wrapper")),
      uiOutput(ns("show_hierarchy_wrapper")),
      uiOutput(ns("select_indicator_wrapper"))
    )
  })
  
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
  
  #indicator additional info
  observeEvent(input$computation_indicator,{
      req(!is.null(input$computation_indicator)&input$computation_indicator!="")
      
      x<- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == input$computation_indicator})][[1]]

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
      
      output$show_hierarchy_wrapper<-renderUI({
          actionButton(ns("show_hierarchy"),i18n("LABEL_SHOW_HIERARCHY"),style="margin-bottom:30px;")
      })
      
      output$select_indicator_wrapper<-renderUI({
          actionButton(ns("select_indicator"),i18n("LABEL_SELECT_INDICATOR"),style="margin-right:10px;margin-bottom:30px;")
      })
      
        
        
      #   output$computation_target_wrapper <- renderUI({
      #     
      #     available_periods_parts <- unlist(strsplit(x$compute_by$available_periods[1], ":"))
      #     period_key <- available_periods_parts[1]
      #     
      #     if(period_key=="process"){
      #       choices=c(setNames(c("release","release+staging"),c(i18n("COMPUTATION_TARGET_RELEASE_ITEM"),i18n("COMPUTATION_TARGET_RELEASE_AND_STAGING_ITEM"))))
      #       fluidRow(
      #         column(6,
      #                selectizeInput(
      #                  ns("computation_target"), label = i18n("COMPUTATION_TARGET_LABEL"), 
      #                  choices = choices, selected = "release+staging"
      #                )),
      #         column(6,
      #                uiOutput(ns("info_target_message"))
      #         )
      #       )
      #     }else{
      #       NULL
      #     }
      # })
      
      #Target mode informative message
      # observeEvent(input$computation_target,{
      #   req(!is.null(input$computation_target))
      #   output$info_target_message<-renderUI({
      #     tags$span(shiny::icon(c('circle-info')),ifelse(input$computation_target=="release","Only already released indicators will be use in the computation","The missing dependent indicators will be automatically computed"), style="color:blue")
      #   })
      # })
    
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
  
  #--------------------------
  #Events
  #--------------------------
  
  observeEvent(input$select_indicator,{
    
    INFO("Selection of indicator : %s",input$computation_indicator)
    indicator_status<-indicator_status(NULL)
    available_periods<-available_periods(NULL)
    full_periods<-full_periods(NULL)
    indicator<-indicator(input$computation_indicator)
    indicator_first_compute<-indicator_first_compute(TRUE)
    
  })
  
  observeEvent(input$show_notice,{
    
    INFO("Click on show notice button")
    
    x<-AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == input$computation_indicator})][[1]]
    
    req(!is.na(x$notice))
    showModal(
      modalDialog(
        tags$iframe(style="height:600px; width:100%", src=x$notice),
        easyClose = TRUE, footer = NULL,size="l" 
      )
    )
    
  })
  
  observeEvent(input$show_hierarchy,{
    
    INFO("Click on show hierarchy button")
    
    indicator<-AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == input$computation_indicator})][[1]]
    
    tree<-getIndicatorHierarchy(id=input$computation_indicator,target=T)
    
    SetGraphStyle(tree, rankdir = "BT")
    
    SetEdgeStyle(tree, arrowhead = "vee", color = "grey35", penwidth = 2,dir="back")
    
    
    #patch for R 4.3 (issue of double || operator)
    Traverse = function(node, 
                        traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"), 
                        pruneFun = NULL,
                        filterFun = NULL) {
      #traverses in various orders. See http://en.wikipedia.org/wiki/Tree_traversal
      
      nodes <- list()
      
      if(length(traversal) > 1L) {
        traversal <- traversal[1L]
      }
      if(is.function(traversal) | traversal == "pre-order" | traversal == "post-order") {
        
        if (length(pruneFun) == 0 || pruneFun(node)) {
          
          if (is.function(traversal)) {
            children <- traversal(node)
            if (is(children, "Node")) children <- list(children)
            if (is.null(children)) children <- list()
          } else children <- node$children
          
          for(child in children) {
            nodes <- c(nodes, Traverse(child, traversal = traversal, pruneFun = pruneFun, filterFun = filterFun))
          }
          if(length(filterFun) == 0 || any(filterFun(node))) {
            if(is.function(traversal) || traversal == "pre-order") nodes <- c(node, nodes)
            else nodes <- c(nodes, node)
          }
        }
        
      } else if(traversal == "in-order") {
        if(!node$isBinary) stop("traversal in-order valid only for binary trees")
        if(length(pruneFun) == 0 | pruneFun(node)) {
          if(!node$isLeaf) {
            n1 <- Traverse(node$children[[1]], traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
            if(length(filterFun) == 0 | filterFun(node)) n2 <- node
            else n2 <- list()
            n3 <- Traverse(node$children[[2]], traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
            nodes <- c(n1, n2, n3)
          } else {
            if(length(filterFun) == 0 | filterFun(node)) n2 <- node
            else n2 <- list()
            nodes <- c(nodes, n2)
          }
        }
        
      } else if (traversal == "ancestor") {
        
        
        if (!isRoot(node)) {
          nodes <- Traverse(node$parent, traversal = traversal, pruneFun = pruneFun, filterFun = filterFun)
        }
        
        if(length(filterFun) == 0 || any(filterFun(node))) {
          nodes <- c(node, nodes)
        }
        
      } else if (traversal == "level") {
        
        nodes <- Traverse(node, filterFun = filterFun, pruneFun = pruneFun)
        if (length(nodes) > 0) nodes <- nodes[order(Get(nodes, function(x) x$level))]
        
        
      } else {
        stop("traversal must be pre-order, post-order, in-order, ancestor, or level")
      }
      return (nodes)
    }

    target <- Traverse(tree, filterFun = function(x){ x$level == 1 & x$type=="process" })
    process <- Traverse(tree, filterFun = function(x){ x$level > 1 & x$type=="process"})
    data <- Traverse(tree, filterFun = function(x) x$type =="data")
    local <- Traverse(tree, filterFun = function(x) x$type =="local")
    
    Do(target,SetNodeStyle,style = "filled,rounded", shape = "box", fontcolor="black",fillcolor = "#90dbf4", fontname = "helvetica",penwidth="4px")
    Do(process,SetNodeStyle,style = "filled,rounded", shape = "box", fontcolor="black",fillcolor = "#8eecf5", fontname = "helvetica",penwidth="2px")
    
    if(length(data)>0)Do(data,SetNodeStyle,style = "filled", shape = "ellipse", fontcolor="black",fillcolor = "#b9fbc0", fontname = "helvetica",penwidth="2px")
    if(length(local)>0)Do(local,SetNodeStyle,style = "filled", shape = "box", fontcolor="black",fillcolor = "#fde4cf", fontname = "helvetica",penwidth="2px")
    
    p<-plot(tree)
    
    output$tree_plot<-renderGrViz({
      p
    })
    
    showModal(
      modalDialog(
        grVizOutput(ns("tree_plot")),
        easyClose = TRUE, footer = NULL,size="l" 
      )
    )
    
  })
  
  #This event is the major part of process
  observeEvent(indicator(),{
    
    req(!is.null(indicator()) & indicator()!="")
    req(!is.null(indicator_first_compute()))
    req(indicator_first_compute() == TRUE)
    
    INFO("Selecting indicator '%s'", indicator())
    
    available_periods <- available_periods(NULL)
    full_periods <- full_periods(NULL)
    indicator_status <- indicator_status(NULL)
    
    selected_indicator$indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == indicator()})][[1]]
    
    out$results <- getComputationResults(selected_indicator$indicator, config = appConfig)
    out$computation <- NULL
    out$indicator <- selected_indicator$indicator
    out$report <- NULL
    
    #get available periods for the selected indicator
    available_periods_new <- getAvailablePeriods(
      id = selected_indicator$indicator$id,
      config = appConfig,
      indicators = AVAILABLE_INDICATORS
    )
    
    #format available periods
    available_periods_new <- formatAvailablePeriods(available_periods_new, selected_indicator$indicator)
    #store it as reactive
    available_periods <- available_periods(available_periods_new)
    
    req(!is.null(available_periods))
    req(!is.null(available_periods()$period))
    
    #full periods
    #Create full period matrix based on typo of compute_by period
    full_periods_new <- getFullPeriods(available_periods_new, selected_indicator$indicator)
    #store it as reactive
    full_periods <- full_periods(full_periods_new %>% arrange(desc(year)))

    #Merge info of results, available period and full period matrix
    DEBUG("Available periods:")
    if(appConfig$debug) print(head(available_periods()))
    DEBUG("Computation results:")
    if(appConfig$debug) print(head(out$results))
    
    #over available periods, list those for which computation has been run
    #either at staging/release status
    indicator_status_new <- available_periods() %>%
      mutate(period = as.character(period)) %>%
      left_join(out$results, by = c("period" = "Period")) %>%
      mutate(Status = ifelse(is.na(Status),"available",Status)) %>%
      rename(Period = period)
    
    
    #if full period is longer that available periods
    #list all periods including those available (with computation or not) - see above
    #extended with those with no available data.
    if(length(setdiff(full_periods()$Period, indicator_status_new$Period))>0){
      DEBUG("Full periods:")
      if(appConfig$debug) print(head(full_periods()))
      DEBUG("Computation matrix:")
      if(appConfig$debug) print(head(indicator_status_new))
      
      indicator_status_new <- full_periods() %>%
        mutate(year = as.character(year)) %>%
        mutate(Period = as.character(Period)) %>%
        left_join(indicator_status_new %>%
                    mutate(year = as.character(year)) %>%
                    mutate(Period = as.character(Period))) %>%
        mutate(Status=ifelse(is.na(Status), "not available", Status))
    }
  
    #store in reactive
    indicator_status <- indicator_status(indicator_status_new)
    
    #Generate for each period a unique element base ID, based on a random UUID
    #Required to ensure uniqueness of DOM element Ids, and avoid any trigger of
    #phantom JS events (events that are not destroyed together with the removal/update
    #of a DOM element).
    target_ids <- sapply(1:nrow(indicator_status()), function(i){
      item <- subset(indicator_status())[i,]
      period <- item$Period
      paste(period, uuid::UUIDgenerate(), sep = "_")
    })
    
    #Generate for each period the UI elements
    lapply(1:nrow(indicator_status()), function(i){
      item <- subset(indicator_status())[i,]
      period <- item$Period
      target_id = target_ids[i]
      
      #Status icon of year level summary
      output[[paste0("icon_summary_",period)]] <- renderUI({
        req("Period" %in% names(indicator_status()))
        target <- subset(indicator_status(),Period==period)
        req(nrow(target)>0)
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
      output[[paste0("icon_status_",period)]] <- renderUI({
        target <- indicator_status()[indicator_status()$Period == period,]
        req(nrow(target)>0)
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
      output[[paste0("status_label_", period)]] <- renderUI({
        target <- indicator_status()[indicator_status()$Period == period,]
        req(nrow(target)>0)
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
      output[[paste0("actions_",period)]] <- renderUI({
        print(indicator_status())
        target <- indicator_status()[indicator_status()$Period == period,]
        req(nrow(target)>0)
        switch (target$Status,
                "release" = {
                  #indicator has been computed at least once and released
                  #we can recompute the results, this will stale the released data
                  #(this action requires a reason to provide to stale the released data)
                  #we can view and download the results
                  return(tags$span(
                    actionButton(inputId = ns(paste0('button_recompute_', target_id)), class="btn btn-light", style = "border-color:transparent;padding-right:10px", title = i18n("ACTION_UPDATE"), label = "", icon = icon("arrows-rotate")),
                    actionButton(inputId = ns(paste0('button_view_', target_id)), class="btn btn-light", style = "border-color:transparent;padding-right:10px", title = i18n("ACTION_VIEW"), label = "", icon = icon("eye", class = "fas")),
                    downloadButtonCustom(ns(paste0("button_download_result_", target_id)),style = "border-color:transparent;padding-right:10px", title = i18n("ACTION_DOWNLOAD_RESULT"), label = "", icon = icon("download"),onclick = sprintf("Shiny.setInputValue('%s', this.id)", ns(paste0("button_download_result_", target_id)))),
                    disabled(actionButton(inputId = ns(paste0('button_release_', target_id)), class="btn btn-light", style = "border-color:transparent",title = i18n("ACTION_RELEASE_DISABLED_YETRELEASED"), label = "", icon = icon("thumbs-up", class = "fas"))),
                    if(length(out$indicator$reports)>0){
                      tagList(
                        tags$div(
                          selectizeInput(
                            ns(paste0('select_report_', target_id)),
                            label = NULL,
                            choices = setNames(sapply(out$indicator$reports, function(x){x$id}),sapply(out$indicator$reports, function(x){x$label})), selected = "",
                            options = list(
                              placeholder = i18n("ACTION_GENERATE_AND_DOWNLOAD_REPORT_SELECTOR"),
                              render = I('{
                                option: function(item, escape) {
                                  return "<div><strong>" + escape(item.label) + "</strong>"
                                }
                              }')
                            ),
                            width = "150px"
                          ),
                          style = "display:inline-block;margin-left:20px;"
                        ),
                        if(!is.null(out$report)){
                          downloadButtonCustom(ns(paste0("button_generate_and_download_report_", target_id)),style = "border-color:transparent;padding-right:10px;float:right;", title = i18n("ACTION_GENERATE_AND_DOWNLOAD_REPORT"), label = "", icon = icon("file-export", class = "fas"),onclick = sprintf("Shiny.setInputValue('%s', this.id)", ns(paste0("button_generate_and_download_report_", target_id))))
                        }else{
                          disabled(downloadButtonCustom(ns(paste0("button_generate_and_download_report_", target_id)),style = "border-color:transparent;padding-right:10px;float:right;", title = i18n("ACTION_GENERATE_AND_DOWNLOAD_REPORT"), label = "", icon = icon("file-export", class = "fas"),onclick = sprintf("Shiny.setInputValue('%s', this.id)", ns(paste0("button_generate_and_download_report_", target_id)))))
                        }
                      )
                    },
                    style = if(length(out$indicator$reports)>0){
                      "position:absolute; right:25px; margin-top: -10px;"
                    }else{
                      "position:absolute; right:241px; margin-top: -10px;"
                    }
                  ))
                },
                "staging" = {
                  #indicator has been computed at least once
                  #we can recompute the indicator as many times as we want
                  #we can view and download the results
                  #if indicator is releasable (depending on data availability), we can release
                  releasable <- isReleasable(
                    id = indicator(), 
                    target_period = period, 
                    config = appConfig, 
                    indicators = AVAILABLE_INDICATORS
                  )
                  
                  return(tags$span(
                    actionButton(inputId = ns(paste0('button_compute_', target_id)), class="btn btn-light", style = "border-color:transparent;padding-right:10px", title = i18n("ACTION_UPDATE"), label = "", icon = icon("arrows-rotate")),
                    actionButton(inputId = ns(paste0('button_view_', target_id)), class="btn btn-light", style = "border-color:transparent;padding-right:10px", title = i18n("ACTION_VIEW"), label = "", icon = icon("eye", class = "fas")),
                    downloadButtonCustom(ns(paste0("button_download_result_", target_id)),style = "border-color:transparent;padding-right:10px",title = i18n("ACTION_DOWNLOAD_RESULT"), label = "", icon = icon("download"),onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))),
                    if(releasable){
                      actionButton(inputId = ns(paste0('button_release_', target_id)), class="btn btn-light", style = "border-color:transparent",title = i18n("ACTION_RELEASE"), label = "", icon = icon("thumbs-up", class = "fas"))
                    }else{
                      disabled(actionButton(inputId = ns(paste0('button_release_', target_id)), class="btn btn-light", style = "border-color:transparent",title = i18n("ACTION_RELEASE_DISABLED_NOTRELEASABLE"), label = "", icon = icon("thumbs-up", class = "fas")))
                    },
                    if(length(out$indicator$reports)>0){
                      tagList(
                        tags$div(
                          selectizeInput(
                            ns(paste0('select_report_', target_id)),
                            label = NULL,
                            choices = setNames(sapply(out$indicator$reports, function(x){x$id}),sapply(out$indicator$reports, function(x){x$label})), selected = "",
                            options = list(
                              placeholder = i18n("ACTION_GENERATE_AND_DOWNLOAD_REPORT_SELECTOR"),
                              render = I('{
                                  option: function(item, escape) {
                                    return "<div><strong>" + escape(item.label) + "</strong>"
                                  }
                                }')
                            ),
                            width = "150px"
                          ),
                          style = "display:inline-block;margin-left:20px;"
                        ),
                        if(!is.null(out$report)){
                          downloadButtonCustom(ns(paste0("button_generate_and_download_report_", target_id)),style = "border-color:transparent;padding-right:10px;float:right;", title = i18n("ACTION_GENERATE_AND_DOWNLOAD_REPORT"), label = "", icon = icon("file-export", class = "fas"),onclick = sprintf("Shiny.setInputValue('%s', this.id)", ns(paste0("button_generate_and_download_report_", target_id))))
                        }else{
                          disabled(downloadButtonCustom(ns(paste0("button_generate_and_download_report_", target_id)),style = "border-color:transparent;padding-right:10px;float:right;", title = i18n("ACTION_GENERATE_AND_DOWNLOAD_REPORT"), label = "", icon = icon("file-export", class = "fas"),onclick = sprintf("Shiny.setInputValue('%s', this.id)", ns(paste0("button_generate_and_download_report_", target_id)))))
                        }
                      )
                    },
                    style = if(length(out$indicator$reports)>0){
                      "position:absolute; right:25px; margin-top: -10px;"
                    }else{
                      "position:absolute; right:241px; margin-top: -10px;"
                    }
                  ))
                },
                "available" = {
                  #indicator is available for computation
                  return(tags$span(
                    actionButton(inputId = ns(paste0('button_compute_', target_id)), class="btn btn-light", style = "border-color:transparent",title = i18n("ACTION_STAGING"), label = "", icon = icon("file-pen")),
                    style = "position:absolute; right:50px; margin-top:-10px;"))
                },
                "not available" = {
                  #indicator is not available for computation
                  return(NULL)
                }
        )
      })
      
    })
    
    output$computation_summary<-renderUI({
      div(
        box(width=12,
            title = tags$b(selected_indicator$indicator$label),
            collapsible = FALSE,
            maximizable = TRUE,
            lapply(unique(indicator_status_new$year), function(i){
              fluidRow(
                bs4Dash::box(width=12,
                    collapsible = T,
                    collapsed = T,
                    title = p(
                      tags$span(tags$b(i),style="margin-left:25px"),
                      tags$span(
                        lapply(1:nrow(subset(indicator_status_new,year==i)), function(x){
                          item<-subset(indicator_status_new,year==i)[x,]
                          label<-strsplit(item$Period,"-")[[1]]
                          if(length(label)==2){
                            label<-label[2]
                          }else{
                            label<-""
                          }
                          
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
                        bs4Dash::box(width=12,
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
    
    #Create events associated to each action button
    lapply(1:nrow(indicator_status()), function(i){
      item <- indicator_status()[i,]
      period <- item$Period
      target_id = target_ids[i]
      
      #event on results download
      output[[paste0("button_download_result_",target_id)]] <<- downloadHandler(
        filename = function() {
          paste0("result", "_", out$indicator$id, "_", indicator_status()[i,"Period"],"_", toupper(indicator_status()[i,"Status"]), ".csv")
        },
        content = function(con) {
          
          INFO("Click on %s result download button",target_id)
          
          data <- as.data.frame(readr::read_csv(indicator_status()[i,"File"]))
          readr::write_csv(data, con)
        }
      )
      
      #event on report selection
      observeEvent(input[[paste0("select_report_", target_id)]],{
        out$report = input[[paste0("select_report_", target_id)]]
      },ignoreInit = T)
      
      #event on results reporting generation/download
      output[[paste0("button_generate_and_download_report_", target_id)]] <<- downloadHandler(
       filename = function() {
         #assumes reports are in general Microsoft Excel spreadsheets.
         paste0("report", "_", out$report, "_", indicator_status()[i,"Period"], "_", toupper(indicator_status()[i,"Status"]), ".xlsx")
       },
       content = function(file) {
         
         INFO("Click on %s report generation/download button", target_id)
         report_def = out$indicator$reports[sapply(out$indicator$reports, function(x){x$id == out$report})][[1]]
         #source the reporting script
         source(report_def$script)
         #read input file
         indicator_computation_data <- as.data.frame(readr::read_csv(indicator_status()[i,"File"]))
         indicator_computation_metadata <- NULL #in our TODO list next, how to provide standard statistical metadata for indicators
         #generate/download report
         INFO("Generate and download report")
         print(pool)
         eval(parse(text = paste0(report_def$fun, "(
                                  con = pool,
                                  data = indicator_computation_data, 
                                  metadata = indicator_computation_metadata,
                                  file = file
                            )")))
       }
      )
      
      #event on indicator release
      observeEvent(input[[paste0("button_release_",target_id)]],{
        
        INFO("Click on %s release button",target_id)
        
        filename <- paste0(out$indicator$id, "_", indicator_status()[i,"Period"], ".csv")
        filepath_staging <- file.path(appConfig$store, "staging", out$indicator$id, gsub("-","/",indicator_status()[i,"Period"]), filename)
        filepath <- file.path(appConfig$store, "release", out$indicator$id, gsub("-","/",indicator_status()[i,"Period"]), filename)
        torelease(filepath_staging)
        alreadyReleased <- file.exists(filepath)
        showModal(releaseModal(session, warning = alreadyReleased))
      },ignoreInit = T)
      
      #event on indicator output view
      observeEvent(input[[paste0("button_view_",target_id)]],{
        
        INFO("Click on %s view button",target_id)
        
        #Result table logic
        output[[paste0("table_",period,"_wrapper")]]<-renderUI({
          if (input[[paste0("button_view_",target_id)]] %% 2 != 0) {
            
            output[[paste0("table_",period)]]<-DT::renderDT(server = FALSE, {
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
            
            DTOutput(ns(paste0("table_",period)))%>%withSpinner(type = 4)
          }else{
            NULL
          }
        })
        
        
        #Update the icon of view icon eye (next action show table) or slashed eye(next action hide table) (not work)
        if (input[[paste0("button_view_",target_id)]] %% 2 != 0) {
          updateActionButton(session, ns(paste0("button_view_",target_id)), "", icon = icon("eye-slash", class = "fas" ))
        } else {
          updateActionButton(session, ns(paste0("button_view_",target_id)), "", icon = icon("eye", class = "fas" ))
        }
        
        
      },ignoreInit = T)
      
      #event on computation (either 1st computation or any computation redone in staging)
      observeEvent(input[[paste0("button_compute_",target_id)]],{
        
        INFO("Click on %s compute or update button",target_id)
        
        period_parts<-strsplit(period,"-")[[1]]
        computation_year<-period_parts[1]
        computation_month<-NULL
        computation_quarter<-NULL
        
        if(length(period_parts)>1){
          if(startsWith(period_parts[2],"M")){
            computation_month<-gsub("M","",period_parts[2])
          }
        }
        
        if(length(period_parts)>1){
          if(startsWith(period_parts[2],"Q")){
            computation_quarter<-gsub("Q","",period_parts[2])
          }
        }
        
        computeIndicator(
          con = pool,
          out = out,
          session = session,
          computation_indicator = indicator(),
          computation_target = "release+staging",
          computation_year = computation_year,
          computation_quarter = computation_quarter,
          computation_month = computation_month,
          compute_dependent_indicators = TRUE
        )
        
        },ignoreInit = T)
      
      #event on recomputation (ie computation after output has been released)
      observeEvent(input[[paste0("button_recompute_",target_id)]],{
        filename <- paste0(out$indicator$id, "_", indicator_status()[i,"Period"], ".csv")
        filepath <- file.path(appConfig$store, "release", out$indicator$id, gsub("-","/",indicator_status()[i,"Period"]), filename)
        toarchive(filepath)
        torecompute(indicator_status()[i,"Period"])
        showModal(recomputeModal(session))
      }, ignoreInit = T)
       
    })
    
    #allow to just update the content of the box and not alter box structure
    indicator_first_compute<-indicator_first_compute(FALSE)
  })
  
  #This event actualize the computations status
  observeEvent(out$results,{
    req(indicator_first_compute()==FALSE)
    
    indicator_status_new<-available_periods()%>%
      mutate(period=as.character(period))%>%
      left_join(out$results, by=c("period"="Period"))%>%
      mutate(Status=ifelse(is.na(Status),"available",Status))%>%
      rename(Period=period)
    
    
    if(length(setdiff(full_periods()$Period,indicator_status_new$Period))>0){
      indicator_status_new<-full_periods()%>%
        mutate(year=as.character(year))%>%
        mutate(Period=as.character(Period))%>%
        left_join(indicator_status_new%>%
            mutate(year=as.character(year))%>%
            mutate(Period=as.character(Period)))%>%
        mutate(Status=ifelse(is.na(Status),"not available",Status))
    }
    
    indicator_status<-indicator_status(indicator_status_new)
  })
  
  #Manage release of the indicator
  #--------------------------------------------------------
  #This event release the indicator and update the result
  observeEvent(input$goRelease, {
    WARN("Triggered Release!!!")
    out<-releaseIndicator(out=out,session=session,target=torelease(),release_dependent_indicators=input$releaseDependent)
    reloader <- reloader(id)
    removeModal()
  })
  
  #This event cancel the release request and remove the modal
  observeEvent(input$cancelRelease,{
    torelease(NULL)
    removeModal()
  })
  
  #Manage recomputation of the indicator
  #--------------------------------------------------------
  #This event recomputes the indicator and update the result
  observeEvent(input$goRecompute, {
    req(!is.null(torecompute()))
    INFO("Recompute indicator '%s' for period '%s'", indicator(), torecompute())
    period_parts<-strsplit(torecompute(),"-")[[1]]
    computation_year<-period_parts[1]
    computation_month<-NULL
    computation_quarter<-NULL
    
    if(length(period_parts)>1){
      if(startsWith(period_parts[2],"M")){
        computation_month<-gsub("M","",period_parts[2])
      }
    }
    
    if(length(period_parts)>1){
      if(startsWith(period_parts[2],"Q")){
        computation_quarter<-gsub("Q","",period_parts[2])
      }
    }
    
    computeIndicator(
      con = pool,
      out = out,
      session = session,
      computation_indicator = indicator(),
      computation_target = "release+staging",
      computation_year = computation_year,
      computation_quarter = computation_quarter,
      computation_month = computation_month,
      compute_dependent_indicators = TRUE,
      archive_previous_release = toarchive(),
      archive_reason = input$recomputeReason
    )
    
    torecompute(NULL)
    removeModal()
  }, ignoreInit = T)
  
  #This event cancel the recompute request and remove the modal
  observeEvent(input$cancelRecompute,{
    torecompute(NULL)
    removeModal()
  })
  
  MODULE_END_TIME <- Sys.time()
  INFO("computation: END")
  DEBUG_MODULE_PROCESSING_TIME("computation", MODULE_START_TIME, MODULE_END_TIME)
  
 })
}
