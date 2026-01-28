#artfish_report_server
artfish_report_server <- function(id, parent.session, pool, reloader){

 moduleServer(id, function(input, output, session){   
  
  INFO("artfish-report: START")
  MODULE_START_TIME <- Sys.time() 
   
  ns<-session$ns
  
  #reactives
  estimates<-reactiveVal(NULL)
  target_data<-reactiveVal(NULL)
  status_tempo<-reactiveVal(NULL)
  status<-reactiveVal(NULL)
  
  AVAILABLE_INDICATORS <- getLocalCountryDataset(appConfig,"statistical_indicators.json")
  indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == "artfish_estimates"})]
  effort_source<-indicator[[1]]$compute_with$fun_args$effort_source$source
  if(!is.null(effort_source))effort_source<-gsub("text:","",effort_source)
  minor_strata<-indicator[[1]]$compute_with$fun_args$minor_strata$source
  
  if(!is.null(minor_strata)){
    if(minor_strata=="landing_site"){
      ref_landing_site <- accessLandingSites(pool)
      ref_landing_site<-subset(ref_landing_site, select=c(ID,NAME))
    }
  }
  
  
  
  #data
  fishing_units <- accessRefFishingUnits(pool)
  ref_species <- accessRefSpecies(pool)
  ref_species$Species <- setNames(sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME),ref_species$ID)
  ref_species<-subset(ref_species, select=c(ID,Species))
  
  released_periods<-getStatPeriods(config=appConfig, "artfish_estimates",target="release")
  if(nrow(released_periods)>0)released_periods$file_status<-"release"
  staging_periods<-getStatPeriods(config=appConfig, "artfish_estimates",target="staging")
  if(nrow(staging_periods)>0)staging_periods$file_status<-"staging"
  
  files<-rbind(released_periods,staging_periods)
  
    output$year_selector<-renderUI({
      req(files)
      req(nrow(files)>0)
      
      choices <- unique(files$year)
      selectizeInput(ns("year"),paste0(i18n("SELECT_INPUT_TITLE_YEAR")," :"),choices=choices[order(as.numeric(choices))],multiple = F,selected=NULL,
                     options = list(
                       placeholder = i18n("SELECT_INPUT_PLACEHOLDER_YEAR"),
                       onInitialize = I('function() { this.setValue(""); }')
                       )
                     )
    })
  
  observeEvent(input$year,{
  req(!is.null(input$year)&input$year!="")
    
  output$month_selector<-renderUI({
    
    choices <- unique(subset(files,year==input$year)$month)
    choices <- as.numeric(gsub("M","",choices))
    
    selectizeInput(ns("month"),paste0(i18n("SELECT_INPUT_TITLE_MONTH")," :"),choices=choices[order(choices)],multiple = F,selected=NULL,
                   options = list(
                     placeholder = i18n("SELECT_INPUT_PLACEHOLDER_MONTH"),
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  })
  
  observeEvent(c(input$year,input$month),{
    req(!is.null(input$year)&input$year!="")
    req(!is.null(input$month)&input$month!="")
    
    selection<-subset(files,year==input$year&month==paste0("M",input$month))
    status_tempo(selection$file_status)
    data<-readr::read_csv(selection$file)
    target_data<-target_data(data)

    fishing_units_selection<-subset(fishing_units,ID%in%unique(data$fishing_unit))
    
    choices<-setNames(fishing_units_selection$ID,fishing_units_selection$NAME)
    
    output$fishing_unit_selector<-renderUI({
      selectizeInput(ns("fishing_unit"),paste0(i18n("SELECT_INPUT_TITLE_FISHING_UNIT")," :"),choices=choices,multiple = F,selected=NULL,
                     options = list(
                       placeholder = i18n("SELECT_INPUT_PLACEHOLDER_FISHING_UNIT"),
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
    })
    
    req(!is.null(minor_strata))
    if(minor_strata=="landing_site"){
    
    selection<-subset(ref_landing_site,ID%in%unique(data$landing_site))
    
    choices<-setNames(selection$ID,selection$NAME)
    
    output$minor_strata_selector<-renderUI({
      selectizeInput(ns("minor_strata"),paste0(i18n("SELECT_INPUT_TITLE_LANDING_SITE")," :"),choices=choices,multiple = F,selected=NULL,
                     options = list(
                       placeholder = i18n("SELECT_INPUT_PLACEHOLDER_LANDING_SITE"),
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
    })
    }
    
  })
  
  
  observeEvent(c(input$fishing_unit,input$minor_strata),{
    req(!is.null(input$fishing_unit)&input$fishing_unit!="")
    subdata<-target_data()
    

    subdata<-subset(subdata,fishing_unit==input$fishing_unit)

    if(!is.null(minor_strata)){
      if(minor_strata=="landing_site"){
        subdata<-subset(subdata,landing_site==input$minor_strata)
      }
    }

    estimates<-estimates(subdata)
    
    output$button<-renderUI({
        actionButton(ns("btn"),i18n("ACTION_BUTTON_TITLE_SUBMIT"))
    })
  })
  
  observeEvent(input$btn,{
    tmp<-status_tempo()
    status(tmp)
    
    estimate<-estimates()
    
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('display', 'none');",
      "}"
    )
    
    output$accuracy<-renderPlotly({

      accuracy<-estimate$overall_accuracy*100
      req(!is.null(accuracy))
      req(!is.na(accuracy))
      
      plot_ly(
        domain = list(x = c(0.20, 0.80), y = c(0, 0.90)),
        value = accuracy,
        title = list(text = paste0(i18n("ARTFISH_REPORT_GAUGE_TITLE")," (%)")),
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis =list(range = list(NULL,100)),
          bar = list(color = "grey"),
          steps = list(
            list(range = c(0, 90), color = "#ffc163"),
            list(range = c(90, 100), color = "#cbe261"))
        )) %>%
        layout(height=200,
               margin = list(l=20,r=30,0,0),
               plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)")
      
    })
    
    output$info_block <- renderUI({
      req(data)
      fluidRow(
        bs4InfoBox(
          title = i18n("ARTFISH_REPORT_INFOBOX_STATUS_TITLE"),
          value = switch(status(),
                         "release" = {
                           i18n("ARTFISH_REPORT_RELEASE_LABEL")
                         },
                         "staging" = {
                           i18n("ARTFISH_REPORT_STAGING_LABEL")
                         }),
          icon = switch(status(),
                        "release" = {
                          icon("circle-check")
                        },
                        "staging" = {
                          icon("clock")
                        }),
          color = switch(status(),
                         "release" = {
                           "success"
                         },
                         "staging" = {
                           "warning"
                           }),
          width = 12
        ),
        bs4InfoBox(
          title = i18n("ARTFISH_REPORT_INFOBOX_EFFORT_SOURCE_TITLE"),
          value = switch(effort_source,
                         "boat_counting" = {
                           i18n("ARTFISH_REPORT_BOAT_COUNTING_LABEL")
                         },
                         "fisher_interview" = {
                           i18n("ARTFISH_REPORT_FISHER_INTERVIEW_LABEL")
                         }),
          icon = switch(effort_source,
                        "boat_counting" = {
                          icon("ship")
                        },
                        "fisher_interview" = {
                          icon("user")
                        }),
          color = switch(effort_source,
                         "boat_counting" = {
                           "success"
                         },
                         "fisher_interview" = {
                           "warning"
                         }),
          width = 12
        )
      )
    })
    
    output$effort <- DT::renderDT(server = FALSE, {
      
      codes<-c(i18n("EFFORT_CODE_1"),i18n("EFFORT_CODE_2"),i18n("EFFORT_CODE_3"),
               i18n("EFFORT_CODE_4"),i18n("EFFORT_CODE_5"),i18n("EFFORT_CODE_6"),
               i18n("EFFORT_CODE_7"),i18n("EFFORT_CODE_8"),i18n("EFFORT_CODE_9"),
               i18n("EFFORT_CODE_10"))
      
      labels<-c(i18n("EFFORT_LABEL_1"),i18n("EFFORT_LABEL_2"),i18n("EFFORT_LABEL_3"),
                i18n("EFFORT_LABEL_4"),i18n("EFFORT_LABEL_5"),i18n("EFFORT_LABEL_6"),
                i18n("EFFORT_LABEL_7"),i18n("EFFORT_LABEL_8"),i18n("EFFORT_LABEL_9"),
                i18n("EFFORT_LABEL_10")
      )
      
      values <- switch(effort_source,
        "fisher_interview" = c(
          ifelse(!is.na(estimate$effort_nominal[1]), formatC(estimate$effort_nominal[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$fleet_engagement_number[1]), formatC(estimate$fleet_engagement_number[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$effort_fishable_duration[1]), formatC(estimate$effort_fishable_duration[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$effort_activity_coefficient[1]), formatC(estimate$effort_activity_coefficient[1],digits = 3, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$effort_total_fishing_duration[1]), formatC(estimate$effort_total_fishing_duration[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F), "–"), #only for fisher_interview
          ifelse(!is.na(estimate$effort_fishing_reference_period[1]), formatC(estimate$effort_fishing_reference_period[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$effort_coefficient_variation[1]), paste0(format(round(estimate$effort_coefficient_variation[1]*100,1), nsmall = 1)," %"), "–"),
          ifelse(!is.na(estimate$effort_activity_coefficient_spatial_accuracy[1]), paste0(format(round(estimate$effort_activity_coefficient_spatial_accuracy[1]*100,1), nsmall = 1)," %"), "–"),
          ifelse(!is.na(estimate$effort_activity_coefficient_temporal_accuracy[1]), paste0(format(round(estimate$effort_activity_coefficient_temporal_accuracy[1]*100,1), nsmall = 1)," %"), "–"),
          ifelse(!is.na(estimate$effort_sui[1]), format(round(estimate$effort_sui[1],1), nsmall = 1), "–")
        ),
        "boat_counting" = c(
          ifelse(!is.na(estimate$effort_nominal[1]), formatC(estimate$effort_nominal[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$fleet_engagement_number[1]), formatC(estimate$fleet_engagement_number[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$effort_fishable_duration[1]), formatC(estimate$effort_fishable_duration[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$effort_activity_coefficient[1]), formatC(estimate$effort_activity_coefficient[1],digits = 3, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$effort_fishing_reference_period[1]), formatC(estimate$effort_fishing_reference_period[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F), "–"),
          ifelse(!is.na(estimate$effort_coefficient_variation[1]), paste0(format(round(estimate$effort_coefficient_variation[1]*100,1), nsmall = 1)," %"), "–"),
          ifelse(!is.na(estimate$effort_activity_coefficient_spatial_accuracy[1]), paste0(format(round(estimate$effort_activity_coefficient_spatial_accuracy[1]*100,1), nsmall = 1)," %"), "–"),
          ifelse(!is.na(estimate$effort_activity_coefficient_temporal_accuracy[1]), paste0(format(round(estimate$effort_activity_coefficient_temporal_accuracy[1]*100,1), nsmall = 1)," %"), "–"),
          ifelse(!is.na(estimate$effort_sui[1]), format(round(estimate$effort_sui[1],1), nsmall = 1), "–")
        )
      )
      
      if(effort_source == "boat_counting"){
        #no effort_total_fishing_duration, 1 column less
        codes = codes[-5]
        labels = labels[-5]
      }
      
      effort<-data.frame(code=codes,
                         label=labels,
                         value=values)
      
      DT::datatable(
        effort,
        escape = FALSE,
        rownames = FALSE,
        selection = 'none',
        options = list(
          dom = 't',
          pageLength = 10,
          headerCallback = JS(headerCallback),
          language = list(url = i18n("TABLE_LANGUAGE")))
        
      )  
    })
    
    output$landing<-  DT::renderDT(server = FALSE, {
      
      codes<-c(i18n("LANDINGSITE_CODE_1"),i18n("LANDINGSITE_CODE_2"),i18n("LANDINGSITE_CODE_3"),
               i18n("LANDINGSITE_CODE_4"),i18n("LANDINGSITE_CODE_5"),i18n("LANDINGSITE_CODE_6"),
               i18n("LANDINGSITE_CODE_7"),i18n("LANDINGSITE_CODE_8"),i18n("LANDINGSITE_CODE_9"),
               i18n("LANDINGSITE_CODE_10"))
      
      labels<-c(i18n("LANDINGSITE_LABEL_1"),i18n("LANDINGSITE_LABEL_2"),i18n("LANDINGSITE_LABEL_3"),
                i18n("LANDINGSITE_LABEL_4"),i18n("LANDINGSITE_LABEL_5"),i18n("LANDINGSITE_LABEL_6"),
                i18n("LANDINGSITE_LABEL_7"),i18n("LANDINGSITE_LABEL_8"),i18n("LANDINGSITE_LABEL_9"),
                i18n("LANDINGSITE_LABEL_10"))
      
      values<-c(
        formatC(sum(estimate$catch_nominal_landed,na.rm=T),digits = 0, format = "f", big.mark = ",", drop0trailing = F),
        formatC(sum(estimate$catch_cpue,na.rm=T),digits = 3, format = "f", big.mark = ",", drop0trailing = F),
        formatC(estimate$catch_nominal_landed_sampled[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
        formatC(estimate$catch_sample_size[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
        formatC(sum(estimate$trade_value,na.rm=T)/sum(estimate$catch_nominal_landed,na.rm=T),digits = 0, format = "f", big.mark = ",", drop0trailing = F),
        formatC(sum(estimate$trade_value,na.rm=T),digits = 0, format = "f", big.mark = ",", drop0trailing = F),
        ifelse(!is.na(estimate$catch_coefficient_variation[1]),paste0(format(round(estimate$catch_coefficient_variation[1]*100,1), nsmall = 1)," %"),"–"),
        ifelse(!is.na(estimate$catch_cpue_spatial_accuracy[1]),paste0(format(round(estimate$catch_cpue_spatial_accuracy[1]*100,1), nsmall = 1)," %"),"–"),
        ifelse(!is.na(estimate$catch_cpue_temporal_accuracy[1]),paste0(format(round(estimate$catch_cpue_temporal_accuracy[1]*100,1), nsmall = 1)," %"),"–"),
        ifelse(!is.na(estimate$catch_sui[1]),format(round(estimate$catch_sui[1],1), nsmall = 1),"–")
      )
      
      landing<-data.frame(code=codes,
                          label=labels,
                          value=values)
      
      DT::datatable(
        landing,
        escape = FALSE,
        rownames = FALSE,
        selection = 'none',
        options = list(
          dom = 't',
          pageLength = 10,
          headerCallback = JS(headerCallback),
          language = list(url = i18n("TABLE_LANGUAGE")))
      )  
    })
    
    output$OvAcc<- DT::renderDT(server = FALSE, {
      
      OvAcc<-data.frame(code="",
                          label=i18n("LABEL_ACCURACY"),
                          value=paste0(format(round(estimate$overall_accuracy[1]*100,1), nsmall = 1)," %"))
      
      DT::datatable(
        OvAcc,
        escape = FALSE,
        rownames = FALSE,
        selection = 'none',
        options = list(
          dom = 't',
          headerCallback = JS(headerCallback),
          language = list(url = i18n("TABLE_LANGUAGE")))
      )  
    })
    
    
    output$species<- DT::renderDT(server = FALSE, {
      
      species<-subset(estimate,select=c(species,catch_nominal_landed,catch_cpue,trade_price,trade_value,catch_fish_average_weight ))
      print("debug species")
      print(species)
      names(species)<-c('NAME',i18n("SPECIES_TABLE_COLNAME_2"),i18n("SPECIES_TABLE_COLNAME_4"),
                        i18n("SPECIES_TABLE_COLNAME_5"),i18n("SPECIES_TABLE_COLNAME_6"),
                        i18n("SPECIES_TABLE_COLNAME_7"))
     
      NAME <- paste0(species,"$",i18n("SPECIES_TABLE_COLNAME_1"))
      species<- species%>%
        left_join(ref_species,by=c('NAME'="ID"))%>%
        select(Species,i18n("SPECIES_TABLE_COLNAME_2"),
               i18n("SPECIES_TABLE_COLNAME_4"),i18n("SPECIES_TABLE_COLNAME_5"),
               i18n("SPECIES_TABLE_COLNAME_6"),i18n("SPECIES_TABLE_COLNAME_7"))

     
      
      DT::datatable(
        species,
        escape = FALSE,
        rownames = FALSE,
        colnames = c(i18n("SPECIES_TABLE_COLNAME_1"),i18n("SPECIES_TABLE_COLNAME_2"),
                     i18n("SPECIES_TABLE_COLNAME_4"),i18n("SPECIES_TABLE_COLNAME_5"),
                     i18n("SPECIES_TABLE_COLNAME_6"),i18n("SPECIES_TABLE_COLNAME_7")),
        selection = 'none',
        options = list(
          dom = 't',
          pageLength = nrow(species),
          language = list(url = i18n("TABLE_LANGUAGE"))
        )
      ) %>% formatRound(i18n("SPECIES_TABLE_COLNAME_2"), digits=0) %>% 
        formatRound(i18n("SPECIES_TABLE_COLNAME_4"), digits = 2) %>% 
        formatRound(i18n("SPECIES_TABLE_COLNAME_5"), digits = 2) %>% 
        formatRound(i18n("SPECIES_TABLE_COLNAME_6"), digits = 0) %>%
        formatRound(i18n("SPECIES_TABLE_COLNAME_7"), digits = 1)
    })
    
    output$results<-renderUI({
      tagList(
      p(i18n("LABEL_EFFORT")),
      DTOutput(ns("effort"))%>%withSpinner(type = 4),
      br(),
      p(i18n("LABEL_LANDINGS")),
      DTOutput(ns("landing"))%>%withSpinner(type = 4),
      br(),
      DTOutput(ns("OvAcc"))%>%withSpinner(type = 4),
      br(),
      p(i18n("LABEL_ESTIMATED_BY_SPECIES")),
      DTOutput(ns("species"))%>%withSpinner(type = 4),
      )
    })
  })
  
  MODULE_END_TIME <- Sys.time()
  INFO("artfish-report: END")
  DEBUG_MODULE_PROCESSING_TIME("Artfish-report", MODULE_START_TIME, MODULE_END_TIME)
  
 })
}