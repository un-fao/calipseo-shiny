#artfish_report_server
artfish_report_server <- function(id, pool, reloader){

 moduleServer(id, function(input, output, session){   
  
  ns<-session$ns
  estimates<-reactiveVal(NULL)
  target_data<-reactiveVal(NULL)
  
  fishing_units<-accessRefFishingUnits(pool)
  
  ref_species<-accessRefSpecies(pool)
  ref_species$Species<-setNames(sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME),ref_species$ID)
  ref_species<-subset(ref_species,select=c(ID,Species))
  
  output$mode_selector<-renderUI({
    
    selectizeInput(ns("mode"),paste0(i18n("SELECT_INPUT_TITLE_MODE")," :"),choices=c("release","staging"),multiple = F,selected="release")
  
  })
  
  observeEvent(c(input$mode,session$userData$computation_new()),{
    req(!is.null(input$mode)&input$mode!="")
  
    output$year_selector<-renderUI({
      
      #dates<-unique(survey$date)
      #dates<- dates[!startsWith(dates, "2013") & !startsWith(dates, "2014")] #we exclude 2013 from Flouca historical data
      #TODO to check what happens with 2014 reports
      
      choices <- unique(getStatPeriods(config=appConfig, "artfish_estimates",target = input$mode)$year)
      
      print(choices)
      
      selectizeInput(ns("year"),paste0(i18n("SELECT_INPUT_TITLE_YEAR")," :"),choices=choices[order(as.numeric(choices))],multiple = F,selected=NULL,
                     options = list(
                       placeholder = i18n("SELECT_INPUT_PLACEHOLDER_YEAR"),
                       onInitialize = I('function() { this.setValue(""); }')
                       )
                     )
    })
  })
  
  observeEvent(input$year,{
  req(!is.null(input$year)&input$year!="")
    
  output$month_selector<-renderUI({
    
    choices <- getStatPeriods(config=appConfig, "artfish_estimates",target = input$mode)
    choices <- unique(subset(choices,year==input$year)$month)
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
    
    data<-getStatPeriods(config=appConfig, "artfish_estimates",target = input$mode)
    data<-subset(data,year==input$year&month==paste0("M",input$month))$file
    data<-readr::read_csv(data)
    target_data<-target_data(data)
    
    print(head(data))
    
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
    
  })
  
  observeEvent(input$fishing_unit,{
    req(!is.null(input$fishing_unit)&input$fishing_unit!="")
    subdata<-target_data()
    subdata<-subset(subdata,fishing_unit==input$fishing_unit)
    estimates<-estimates(subdata)
    
    output$button<-renderUI({
        actionButton(ns("btn"),i18n("ACTION_BUTTON_TITLE_SUBMIT"))
    })
  })
  
  observeEvent(input$btn,{
    
    # data_effort<-accessEffortData(pool,year=as.integer(unlist(strsplit(input$period,"-"))[1]),month=as.integer(unlist(strsplit(input$period,"-"))[2]),fishing_unit=input$stratum)
    # data_landing<-accessLandingData(pool,year=as.integer(unlist(strsplit(input$period,"-"))[1]),month=as.integer(unlist(strsplit(input$period,"-"))[2]),fishing_unit=input$stratum)
    # estimate<-artfish_estimates(data_effort=data_effort,data_landing=data_landing)
    
    estimate<-estimates()
    
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('display', 'none');",
      "}"
    )
    
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
      
      values<-c(formatC(estimate$effort_nominal[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$fleet_engagement_number[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$effort_fishable_duration[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$effort_activity_coefficient[1],digits = 3, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$effort_total_fishing_duration[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$effort_fishing_reference_period[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                paste0(format(round(estimate$effort_coefficient_variation[1]*100,1), nsmall = 1)," %"),
                paste0(format(round(estimate$effort_activity_coefficient_spatial_accuracy[1]*100,1), nsmall = 1)," %"),
                paste0(format(round(estimate$effort_activity_coefficient_temporal_accuracy[1]*100,1), nsmall = 1)," %"),
                format(round(estimate$effort_sui[1],1), nsmall = 1))
      
      print(values)
      
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
      
      values<-c(formatC(sum(estimate$catch_nominal_landed,na.rm=T),digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(sum(estimate$catch_cpue,na.rm=T),digits = 3, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$catch_nominal_landed_sampled[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$catch_sample_size[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(sum(estimate$trade_value,na.rm=T)/sum(estimate$catch_nominal_landed,na.rm=T),digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(sum(estimate$trade_value,na.rm=T),digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                paste0(format(round(estimate$catch_coefficient_variation[1]*100,1), nsmall = 1)," %"),
                paste0(format(round(estimate$catch_cpue_spatial_accuracy[1]*100,1), nsmall = 1)," %"),
                paste0(format(round(estimate$catch_cpue_temporal_accuracy[1]*100,1), nsmall = 1)," %"),
                format(round(estimate$catch_sui[1],1), nsmall = 1))
      
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
      
      species<-subset(estimate,select=c(species,catch_nominal_landed,effort_nominal,catch_cpue,trade_price,trade_value,catch_fish_average_weight ))
      names(species)<-c('NAME',i18n("SPECIES_TABLE_COLNAME_2"),
                        i18n("SPECIES_TABLE_COLNAME_3"),i18n("SPECIES_TABLE_COLNAME_4"),
                        i18n("SPECIES_TABLE_COLNAME_5"),i18n("SPECIES_TABLE_COLNAME_6"),
                        i18n("SPECIES_TABLE_COLNAME_7"))
     
      NAME <- paste0(species,"$",i18n("SPECIES_TABLE_COLNAME_1"))
      # print(species$NAME)
      # print(ref_species$ID)
      species<- species%>%
        left_join(ref_species,by=c('NAME'="ID"))%>%
        select(Species,i18n("SPECIES_TABLE_COLNAME_2"),i18n("SPECIES_TABLE_COLNAME_3"),
               i18n("SPECIES_TABLE_COLNAME_4"),i18n("SPECIES_TABLE_COLNAME_5"),
               i18n("SPECIES_TABLE_COLNAME_6"),i18n("SPECIES_TABLE_COLNAME_7"))

     
      
      DT::datatable(
        species,
        escape = FALSE,
        rownames = FALSE,
        colnames = c(i18n("SPECIES_TABLE_COLNAME_1"),i18n("SPECIES_TABLE_COLNAME_2"),i18n("SPECIES_TABLE_COLNAME_3"),
                     i18n("SPECIES_TABLE_COLNAME_4"),i18n("SPECIES_TABLE_COLNAME_5"),
                     i18n("SPECIES_TABLE_COLNAME_6"),i18n("SPECIES_TABLE_COLNAME_7")),
        selection = 'none',
        options = list(
          dom = 't',
          pageLength = nrow(species),
          language = list(url = i18n("TABLE_LANGUAGE"))
        )
      ) %>% formatRound(i18n("SPECIES_TABLE_COLNAME_2"), digits=0) %>% 
        formatRound(i18n("SPECIES_TABLE_COLNAME_3"), digits=0) %>% 
        formatRound(i18n("SPECIES_TABLE_COLNAME_4"), digits = 3) %>% 
        formatRound(i18n("SPECIES_TABLE_COLNAME_5"), digits = 3) %>% 
        formatRound(i18n("SPECIES_TABLE_COLNAME_6"), digits = 0)
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
  
 })
}