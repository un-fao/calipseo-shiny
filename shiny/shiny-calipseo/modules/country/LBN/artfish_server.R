#artfish_server
artfish_server <- function(input, output, session, pool){

ns<-session$ns
  
  output$urlPage<-renderUI({
    session$userData$page("artfish")
    updatePageUrl("artfish", session)
  })
  
  survey<-accessSurveyDateAndStratum(pool)
  
  ref_species<-accessRefSpecies(pool)
  ref_species$Species<-setNames(sprintf("%s [%s]",ref_species$SCIENTIFIC_NAME,ref_species$NAME),ref_species$ID)
  ref_species<-subset(ref_species,select=c(ID,Species))
  
  output$period_selector<-renderUI({
    
    dates<-unique(survey$date)
    dates<- dates[!startsWith(dates, "2013")] #we exclude 2013 from Flouca historical data
    #TODO to check what happens with 2014 reports
    
    selectizeInput(ns("period"),"Selection of period",choices=dates,multiple = F,selected=NULL,
                   options = list(
                     placeholder = 'select a period',
                     onInitialize = I('function() { this.setValue(""); }')
                     )
                   )
  })
  
  observeEvent(input$period,{
    
    output$stratum_selector<-renderUI({
      if(!is.null(input$period))if(input$period!=""){
      stratum<-subset(survey,date==input$period)
      selectizeInput(ns("stratum"),"Selection of stratum",choices=setNames(stratum$strat_id,stratum$strat_name),multiple = F)
      }
    })
    
    output$button<-renderUI({
      if(!is.null(input$period))if(input$period!=""){
        actionButton(ns("btn"),"Submit")
      }
    })
  })
  
  observeEvent(input$btn,{
    
    data_effort<-accessEffortData(pool,year=as.integer(unlist(strsplit(input$period,"-"))[1]),month=as.integer(unlist(strsplit(input$period,"-"))[2]),fishing_unit=input$stratum)
    
    data_landing<-accessLandingData(pool,year=as.integer(unlist(strsplit(input$period,"-"))[1]),month=as.integer(unlist(strsplit(input$period,"-"))[2]),fishing_unit=input$stratum)
    
    estimate<-artfish_estimates(data_effort=data_effort,data_landing=data_landing)
    
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('display', 'none');",
      "}"
    )
    
    output$effort <- DT::renderDT(server = FALSE, {
      
      codes<-c("(E)","(NB)","(FDM)","(PBA)","(NAD)","(NDE)","(CV)","(SAE)","(TAE)","(SUI)")
      
      labels<-c("Estimated Effort = (PBA)x(NB)x(FDM)",
                "Number of boats-gears",
                "Fishing days in month",
                "Probability Boat Active = (NAD)/(NDE)",
                "Number of days declared active",
                "Total number of days examined",
                "Coefficient of variation",
                "Spacial Accuracy (effort)",
                "Temporal Accuracy (effort)",
                "Sampling Uniformity index (0 - 1)"
      )
      
      values<-c(formatC(estimate$EST_EFF_EFFORT[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$EST_EFF_NBOATS[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$EST_EFF_NACT[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$EST_EFF_PBA[1],digits = 3, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$EST_EFF_ACTDAYS[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$EST_EFF_EXDAYS[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                paste0(format(round(estimate$EST_EFF_CV[1]*100,1), nsmall = 1)," %"),
                paste0(format(round(estimate$EST_EFF_SPAACCUR[1]*100,1), nsmall = 1)," %"),
                paste0(format(round(estimate$EST_EFF_TMPACCUR[1]*100,1), nsmall = 1)," %"),
                format(round(estimate$EST_EFF_SUI[1],1), nsmall = 1))
      
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
          headerCallback = JS(headerCallback))
      )  
    })
    
    output$landing<-  DT::renderDT(server = FALSE, {
      
      codes<-c("","","(SMC)","(SME)","","","(CV)","(SAC)","(TAC)","(SUI)")
      labels<-c("Estimated catch = Estimated Effort x CPUE",
                "CPUE = (SMC)/(SME)",
                "Sample catch",
                "Sample Effort",
                "Overall Price (Unit Value)",
                "Total value",
                "Coefficient of variation",
                "Spacial Accuracy (catch)",
                "Temporal Accuracy (catch)",
                "Sampling Uniformity index (0 - 1)")
      
      values<-c(formatC(estimate$EST_LND_CATCH_G[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(sum(estimate$EST_LND_CPUE,na.rm=T),digits = 3, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$EST_LND_SMPCATCH[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(estimate$EST_LND_NSMP[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(sum(estimate$EST_LND_VALUE,na.rm=T)/estimate$EST_LND_CATCH_G[1],digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                formatC(sum(estimate$EST_LND_VALUE,na.rm=T),digits = 0, format = "f", big.mark = ",", drop0trailing = F),
                paste0(format(round(estimate$EST_LND_CV[1]*100,1), nsmall = 1)," %"),
                paste0(format(round(estimate$EST_LND_SPAACCUR[1]*100,1), nsmall = 1)," %"),
                paste0(format(round(estimate$EST_LND_TMPACCUR[1]*100,1), nsmall = 1)," %"),
                format(round(estimate$EST_LND_SUI[1],1), nsmall = 1))
      
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
          headerCallback = JS(headerCallback))
      )  
    })
    
    output$OvAcc<- DT::renderDT(server = FALSE, {
      
      OvAcc<-data.frame(code="",
                          label="Overall Accuracy = Minimum of TAE, SAE, TAC, SAC",
                          value=paste0(format(round(estimate$EST_ACCUR[1]*100,1), nsmall = 1)," %"))
      
      DT::datatable(
        OvAcc,
        escape = FALSE,
        rownames = FALSE,
        selection = 'none',
        options = list(
          dom = 't',
          headerCallback = JS(headerCallback))
      )  
    })
    
    output$species<- DT::renderDT(server = FALSE, {
      
      species<-subset(estimate,select=c(EST_SPC,EST_LND_CATCH,EST_EFF_EFFORT,EST_LND_CPUE,EST_LND_PRICE,EST_LND_VALUE,EST_LND_AVW))
      names(species)<-c("EST_SPC","Quantity","Effort","CPUE","Price","Value","Aver.weight")
      species$Quantity<-formatC(species$Quantity,digits = 0, format = "f", big.mark = ",", drop0trailing = F)
      species$Effort<-formatC(species$Effort,digits = 0, format = "f", big.mark = ",", drop0trailing = F)
      species$CPUE<-formatC(species$CPUE,digits = 3, format = "f", big.mark = ",", drop0trailing = F)
      species$Price<-formatC(species$Price,digits = 3, format = "f", big.mark = ",", drop0trailing = F)
      species$Value<-formatC(species$Value,digits = 0, format = "f", big.mark = ",", drop0trailing = F)
      
      species<-species%>%
        left_join(ref_species,by=c("EST_SPC"="ID"))%>%
        select(Species,Quantity,Effort,CPUE,Price,Value,Aver.weight)
      
      DT::datatable(
        species,
        escape = FALSE,
        rownames = FALSE,
        selection = 'none',
        options = list(
          dom = 't',
          pageLength = nrow(species)
        )
      )  
    })
    
    output$results<-renderUI({
      tagList(
      p("Effort"),
      DTOutput(ns("effort"))%>%withSpinner(type = 4),
      br(),
      p("Landings"),
      DTOutput(ns("landing"))%>%withSpinner(type = 4),
      br(),
      DTOutput(ns("OvAcc"))%>%withSpinner(type = 4),
      br(),
      p("Estimated catch by species"),
      DTOutput(ns("species"))%>%withSpinner(type = 4),
      )
    })
  })
}