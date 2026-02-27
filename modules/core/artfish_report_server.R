#artfish_report_server
artfish_report_server <- function(id, parent.session, pool, reloader){

 moduleServer(id, function(input, output, session){   
  
  INFO("artfish-report: START")
  MODULE_START_TIME <- Sys.time() 
   
  ns<-session$ns
  
  AVAILABLE_INDICATORS <- getLocalCountryDataset(appConfig,"statistical_indicators.json")
  indicator <- AVAILABLE_INDICATORS[sapply(AVAILABLE_INDICATORS, function(x){x$id == "artfish_estimates"})]
  effort_source<-indicator[[1]]$compute_with$fun_args$effort_source$source
  if(!is.null(effort_source))effort_source<-gsub("text:","",effort_source)
  minor_strata<-indicator[[1]]$compute_with$fun_args$minor_strata$source

  ref_landing_site <-NULL
  
  if(!is.null(minor_strata)){
    if(minor_strata=="landing_site"){
      ref_landing_site <- accessLandingSites(pool)
      ref_landing_site<-subset(ref_landing_site, select=c(ID,NAME))
    }
  }
  
  #data

  ref_species <- accessRefSpecies(pool)
  ref_species$Species <- setNames(sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME),ref_species$ID)
  ref_species<-subset(ref_species, select=c(ID,Species))
  ref_fishing_unit <- accessRefFishingUnits(pool)
  
  released_periods<-getStatPeriods(config=appConfig, "artfish_estimates",target="release")
  if(nrow(released_periods)>0)released_periods$file_status<-"release"
  staging_periods<-getStatPeriods(config=appConfig, "artfish_estimates",target="staging")
  if(nrow(staging_periods)>0)staging_periods$file_status<-"staging"
  
  files<-rbind(released_periods,staging_periods)

  artfishr::artfish_shiny_report_server("artfish_report",
                                        lang = appConfig$language,
                                        files = files,
                                        effort_source = effort_source,
                                        minor_strata=minor_strata,
                                        ref_species=ref_species,
                                        ref_fishing_unit=ref_fishing_unit,
                                        ref_landing_site=ref_landing_site)
  
  MODULE_END_TIME <- Sys.time()
  INFO("artfish-report: END")
  DEBUG_MODULE_PROCESSING_TIME("Artfish-report", MODULE_START_TIME, MODULE_END_TIME)
  
 })
}