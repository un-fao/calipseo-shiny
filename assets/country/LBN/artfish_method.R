###Artfish estimates
artfish_estimates<-function(con,year=NULL,month=NULL,data_effort=NULL,data_landing=NULL){
  
  if(is.null(data_effort))data_effort=accessEffortData(con,year,month)
  if(is.null(data_landing))data_landing=accessLandingData(con,year,month)
  
  fishing_units<-accessFishingUnits(con)
  fishing_units<-subset(fishing_units,select=c(code,label))
  names(fishing_units)<-c("EST_BGC","EST_BGC_NAME")
  
  ref_species<-accessRefSpecies(con)
  ref_species$Species<-setNames(sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME),ref_species$ID)
  ref_species$ID<-as.character(ref_species$ID)
  ref_species<-subset(ref_species,select=c(ID,Species))
  names(ref_species)<-c("EST_SPC","EST_SPC_NAME")
  
  estimate<-artfishr::artfish_estimates(
    data_effort = data_effort, data_landing = data_landing,
    ref_fishingunits = fishing_units, ref_species = ref_species,
    year = year, month = month
  )
  
  return(estimate)
  
}

###Artfish estimates
artfish_estimates_by_fleet_segment<-function(con,year=NULL,month=NULL,data_effort=NULL,data_landing=NULL){
  
  if(is.null(data_effort))data_effort=accessEffortDataByFleetSegment(con,year,month)
  if(is.null(data_landing))data_landing=accessLandingDataByFleetSegment(con,year,month)
  
  fishing_units<-accessFishingUnits(con)
  fishing_units<-subset(fishing_units,select=c(code,label))
  names(fishing_units)<-c("EST_BGC","EST_BGC_NAME")
  
  ref_species<-accessRefSpecies(con)
  ref_species$Species<-setNames(sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME),ref_species$ID)
  ref_species$ID<-as.character(ref_species$ID)
  ref_species<-subset(ref_species,select=c(ID,Species))
  names(ref_species)<-c("EST_SPC","EST_SPC_NAME")
  
  
  estimate<-artfishr::artfish_estimates_by_fleet_segment(
    data_effort = data_effort, data_landing = data_landing,
    ref_fishingunits = fishing_units, ref_species = ref_species,
    year = year, month = month
  )
  
  return(estimate)
}

###Artfish year summary
artfish_year_summary<-function(data,year=NULL,variable,value, value_fun = sum, levels=NULL){
  
  if(!is.null(year)){
    data<-subset(data,EST_YEAR==year)
  }
  
  accuracy<-mean(data$EST_ACCUR,na.rm=T)

  summary<-data
  
  if(!is.null(levels)){
    summary<-summary%>%filter(!!sym(variable)%in%levels)
  }
  
  summary<-summary%>%
    select(!!sym(variable),EST_MONTH,!!sym(value)) %>%
    group_by(!!sym(variable),EST_MONTH)%>%
    dplyr::summarise(!!value:=value_fun(!!sym(value),na.rm=T))%>%
    dplyr::mutate(EST_MONTH=sprintf('%02d',EST_MONTH),
           !!variable:= as.character(!!sym(variable))) %>%
    ungroup() %>% 
    complete(nesting(!!sym(variable)),EST_MONTH=c("01","02","03","04","05","06","07","08","09","10","11","12"))%>%
   bind_rows(group_by(.,!!sym(variable)) %>%
               dplyr::summarise(!!value:=sum(!!sym(value),na.rm=T)) %>%
               dplyr::mutate(EST_MONTH='Total')) %>%
   bind_rows(group_by(.,EST_MONTH) %>%
               dplyr::summarise(!!value:=sum(!!sym(value),na.rm=T)) %>%
               dplyr::mutate(!!variable:='Total')) %>%
   pivot_wider(names_from = EST_MONTH, values_from = !!sym(value))%>%
   select(!!sym(variable),`01`,`02`,`03`,`04`,`05`,`06`,`07`,`08`,`09`,`10`,`11`,`12`,"Total")
             
  rank<-summary%>%
    select(!!sym(variable),Total)%>%
    filter(!!sym(variable)!="Total")%>%
    dplyr::mutate(rank=rank(-Total))%>%
    dplyr::mutate(percent=Total/sum(Total))%>%
    arrange(-percent)%>%
    dplyr::mutate(cum_percent=cumsum(percent))%>%
    select(!!sym(variable),rank,Total,percent,cum_percent)
             
  return(list(accuracy=accuracy,summary=summary,rank=rank))
             
}

