###Artfish estimates
artfish_estimates<-function(con,year=NULL,month=NULL,effort=NULL,effort_source=c("fisher_interview","boat_counting"),active_vessels=NULL,active_vessels_strategy=NULL,active_days=NULL,landings=NULL,minor_strata=NULL){
  
  effort_source = match.arg(effort_source)
  if(is.null(active_vessels))active_vessels=accessArtfishA(con,year,month)
  if(is.null(effort)) effort = switch(effort_source,
    "fisher_interview" = accessArtfishB1(con,year,month),
    "boat_counting" = accessArtfishB2(con,year,month)
  )
  if(is.null(active_days))active_days=accessArtfishC(con,year,month)
  if(is.null(landings))landings=accessArtfishD(con,year,month)
  
  # fishing_units<-accessRefFishingUnits(con)
  # fishing_units<-subset(fishing_units,select=c(CODE,NAME))
  # names(fishing_units)<-c("EST_BGC","EST_BGC_NAME")
  # 
  # ref_species<-accessRefSpecies(con)
  # ref_species$Species<-setNames(sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME),ref_species$ID)
  # ref_species$ID<-as.character(ref_species$ID)
  # ref_species<-subset(ref_species,select=c(ID,Species))
  # names(ref_species)<-c("EST_SPC","EST_SPC_NAME")
  
  estimate<-artfishr::compute_report(
    effort=effort,
    effort_source=effort_source,
    active_vessels=active_vessels,
    active_vessels_strategy=active_vessels_strategy,
    active_days=active_days,
    landings=landings,
    minor_strata=minor_strata)
  
  return(estimate)
  
}

###Artfish year summary
artfish_year_summary<-function(data,target_year=NULL,variable,value, value_fun = sum, levels=NULL){
  
  if(!is.null(target_year)){
    data<-subset(data,year==target_year)
  }
  
  accuracy<-mean(data$overall_accuracy,na.rm=T)

  summary<-data
  
  if(!is.null(levels)){
    summary<-summary%>%filter(!!sym(variable)%in%levels)
  }
  
  summary<-summary%>%
    select(!!sym(variable),month,!!sym(value)) %>%
    group_by(!!sym(variable),month)%>%
    dplyr::summarise(!!value:=value_fun(!!sym(value),na.rm=T))%>%
    dplyr::mutate(month=sprintf('%02d',month),
           !!variable:= as.character(!!sym(variable))) %>%
    ungroup() %>% 
    complete(nesting(!!sym(variable)),month=c("01","02","03","04","05","06","07","08","09","10","11","12"))%>%
   bind_rows(group_by(.,!!sym(variable)) %>%
               dplyr::summarise(!!value:=sum(!!sym(value),na.rm=T)) %>%
               dplyr::mutate(month='Total')) %>%
   bind_rows(group_by(.,month) %>%
               dplyr::summarise(!!value:=sum(!!sym(value),na.rm=T)) %>%
               dplyr::mutate(!!variable:='Total')) %>%
   pivot_wider(names_from = month, values_from = !!sym(value))%>%
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

