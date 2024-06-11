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
  
  effort<-data_effort%>%
    dplyr::rename(EST_YEAR=year,
           EST_MONTH=month,
           EST_BGC=fishing_unit)%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::summarise(
      EST_EFF_NSMP=length(days_sampled),
      EST_EFF_NBDAYS=length(unique(days)),
      EST_EFF_SRVTYPE=as.integer(unique(effort_type)),
      EST_EFF_APPROACH=ifelse(EST_EFF_SRVTYPE==7,"WEEKLY","To DO"),
      EST_EFF_ACTDAYS=sum(days_sampled,na.rm = T),
      EST_EFF_EXDAYS=EST_EFF_NSMP*EST_EFF_SRVTYPE,
      EST_EFF_PBA=EST_EFF_ACTDAYS/EST_EFF_EXDAYS,
      EST_EFF_NACT=unique(NB_ACTIVE_DAYS),
      EST_EFF_NBOATS=unique(BG),
      EST_EFF_POP=EST_EFF_NBOATS*EST_EFF_NACT,
      EST_EFF_EFFORT=EST_EFF_PBA*EST_EFF_NBOATS*EST_EFF_NACT,
      mean=mean(days_sampled,na.rm=T),
      sd=sd(days_sampled,na.rm=T),
      se=sd/sqrt(EST_EFF_NSMP),
      EST_EFF_CV=se/mean,
      EST_EFF_SPAACCUR=artfish_accuracy(n=EST_EFF_NSMP,N=EST_EFF_NBOATS*4,method="higher"),
      EST_EFF_TMPACCUR=1,
      EST_EFF_SUI=unif_index(days)
    )%>%
    ungroup()%>%
    left_join(fishing_units, by="EST_BGC")%>%
    select(EST_YEAR,
           EST_MONTH,
           EST_BGC,
           EST_BGC_NAME,
           EST_EFF_NBOATS,
           EST_EFF_NACT,
           EST_EFF_NBDAYS,
           EST_EFF_ACTDAYS,
           EST_EFF_EXDAYS,
           EST_EFF_PBA,
           EST_EFF_NSMP,
           EST_EFF_CV,
           EST_EFF_SUI,
           EST_EFF_SRVTYPE,
           EST_EFF_APPROACH,
           EST_EFF_SPAACCUR,
           EST_EFF_TMPACCUR,
           EST_EFF_POP,
           EST_EFF_EFFORT
           )

 ####Landing
 
 landing<-data_landing%>%
   dplyr::rename(EST_YEAR=year,
          EST_MONTH=month,
          EST_BGC=fishing_unit)%>%
   mutate(EST_BGC=as.character(EST_BGC))%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC,days,id)%>%
   dplyr::summarise(quantity=sum(quantity,na.rm = T),value=sum(value,na.rm=T),price=mean(price,na.rm=T))%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
   dplyr::mutate(quantity = replace(quantity,is.na(quantity), 0))%>%
   dplyr::summarise(
     EST_LND_NDAYS=length(unique(days)),
     EST_LND_SMPCATCH=sum(quantity),
     EST_LND_NSMP=length(quantity),
     EST_LND_CPUE_G=EST_LND_SMPCATCH/EST_LND_NSMP,
     sd=sd(quantity,na.rm=T),
     se=sd/sqrt(EST_LND_NSMP),
     EST_LND_CV=se/EST_LND_CPUE_G,
     EST_LND_SUI=unif_index(days)
   )%>%
  ungroup()%>%
  left_join(fishing_units, by="EST_BGC")%>%
  select(
    EST_YEAR,
    EST_MONTH,
    EST_BGC,
    EST_BGC_NAME,
    EST_YEAR,
    EST_LND_NDAYS,
    EST_LND_SMPCATCH,
    EST_LND_NSMP,
    EST_LND_CPUE_G,
    EST_LND_CV,
    EST_LND_SUI
  )
 
 estimate<-effort%>%
   left_join(landing)%>%
   mutate(EST_BGC=as.character(EST_BGC))%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
   dplyr::mutate(
     EST_LND_CATCH_G=EST_EFF_EFFORT*EST_LND_CPUE_G,
     EST_LND_SPAACCUR=artfish_accuracy(n=EST_LND_NSMP,N=EST_EFF_POP,method="higher"),
     EST_LND_TMPACCUR=artfish_accuracy(n=EST_LND_NDAYS,N=EST_EFF_NACT,method="higher"),
     EST_ACCUR=min(EST_EFF_SPAACCUR,EST_EFF_TMPACCUR,EST_LND_SPAACCUR,EST_LND_TMPACCUR,na.rm=T)
   )
 
 estimate<-data_landing%>%
   dplyr::rename(EST_YEAR=year,
          EST_MONTH=month,
          EST_BGC=fishing_unit,
          EST_SPC=species)%>%
   mutate(EST_SPC=as.character(EST_SPC))%>%
   mutate(EST_BGC=as.character(EST_BGC))%>%
   left_join(ref_species, by="EST_SPC")%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
   filter(!is.na(EST_SPC))%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_SPC,EST_SPC_NAME)%>%
   dplyr::summarise(n=sum(quantity),EST_LND_NOFISH=sum(number),EST_LND_PRICE=mean(price))%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
   dplyr::mutate(sum=sum(n),ratio=n/sum,EST_NOSPE=length(unique(EST_SPC)))%>%
   select(-n,-sum)%>%
   left_join(estimate)%>%
   dplyr::mutate(EST_LND_CPUE=EST_LND_CPUE_G*ratio,
          EST_LND_CATCH=EST_EFF_EFFORT*EST_LND_CPUE,
          EST_LND_VALUE=EST_LND_CATCH*EST_LND_PRICE,
          EST_LND_AVW=EST_LND_CATCH/EST_LND_NOFISH)%>%
   select(-ratio,-EST_LND_CPUE_G)


 return(estimate)
}

###Artfish estimates
artfish_estimates_by_fleet_segment<-function(con,year=NULL,month=NULL,data_effort=NULL,data_landing=NULL){
  
  if(is.null(data_effort))data_effort=accessEffortDataByFleetSegment(con,year,month)
  if(is.null(data_landing))data_landing=accessLandingDataByFleetSegment(con,year,month)
  
  print(data_effort)
  
  fishing_units<-accessFishingUnits(con)
  fishing_units<-subset(fishing_units,select=c(code,label))
  names(fishing_units)<-c("EST_BGC","EST_BGC_NAME")
  
  ref_species<-accessRefSpecies(con)
  ref_species$Species<-setNames(sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME),ref_species$ID)
  ref_species$ID<-as.character(ref_species$ID)
  ref_species<-subset(ref_species,select=c(ID,Species))
  names(ref_species)<-c("EST_SPC","EST_SPC_NAME")
  
  
  effort<-data_effort%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit,
                  EST_FS=fleet_segment)%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_FS)%>%
    dplyr::summarise(
      EST_EFF_NSMP=length(days_sampled),
      EST_EFF_NBDAYS=length(unique(days)),
      EST_EFF_SRVTYPE=as.integer(unique(effort_type)),
      EST_EFF_APPROACH=ifelse(EST_EFF_SRVTYPE==7,"WEEKLY","To DO"),
      EST_EFF_ACTDAYS=sum(days_sampled,na.rm = T),
      EST_EFF_EXDAYS=EST_EFF_NSMP*EST_EFF_SRVTYPE,
      EST_EFF_PBA=EST_EFF_ACTDAYS/EST_EFF_EXDAYS,
      EST_EFF_NACT=unique(NB_ACTIVE_DAYS),
      EST_EFF_NBOATS=unique(BG),
      EST_EFF_POP=EST_EFF_NBOATS*EST_EFF_NACT,
      EST_EFF_EFFORT=EST_EFF_PBA*EST_EFF_NBOATS*EST_EFF_NACT,
      mean=mean(days_sampled,na.rm=T),
      sd=sd(days_sampled,na.rm=T),
      se=sd/sqrt(EST_EFF_NSMP),
      EST_EFF_CV=se/mean,
      EST_EFF_SPAACCUR=artfish_accuracy(n=EST_EFF_NSMP,N=EST_EFF_NBOATS*4,method="higher"),
      EST_EFF_TMPACCUR=1,
      EST_EFF_SUI=unif_index(days)
    )%>%
    ungroup()%>%
    left_join(fishing_units, by="EST_BGC")%>%
    select(EST_YEAR,
           EST_MONTH,
           EST_BGC,
           EST_FS,
           EST_EFF_NBOATS,
           EST_EFF_NACT,
           EST_EFF_NBDAYS,
           EST_EFF_ACTDAYS,
           EST_EFF_EXDAYS,
           EST_EFF_PBA,
           EST_EFF_NSMP,
           EST_EFF_CV,
           EST_EFF_SUI,
           EST_EFF_SRVTYPE,
           EST_EFF_APPROACH,
           EST_EFF_SPAACCUR,
           EST_EFF_TMPACCUR,
           EST_EFF_POP,
           EST_EFF_EFFORT
    )
  
  ####Landing
  
  landing<-data_landing%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit,
                  EST_FS=fleet_segment)%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_FS,days,id)%>%
    dplyr::summarise(quantity=sum(quantity,na.rm = T),value=sum(value,na.rm=T),price=mean(price,na.rm=T))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_FS)%>%
    dplyr::mutate(quantity = replace(quantity,is.na(quantity), 0))%>%
    dplyr::summarise(
      EST_LND_NDAYS=length(unique(days)),
      EST_LND_SMPCATCH=sum(quantity),
      EST_LND_NSMP=length(quantity),
      EST_LND_CPUE_G=EST_LND_SMPCATCH/EST_LND_NSMP,
      sd=sd(quantity,na.rm=T),
      se=sd/sqrt(EST_LND_NSMP),
      EST_LND_CV=se/EST_LND_CPUE_G,
      EST_LND_SUI=unif_index(days)
    )%>%
    ungroup()%>%
    left_join(fishing_units, by="EST_BGC")%>%
    select(
      EST_YEAR,
      EST_MONTH,
      EST_BGC,
      EST_FS,
      EST_YEAR,
      EST_LND_NDAYS,
      EST_LND_SMPCATCH,
      EST_LND_NSMP,
      EST_LND_CPUE_G,
      EST_LND_CV,
      EST_LND_SUI
    )
  
  estimate<-effort%>%
    left_join(landing)%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_FS)%>%
    dplyr::mutate(
      EST_LND_CATCH_G=EST_EFF_EFFORT*EST_LND_CPUE_G,
      EST_LND_SPAACCUR=artfish_accuracy(n=EST_LND_NSMP,N=EST_EFF_POP,method="higher"),
      EST_LND_TMPACCUR=artfish_accuracy(n=EST_LND_NDAYS,N=EST_EFF_NACT,method="higher"),
      EST_ACCUR=min(EST_EFF_SPAACCUR,EST_EFF_TMPACCUR,EST_LND_SPAACCUR,EST_LND_TMPACCUR,na.rm=T)
    )
  
  estimate<-data_landing%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit,
                  EST_FS=fleet_segment,
                  EST_SPC=species)%>%
    mutate(EST_SPC=as.character(EST_SPC))%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    left_join(ref_species, by="EST_SPC")%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_FS)%>%
    filter(!is.na(EST_SPC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_FS,EST_SPC)%>%
    dplyr::summarise(n=sum(quantity),EST_LND_NOFISH=sum(number),EST_LND_PRICE=mean(price))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_FS)%>%
    dplyr::mutate(sum=sum(n),ratio=n/sum,EST_NOSPE=length(unique(EST_SPC)))%>%
    select(-n,-sum)%>%
    left_join(estimate)%>%
    dplyr::mutate(EST_LND_CPUE=EST_LND_CPUE_G*ratio,
                  EST_LND_CATCH=EST_EFF_EFFORT*EST_LND_CPUE,
                  EST_LND_VALUE=EST_LND_CATCH*EST_LND_PRICE,
                  EST_LND_AVW=EST_LND_CATCH/EST_LND_NOFISH)%>%
    select(-ratio,-EST_LND_CPUE_G)
  
  
  return(estimate)
}

###Artfish year summary
artfish_year_summary<-function(data,year=NULL,variable,value,levels=NULL){
  
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
    dplyr::summarise(!!value:=sum(!!sym(value),na.rm=T))%>%
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

