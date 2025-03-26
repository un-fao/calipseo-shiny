artfish_estimates_b<-function(con,year=NULL,month=NULL,data_effort=NULL,data_landing=NULL){

  #Access to countries_param
  country_params<-accessCountryParam(con)

  #Check type of survey 
  survey_type<-subset(country_params,CODE=="EFFSURVTYPE")$CL_CODE_ID

  switch(survey_type,
    "1"={},
    "2"={
         
         
         if(is.null(data_effort))data_effort=accessEffortData(con,year,month)
         if(is.null(data_landing))data_landing=accessLandingData(con,year,month)
         
         effort<-data_effort%>%
           dplyr::rename(EST_YEAR=year,
                         EST_MONTH=month,
                         EST_BGC=fishing_unit)%>%
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
           select(EST_YEAR,
                  EST_MONTH,
                  EST_BGC,
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
           select(
             EST_YEAR,
             EST_MONTH,
             EST_BGC,
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
           group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
           filter(!is.na(EST_SPC))%>%
           group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_SPC)%>%
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
         
       },
    "3"={},
    "4"={},
    "5"={
      
  if(is.null(data_landing))data_landing=accessLandingData(con,year,month)
  
  vessels_list <- accessVesselsLandingSite(con)
  
  is_vessel_active_query<-subset(country_params,CODE=="ISVESSELACTIVE")$TEXT
  is_vessel_active_query<-gsub("NOW()",sprintf("'%s-12-31'",year),is_vessel_active_query,fixed=T)
  is_vessel_active_table<-suppressWarnings(dbGetQuery(con, is_vessel_active_query))
  names(is_vessel_active_table)<-c("ID","Active")
  vessels_list<-merge(vessels_list,is_vessel_active_table)
  
  vessels_list<-vessels_list%>%
    filter(Active==1)%>%
    group_by(LANDING_SITE_ID)%>%
    summarise(NB_ACT_BOAT=length(unique(ID)))%>%
    ungroup()
  
  #vessels_list<-read.csv("D:/Calipseo/TEST/vessels_list_test.csv")
    
  fishing_days_per_month<-accessFishingDaysPerMonth(con)
  
  #fishing_days_per_month<-read.csv("D:/Calipseo/TEST/dt_fishing_days_per_month_test.csv")
  
  print(sprintf("Selected year : %s ; selected month : %s",year,month))
  
  effort_A<-fishing_days_per_month%>%
    filter(YEAR==year,MONTH==month)%>%
     group_by(YEAR,MONTH,LANDING_SITE_ID)%>%
     summarise(SUM_FISHING_DAYS=sum(FISHING_DAY),
               DAYS=max(DAYS))%>%
     group_by(YEAR,MONTH)%>%
    summarise(EST_EFF_NBDAYS=max(DAYS),
              EST_EFF_NSMP=length(unique(LANDING_SITE_ID)),
              EST_EFF_ACTDAYS=sum(SUM_FISHING_DAYS),
              EST_EFF_EXDAYS=EST_EFF_NSMP*EST_EFF_NBDAYS,
              EST_EFF_NACT=EST_EFF_ACTDAYS/EST_EFF_EXDAYS*unique(EST_EFF_NBDAYS),
              EST_EFF_SD=sd(SUM_FISHING_DAYS))%>%
    ungroup()
    
  effort_survey<-accessEffortSurvey(con)
  
  #effort_survey<-read.csv("D:/Calipseo/TEST/dt_effort_survey_test.csv")
  
  effort_BAC<-effort_survey%>%
    filter(YEAR==year,MONTH==month)%>%
    left_join(vessels_list%>%select(LANDING_SITE_ID,NB_ACT_BOAT),by="LANDING_SITE_ID")%>%
    rowwise()%>%
    mutate(prop=NB_ACTIVE_FISHING_UNITS/NB_ACT_BOAT)%>%
    group_by(YEAR,MONTH,FISHING_UNIT_ID)%>%
    summarise(
      EST_EFF_NACTFU=length(NB_ACTIVE_FISHING_UNITS),
      EST_EFF_BAC=mean(prop,na.rm=T),
      EST_EFF_EFFORT_SD=sd(prop,na.rm=T)
    )%>%
    ungroup()
  
   effort<-effort_A%>%
     left_join(effort_BAC)%>%
     mutate(EST_EFF_NBOATS=sum(vessels_list$NB_ACT_BOAT))%>%
     rowwise()%>%
     mutate(EST_EFF_EFFORT=EST_EFF_BAC*EST_EFF_NBOATS*EST_EFF_NACT,
            EST_EFF_SIGMA_EFFORT=EST_EFF_EFFORT*sqrt((EST_EFF_EFFORT_SD/EST_EFF_BAC)^2+(EST_EFF_SD/EST_EFF_NACT)^2),
            EST_EFF_IC_MINUS=EST_EFF_EFFORT-1.96*EST_EFF_SIGMA_EFFORT/EST_EFF_NACTFU,
            EST_EFF_IC_PLUS=EST_EFF_EFFORT+1.96*EST_EFF_SIGMA_EFFORT/EST_EFF_NACTFU
            )%>%
     ungroup()%>%
     dplyr::rename(EST_YEAR=YEAR,
                   EST_MONTH=MONTH,
                   EST_BGC=FISHING_UNIT_ID)
   
  # #Method based of LBN artfish indicators

  # 
  # effort_A<-fishing_days_per_month%>%
  #   group_by(YEAR,MONTH)%>%
  #   summarise(EST_EFF_NBDAYS=max(DAYS),
  #             EST_EFF_NSMP=length(unique(LANDING_SITE_ID)),
  #             EST_EFF_ACTDAYS=sum(FISHING_DAY),
  #             EST_EFF_EXDAYS=EST_EFF_NSMP*EST_EFF_NBDAYS,
  #             EST_EFF_NACT=EST_EFF_ACTDAYS/EST_EFF_EXDAYS*unique(EST_EFF_NBDAYS),
  #             mean=mean(FISHING_DAY,na.rm=T),
  #             sd=sd(FISHING_DAY,na.rm=T),
  #             se=sd/sqrt(EST_EFF_NSMP),
  #             EST_EFF_CV=se/mean,
  #             EST_EFF_SUI=unif_index(DAYS))%>%
  #   ungroup()
  # 
  # 
  #   
  # effort_BAC<-effort_survey%>%
  #   left_join(vessels_list%>%select(LANDING_SITE_ID,NB_ACT_BOAT),by="LANDING_SITE_ID")%>%
  #   rowwise()%>%
  #   mutate(prop=NB_ACTIVE_FISHING_UNITS/NB_ACT_BOAT)%>%
  #   group_by(YEAR,MONTH,FISHING_UNIT_ID)%>%
  #   summarise(
  #     EST_EFF_PBA=mean(prop,na.rm=T),
  #     sd=sd(prop,na.rm=T)
  #   )%>%
  #       ungroup()
  # 
  # effort<-effort_BAC%>%
  #   left_join(effort_A,by=c("YEAR","MONTH"))%>%
  #   mutate(
  #     EST_EFF_NBOATS=sum(vessels_list$NB_ACT_BOAT),
  #     EST_EFF_POP=EST_EFF_NBOATS*EST_EFF_NACT)%>%
  #   rowwise()%>%
  #   mutate(EST_EFF_EFFORT=EST_EFF_PBA*EST_EFF_NBOATS*EST_EFF_NACT,
  #          EST_EFF_SPAACCUR=artfish_accuracy(n=EST_EFF_NSMP,N=EST_EFF_NBOATS*4,method="higher"),
  #          EST_EFF_TMPACCUR=1
  #   )%>%
  #   ungroup()%>%
  #   rename(EST_YEAR=YEAR,
  #          EST_MONTH=MONTH,
  #          EST_BGC=FISHING_UNIT_ID)%>%
  #       select(EST_YEAR,
  #              EST_MONTH,
  #              EST_BGC,
  #              EST_EFF_NBOATS,
  #              EST_EFF_NACT,
  #              EST_EFF_NBDAYS,
  #              EST_EFF_ACTDAYS,
  #              EST_EFF_EXDAYS,
  #              EST_EFF_PBA,
  #              EST_EFF_NSMP,
  #              EST_EFF_CV,
  #              EST_EFF_SUI,
  #              #EST_EFF_SRVTYPE,
  #              #EST_EFF_APPROACH,
  #              EST_EFF_SPAACCUR,
  #              EST_EFF_TMPACCUR,
  #              EST_EFF_POP,
  #              EST_EFF_EFFORT
  #       )
  # #end
  
   
   # fishing_trip<-read.csv("D:/Calipseo/TEST/dt_fishing_trip_test.csv")
   # fishing_activities<-read.csv("D:/Calipseo/TEST/dt_fishing_activities_test.csv")
   # fishing_activities_species<-read.csv("D:/Calipseo/TEST/dt_fishing_activities_species_test.csv")
   # 
   # data_landing<-fishing_activities%>%
   #   select(ID,DT_FISHING_TRIP_ID)%>%
   #   left_join(   fishing_activities_species%>%
   #                  select(DT_FISHING_ACTIVITY_ID, CL_REF_SPECIES_ID,
   #                         QUANTITY,
   #                         TOTAL_VALUE,
   #                         CATCH_NUMBER,
   #                         PRICE_PER_UNIT_CATCH),by=c("ID" = "DT_FISHING_ACTIVITY_ID"))%>%
   #   select(-ID)%>%
   #   left_join(   fishing_trip%>%
   #                  select(ID, DATE_FROM,CL_FISH_FISHING_UNIT_ID)%>%
   #                  mutate(DATE_FROM=as.Date(gsub("'","",DATE_FROM))),by=c("DT_FISHING_TRIP_ID"="ID"))%>%
   #   rowwise()%>%
   #   summarise(
   #          id=DT_FISHING_TRIP_ID,
   #          year=year(DATE_FROM),
   #          month=month(DATE_FROM),
   #          days=day(DATE_FROM),
   #          fishing_unit=CL_FISH_FISHING_UNIT_ID,
   #          species=CL_REF_SPECIES_ID,
   #          quantity=as.numeric(gsub(",",".",ifelse(QUANTITY=="NULL",NA,QUANTITY))),
   #          value=as.numeric(gsub(",",".",ifelse(TOTAL_VALUE=="NULL",NA,TOTAL_VALUE))),
   #          number=as.numeric(gsub(",",".",ifelse(CATCH_NUMBER=="NULL",NA,CATCH_NUMBER))),
   #          price=as.numeric(gsub(",",".",ifelse(PRICE_PER_UNIT_CATCH=="NULL",NA,PRICE_PER_UNIT_CATCH)))
   #          )%>%
   #   ungroup()
  
  landing<-data_landing%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit)%>%
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
    select(
      EST_YEAR,
      EST_MONTH,
      EST_BGC,
      EST_YEAR,
      EST_LND_NDAYS,
      EST_LND_SMPCATCH,
      EST_LND_NSMP,
      EST_LND_CPUE_G,
      EST_LND_CV,
      EST_LND_SUI
    )%>%
    ungroup()
  
  #print(landing)
  
  estimate<-effort%>%
    left_join(landing)%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::mutate(
      EST_LND_CATCH_G=EST_EFF_EFFORT*EST_LND_CPUE_G
      # EST_LND_SPAACCUR=artfish_accuracy(n=EST_LND_NSMP,N=EST_EFF_POP,method="higher"),
      # EST_LND_TMPACCUR=artfish_accuracy(n=EST_LND_NDAYS,N=EST_EFF_NACT,method="higher"),
      # EST_ACCUR=min(EST_LND_SPAACCUR,EST_LND_TMPACCUR,na.rm=T)
    )%>%
    ungroup()
  

  
  
  #print(estimate)
  
  estimate<-data_landing%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit,
                  EST_SPC=species)%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    filter(!is.na(EST_SPC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_SPC)%>%
    dplyr::summarise(n=sum(quantity,na.rm = T),EST_LND_NOFISH=sum(number,na.rm = T),EST_LND_PRICE=mean(price,na.rm = T))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::mutate(sum=sum(n),ratio=n/sum,EST_NOSPE=length(unique(EST_SPC)))%>%
    select(-n,-sum)%>%
    left_join(estimate)%>%
    dplyr::mutate(EST_LND_CPUE=EST_LND_CPUE_G*ratio,
                  EST_LND_CATCH=EST_EFF_EFFORT*EST_LND_CPUE,
                  EST_LND_VALUE=EST_LND_CATCH*EST_LND_PRICE,
                  EST_LND_AVW=EST_LND_CATCH/EST_LND_NOFISH)%>%
    select(-ratio)
  

  
  return(estimate)
  
})

}