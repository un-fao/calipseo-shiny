#Calculcate Artfish - Effort
#create function wrapper 
artfish_effort<-function(pool,year,month){
  
#retrieve countries_param
country_params<-accessCountryParam(pool)

#Check type of survey 
survey_type<-subset(country_params,CODE=="EFFSURVTYPE")$CL_CODE_ID

switch(survey_type,
       "1"={},
       "2"={"LBN case"},
       "3"={},
       "4"={},
       "5"={"LCA case"})

if(survey_type==5){
  
  vessels_list <- accessVesselsLandingSite(pool)
  
  is_vessel_active_query<-subset(country_params,CODE=="ISVESSELACTIVE")$TEXT
  
  #is_vessel_active_query<-gsub("NOW()",sprintf("'%s-12-31'",year),is_vessel_active_query,fixed=T)
  is_vessel_active_table<-suppressWarnings(dbGetQuery(pool, is_vessel_active_query))
  names(is_vessel_active_table)<-c("ID","Active")
  vessels_list<-merge(vessels_list,is_vessel_active_table)
  
  vessels_list<-vessels_list%>%
    filter(Active==1)%>%
    group_by(LANDING_SITE_ID)%>%
    summarise(NB_ACT_BOAT=length(unique(ID)))%>%
    ungroup()
    
  fishing_days_per_month<-accessFishingDaysPerMonth(pool)
  
  #fishing_days_per_month<-read.csv("D:/Calipseo/TEST/dt_fishing_days_per_month_test.csv")
  
  effort_A<-fishing_days_per_month%>%
    group_by(YEAR,MONTH)%>%
    mutate(EST_EFF_NBDAYS=max(DAYS),
           EST_EFF_NBSITE=length(unique(LANDING_SITE_ID)))%>%
    summarise(EST_EFF_ACTDAYS=sum(FISHING_DAY),
              EST_EFF_EXDAYS=unique(EST_EFF_NBDAYS*EST_EFF_NBSITE),
              EST_EFF_NACT=EST_EFF_ACTDAYS/EST_EFF_EXDAYS*unique(EST_EFF_NBDAYS))%>%
    ungroup()
    
  effort_survey<-accessEffortSurvey(pool)
  
  #effort_survey<-read.csv("D:/Calipseo/TEST/dt_effort_survey_test.csv")
  
  # effort_F<-effort_survey%>%
  #   select(YEAR,MONTH,LANDING_SITE_ID)%>%
  #   distinct()%>%
  #   left_join(vessels_list%>%select(LANDING_SITE_ID,NB_ACT_BOAT),by="LANDING_SITE_ID")%>%
  #   group_by(YEAR,MONTH)%>%
  #   summarise(EST_EFF_NBOATS=sum(NB_ACT_BOAT))%>%
  #   ungroup()
  
  effort_BAC<-effort_survey%>%
    left_join(vessels_list%>%select(LANDING_SITE_ID,NB_ACT_BOAT),by="LANDING_SITE_ID")%>%
    group_by(YEAR,MONTH,DAYS,LANDING_SITE_ID,NB_ACT_BOAT)%>%
    summarise(NB_ACT_FISH_UNIT=sum(NB_ACTIVE_FISHING_UNITS))%>%
    group_by(YEAR,MONTH)%>%
    summarise(EST_EFF_BAC=sum(NB_ACT_FISH_UNIT)/sum(NB_ACT_BOAT))%>%
    ungroup()
  
  effort<-effort_A%>%
    left_join(effort_BAC)%>%
    mutate(EST_EFF_NBOATS=sum(vessels_list$NB_ACT_BOAT))%>%
    rowwise()%>%
    mutate(EST_EFF_EFFORT=EST_EFF_BAC*EST_EFF_NBOATS*EST_EFF_NACT)%>%
    ungroup()
  
  effort_per_fu<-effort_survey%>%
    group_by(YEAR,MONTH,FISHING_UNIT_ID)%>%
    summarise(NB_ACT_FISH_UNIT=sum(NB_ACTIVE_FISHING_UNITS))%>%
    group_by(YEAR,MONTH)%>%
    mutate(RATE=NB_ACT_FISH_UNIT/sum(NB_ACT_FISH_UNIT))%>%
    ungroup()%>%
    left_join(effort,by=c("YEAR","MONTH"))%>%
    mutate(EST_EFF_EFFORT=RATE*EST_EFF_EFFORT)

}
}