#Non-Probabilistic Temporal Accuracy
Tmp_Acc<-function(n,N){
  W=0.75*(1-1/N)
  a=(2*W*(N^2))/(N-1)^2-(N+1)/(N-1)
  g=a+(1-a)/N
  S=(1-a)*(1/log(N)-1/(N*log(N))-1/N)
  k=(-2/log(N))*log(S/(1-S-g))
  a2=(1-S-g)^2/(2*S+g-1)
  a1=g-a2
  x=log(n)/log(N)
  A=a1+a2*(N^(-k*x))
  
  return(A)
}

#Pessimistic Spatial Accuracy

Spa_Acc<-function(n,N){
  R=sqrt((2*N-1)/(6*(N-1))-1/4)
  A=1-1.96*(R/sqrt(n))*sqrt(1-n/N)
  
  return(A)
}

#uniformity index
unif_index<-function(days){
  table<-as.data.frame(table(days))
  mean=mean(table$Freq)
  table$ratio<-ifelse(table$Freq/mean>1,1,table$Freq/mean)
  index=round(sum(table$ratio),0)/nrow(table)
  return(index)
}

###Artfish estimates
artfish_estimates<-function(con,data_effort,data_landing){
  
  effort<-data_effort%>%
    rename(EST_YEAR=year,
           EST_MONTH=month,
           EST_BGC=fishing_unit)%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    summarise(
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
      EST_EFF_SPAACCUR=Spa_Acc(n=EST_EFF_NSMP,N=EST_EFF_NBOATS),
      EST_EFF_TMPACCUR=Tmp_Acc(n=EST_EFF_NBDAYS,N=EST_EFF_NACT),
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
   rename(EST_YEAR=year,
          EST_MONTH=month,
          EST_BGC=fishing_unit)%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC,days,id)%>%
   summarise(quantity=sum(quantity,na.rm = T),value=sum(value,na.rm=T),price=mean(price,na.rm=T))%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
   mutate(quantity = replace(quantity,is.na(quantity), 0))%>%
   summarise(
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
   mutate(
     EST_LND_CATCH_G=EST_EFF_EFFORT*EST_LND_CPUE_G,
     EST_LND_SPAACCUR=Spa_Acc(n=EST_LND_NSMP,N=EST_EFF_NBOATS),
     EST_LND_TMPACCUR=Tmp_Acc(n=EST_LND_NDAYS,N=EST_EFF_NACT),
     EST_ACCUR=min(EST_EFF_SPAACCUR,EST_EFF_TMPACCUR,EST_LND_SPAACCUR,EST_LND_TMPACCUR,na.rm=T)
   )
 
 estimate<-data_landing%>%
   rename(EST_YEAR=year,
          EST_MONTH=month,
          EST_BGC=fishing_unit,
          EST_SPC=species)%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
   filter(!is.na(EST_SPC))%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_SPC)%>%
   summarise(n=sum(quantity),EST_LND_NOFISH=sum(number),EST_LND_PRICE=mean(price))%>%
   group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
   mutate(sum=sum(n),ratio=n/sum,EST_NOSPE=length(unique(EST_SPC)))%>%
   select(-n,-sum)%>%
   left_join(estimate)%>%
   mutate(EST_LND_CPUE=EST_LND_CPUE_G*ratio,
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
    summarise(!!value:=sum(!!sym(value),na.rm=T))%>%
    mutate(EST_MONTH=sprintf('%02d',EST_MONTH),
           !!variable:= as.character(!!sym(variable))) %>%
    complete(nesting(!!sym(variable)),EST_MONTH=c("01","02","03","04","05","06","07","08","09","10","11","12"))%>%
               ungroup() %>% 
               bind_rows(group_by(.,!!sym(variable)) %>%
                           summarise(!!value:=sum(!!sym(value),na.rm=T)) %>%
                           mutate(EST_MONTH='Total')) %>%
               bind_rows(group_by(.,EST_MONTH) %>%
                           summarise(!!value:=sum(!!sym(value),na.rm=T)) %>%
                           mutate(!!variable:='Total')) %>%
               pivot_wider(names_from = EST_MONTH, values_from = !!sym(value))%>%
               select(!!sym(variable),`01`,`02`,`03`,`04`,`05`,`06`,`07`,`08`,`09`,`10`,`11`,`12`,"Total")
             
  rank<-summary%>%
    select(!!sym(variable),Total)%>%
    filter(!!sym(variable)!="Total")%>%
    mutate(rank=rank(-Total))%>%
    mutate(percent=Total/sum(Total))%>%
    arrange(-percent)%>%
    mutate(cum_percent=cumsum(percent))%>%
    select(!!sym(variable),rank,Total,percent,cum_percent)
             
  return(list(accuracy=accuracy,summary=summary,rank=rank))
             
}

