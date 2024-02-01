###compute_fao
compute_fao<-function(data){
  
  data<-data%>%
    group_by(EST_YEAR)%>%
    summarise(EST_LND_CATCH= sum(EST_LND_CATCH,na.rm=T)/1000)%>%
    mutate(COMMENT="",
           EST_LND_CATCH=round(EST_LND_CATCH,0))%>%
    rename(TOTAL_LANDINGS=EST_LND_CATCH,
           YEAR=EST_YEAR)%>%
    ungroup()%>%
    select(YEAR,TOTAL_LANDINGS,COMMENT)
  
  return(data)
  
}