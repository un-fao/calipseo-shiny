###compute_fao
compute_fao<-function(con=NULL,year=NULL,data){
  
  data<-data%>%
    group_by(year)%>%
    summarise(EST_LND_CATCH= sum(catch_nominal_landed,na.rm=T)/1000)%>%
    mutate(COMMENT="",
           EST_LND_CATCH=round(catch_nominal_landed,0))%>%
    rename(TOTAL_LANDINGS=catch_nominal_landed,
           YEAR=year)%>%
    ungroup()%>%
    select(YEAR,TOTAL_LANDINGS,COMMENT)
  
  return(data)
  
}