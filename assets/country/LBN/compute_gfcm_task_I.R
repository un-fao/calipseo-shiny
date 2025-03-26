###compute_gfcm_task_I
compute_gfcm_task_I<-function(con=NULL,year=NULL,data,socio){
  
  names(socio)<-gsub(" ","_",names(socio))
  socio<-socio[,c("Vessel_register_number","Net_Tonnage","HP")]
  
  socio<-socio%>%
    summarise(NB_VESSELS=round(length(unique(Vessel_register_number)),0),
              TOTAL_CAPACITY=round(sum(as.numeric(Net_Tonnage),na.rm=T),0),
              TOTAL_ENGINE_POWER=round(sum(as.numeric(HP),na.rm=T),0))
  
  data<-data%>%
    group_by(year)%>%
    summarise(catch_nominal_landed= sum(catch_nominal_landed,na.rm=T)/1000)%>%
    mutate(COMMENT="",
           GSA="27",
           catch_nominal_landed=round(catch_nominal_landed,0))%>%
    rename(TOTAL_LANDINGS=catch_nominal_landed,
           YEAR=year)%>%
    ungroup()%>%
    bind_cols(socio)%>%
    select(YEAR,NB_VESSELS,TOTAL_CAPACITY,TOTAL_LANDINGS,TOTAL_ENGINE_POWER,COMMENT)
  
  return(data)
  
}