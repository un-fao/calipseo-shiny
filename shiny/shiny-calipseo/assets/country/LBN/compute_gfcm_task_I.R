###compute_gfcm_task_I
compute_gfcm_task_I<-function(data,socio){
  
  names(socio)<-gsub(" ","_",names(socio))
  socio<-socio[,c("Vessel_register_number","Net_Tonnage","HP")]
  
  socio<-socio%>%
    summarise(NB_VESSELS=round(length(unique(Vessel_register_number)),0),
              TOTAL_CAPACITY=round(sum(as.numeric(Net_Tonnage),na.rm=T),0),
              TOTAL_ENGINE_POWER=round(sum(as.numeric(HP),na.rm=T),0))
  
  data<-data%>%
    group_by(EST_YEAR)%>%
    summarise(EST_LND_CATCH= sum(EST_LND_CATCH,na.rm=T)/1000)%>%
    mutate(COMMENT="",
           GSA="27",
           EST_LND_CATCH=round(EST_LND_CATCH,0))%>%
    rename(TOTAL_LANDINGS=EST_LND_CATCH,
           YEAR=EST_YEAR)%>%
    ungroup()%>%
    bind_cols(socio)%>%
    select(YEAR,NB_VESSELS,TOTAL_CAPACITY,TOTAL_LANDINGS,TOTAL_ENGINE_POWER,COMMENT)
  
  return(data)
  
}