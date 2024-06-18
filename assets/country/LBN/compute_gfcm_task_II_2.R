###compute_gfcm_task_II_2
compute_gfcm_task_II_2<-function(con=NULL,year=NULL,ref_species,data){
  
  data<-data%>%
    group_by(EST_YEAR,EST_FS,EST_SPC)%>%
    summarise(TOTAL_LANDING= round(sum(EST_LND_CATCH,na.rm=T)/1000,0),
              TOTAL_DISCARDS = 0,
              TOTAL_CATCH = round(sum(EST_LND_CATCH,na.rm=T)/1000,0)
              )%>%
    ungroup()%>%
    mutate(COMMENT="",
           GSA="27",
           EST_SPC=as.character(EST_SPC))%>%
    left_join(ref_species%>%select(ID,ASFIS_CODE)%>%mutate(ID=as.character(ID)),by=c('EST_SPC'='ID'))%>%
    rename(FLEET_SEGMENT=EST_FS,
           SPECIES=ASFIS_CODE)%>%
    select(GSA,FLEET_SEGMENT,SPECIES,TOTAL_LANDING,TOTAL_DISCARDS,TOTAL_CATCH,COMMENT)
  
  return(data)
  
}