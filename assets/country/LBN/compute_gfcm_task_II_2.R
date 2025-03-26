###compute_gfcm_task_II_2
compute_gfcm_task_II_2<-function(con=NULL,year=NULL,ref_species,data){
  
  data<-data%>%
    group_by(year,EST_FS,species)%>%
    summarise(TOTAL_LANDING= round(sum(catch_nominal_landed,na.rm=T)/1000,0),
              TOTAL_DISCARDS = 0,
              TOTAL_CATCH = round(sum(catch_nominal_landed,na.rm=T)/1000,0)
              )%>%
    ungroup()%>%
    mutate(COMMENT="",
           GSA="27",
           species=as.character(species))%>%
    mutate(ref_species%>%select(ID,ASFIS_CODE)%>%mutate(ID=as.character(ID))%>%rename(species=ID))%>%
    rename(FLEET_SEGMENT=EST_FS,
           SPECIES=ASFIS_CODE)%>%
    select(GSA,FLEET_SEGMENT,SPECIES,TOTAL_LANDING,TOTAL_DISCARDS,TOTAL_CATCH,COMMENT)
  
  return(data)
  
}