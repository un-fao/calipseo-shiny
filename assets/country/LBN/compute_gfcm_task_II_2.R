###compute_gfcm_task_II_2
compute_gfcm_task_II_2<-function(con=NULL,year=NULL,data,ref_species){
  
  data<-data%>%
    group_by(year,fleet_segment,species)%>%
    summarise(TOTAL_LANDING= round(sum(catch_fleet_segment,na.rm=T)/1000,2),
              TOTAL_DISCARDS = 0,
              TOTAL_CATCH = round(sum(catch_fleet_segment,na.rm=T)/1000,2)
              )%>%
    ungroup()%>%
    mutate(COMMENT="",
           GSA="27",
           species=as.character(species))%>%
    left_join(ref_species%>%select(ID,ASFIS_CODE)%>%mutate(ID=as.character(ID))%>%rename(species=ID))%>%
    rename(FLEET_SEGMENT=fleet_segment,
           SPECIES=ASFIS_CODE)%>%
    select(GSA,FLEET_SEGMENT,SPECIES,TOTAL_LANDING,TOTAL_DISCARDS,TOTAL_CATCH,COMMENT)
  
  return(data)
  
}