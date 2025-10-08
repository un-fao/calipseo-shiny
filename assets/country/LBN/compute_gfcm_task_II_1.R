###compute_gfcm_task_II_1
compute_gfcm_task_II_1<-function(con=NULL,year=NULL,data,socio){
  
  names(socio)<-gsub(" ","_",names(socio))
  socio<-socio[,c("Vessel_register_number","FISHING_SEGMENT_GFCM")]
  
socio<-socio%>%
  group_by(FISHING_SEGMENT_GFCM)%>%
  summarise(NB_VESSELS=length(unique(Vessel_register_number)))%>%
  ungroup()%>%
  rename(FLEET_SEGMENT=FISHING_SEGMENT_GFCM)
  
data<-data%>%
  group_by(year,fleet_segment)%>%
  #summarise(EST_EFF_NBOATS=sum(EST_EFF_NBOATS,na.rm=T),
  #          catch_nominal_landed= sum(catch_nominal_landed,na.rm=T))%>%
  summarise(catch_fleet_segment = sum(catch_fleet_segment,na.rm=T)/1000)%>%
  mutate(COMMENT="",
         GSA="27",
         catch_fleet_segment = round(catch_fleet_segment,2))%>%
  rename(FLEET_SEGMENT=fleet_segment,
         #NB_VESSELS=EST_EFF_NBOATS,
         TOTAL_LANDINGS=catch_fleet_segment)%>%
  left_join(socio)%>%
  select(GSA,FLEET_SEGMENT,NB_VESSELS,TOTAL_LANDINGS,COMMENT)%>%
  ungroup()

return(data)
      
}