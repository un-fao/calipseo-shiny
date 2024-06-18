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
  group_by(EST_YEAR,EST_FS)%>%
  #summarise(EST_EFF_NBOATS=sum(EST_EFF_NBOATS,na.rm=T),
  #          EST_LND_CATCH= sum(EST_LND_CATCH,na.rm=T))%>%
  summarise(EST_LND_CATCH= sum(EST_LND_CATCH,na.rm=T)/1000)%>%
  mutate(COMMENT="",
         GSA="27",
         EST_LND_CATCH=round(EST_LND_CATCH,0))%>%
  rename(FLEET_SEGMENT=EST_FS,
         #NB_VESSELS=EST_EFF_NBOATS,
         TOTAL_LANDINGS=EST_LND_CATCH)%>%
  left_join(socio)%>%
  select(GSA,FLEET_SEGMENT,NB_VESSELS,TOTAL_LANDINGS,COMMENT)%>%
  ungroup()

return(data)
      
}