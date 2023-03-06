###compute_gfcm_task_II_1
compute_gfcm_task_II_1<-function(data){
  
data<-data%>%
  group_by(EST_YEAR,EST_FS)%>%
  summarise(EST_EFF_NBOATS=sum(EST_EFF_NBOATS,na.rm=T),
            EST_LND_CATCH= sum(EST_LND_CATCH,na.rm=T))%>%
  mutate(COMMENT="",
         GSA="27")%>%
  rename(FLEET_SEGMENT=EST_FS,
         NB_VESSELS=EST_EFF_NBOATS,
         TOTAL_LANDINGS=EST_LND_CATCH)%>%
  select(GSA,FLEET_SEGMENT,NB_VESSELS,TOTAL_LANDINGS,COMMENT)

return(data)
      
}