###compute_gfcm_task_III
compute_gfcm_task_III<-function(data,vulnerable_sp){
  
  vulnerable_sp<-vulnerable_sp[,c("group","scientific_name")]
  names(vulnerable_sp)<-c("GROUP","SCIENTIFIC_NAME")
  
  data<-data%>%
    filter(SCIENTIFIC_NAME%in%vulnerable_sp$SCIENTIFIC_NAME)%>%
    rowwise()%>%
    mutate(COMMENT="",
           GSA="27",
           SOURCE="OT",
           NUMBER_ALIVE="",
           NUMBER_DEAD="",
           NUMBER_UNKNOWN="")%>%
    ungroup()%>%
    left_join(vulnerable_sp)%>%
    select(DATE,GSA,SOURCE,FLEET_SEGMENT,FISHING_GEAR,GROUP,FAMILY,SCIENTIFIC_NAME,TOTAL_NUMBER,TOTAL_LANDINGS,NUMBER_ALIVE,NUMBER_DEAD,NUMBER_UNKNOWN,COMMENT)%>%
    ungroup()
  
  return(data)
  
}