###compute_active_vessel
compute_active_vessel<-function(con=NULL,year=NULL,month=NULL){
  
  vessel_info <- accessVesselInfo(con)
  
  country_params<-accessCountryParam(con)
  
  is_vessel_active_query<-subset(country_params,CODE=="ISVESSELACTIVE")$TEXT
  
  is_vessel_active_query<-gsub("NOW()",sprintf("'%s-%02d-01'",year,ifelse(is.null(month),"01",as.numeric(month))),is_vessel_active_query,fixed=T)
  
  if(length(is_vessel_active_query)>0){
    is_vessel_active_table<-suppressWarnings(dbGetQuery(pool, is_vessel_active_query))
    names(is_vessel_active_table)<-c("ID","Active")
    is_vessel_active_table$Active[is_vessel_active_table$Active==0]<-"Vessel Non Active"
    is_vessel_active_table$Active[is_vessel_active_table$Active==1]<-"Vessel Active"
    vessel_info<-merge(vessel_info,is_vessel_active_table)
  }else{
    vessel_info$Active<-NA
  }
  
  vessel_info<-subset(vessel_info,Active=="Vessel Active")
  
  data<-vessel_info
  
  colVariables<-c()
  
  if(!all(is.na(vessel_info$Active))){
    colVariables<-c(colVariables,c("Active"="Active"))
    vessel_info$Active[is.na(vessel_info$Active)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$OPERATIONAL_STATUS))){
    colVariables<-c(colVariables,c("OPERATIONAL_STATUS"="Operational Status"))
    vessel_info$OPERATIONAL_STATUS[is.na(vessel_info$OPERATIONAL_STATUS)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$VESSEL_TYPE))){
    colVariables<-c(colVariables,c("VESSEL_TYPE"="Vessel Type"))
    vessel_info$VESSEL_TYPE[is.na(vessel_info$VESSEL_TYPE)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$HOME_PORT_LANDING_SITE))){
    colVariables<-c(colVariables,c("HOME_PORT_LANDING_SITE"="Home Port"))
    vessel_info$HOME_PORT_LANDING_SITE[is.na(vessel_info$HOME_PORT)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$REG_PORT_LANDING_SITE))){
    colVariables<-c(colVariables,c("REG_PORT_LANDING_SITE"="Registration Port"))
    vessel_info$REG_PORT_LANDING_SITE[is.na(vessel_info$REG_PORT_LANDING_SITE)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$MANUFACTURER))){
    colVariables<-c(colVariables,c("MANUFACTURER"="Manufacturer"))
    vessel_info$MANUFACTURER[is.na(vessel_info$MANUFACTURER)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$ENGINE_TYPE))){
    colVariables<-c(colVariables,c("ENGINE_TYPE"="Engine Type"))
    vessel_info$ENGINE_TYPE[is.na(vessel_info$ENGINE_TYPE)] <- "Unknown"
  }
  
  if(!all(is.na(vessel_info$ENERGY_TYPE))){
    colVariables<-c(colVariables,c("ENERGY_TYPE"="Energy type"))
    vessel_info$ENERGY_TYPE[is.na(vessel_info$ENERGY_TYPE)] <- "Unknown"
  }
  
  colVariables2<-setNames(names(colVariables),colVariables)
  
  data<-data%>%
    arrange(REGISTRATION_NUMBER,MANUFACTURER,ENGINE_TYPE,ENERGY_TYPE)%>%
    group_by(REGISTRATION_NUMBER) %>%
    mutate(MANUFACTURE=paste0(unique(MANUFACTURER),collapse = "+"),
           ENGINE_TYPE=paste0(unique(ENGINE_TYPE),collapse = "+"),
           ENERGY_TYPE=paste0(unique(ENERGY_TYPE),collapse = "+"))%>%
    ungroup()%>%
    distinct()%>%
    mutate(value=1)%>%
    group_by_at(names(colVariables))%>%
    summarise(Number=sum(value))%>%
    ungroup()%>%
    rename(all_of(colVariables2))%>%
    select(-Active)
  
 return(data)  
  
}