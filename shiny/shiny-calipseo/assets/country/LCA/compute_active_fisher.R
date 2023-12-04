###compute_active_fisher
compute_active_fisher<-function(data){
  
  colVariables<-c()
  
  if(!all(is.na(data$Gender))){
    colVariables<-c(colVariables,c("Gender"="Gender"))
    data$Gender[is.na(data$Gender)] <- "Unknown"
  }
  
  if(!all(is.na(data$Education))){
    data$Education[is.na(data$Education)] <- "Unknown"
  }
  
  if(!all(is.na(data$Worktime))){
    data$Worktime[is.na(data$Worktime)] <- "Unknown"
  }
  
  # if(!all(is.na(data$License))){
  #   data$License[is.na(data$License)] <- "Unknown"
  # }
  
  if(!all(is.na(data$Role))){
    data$Role[is.na(data$Role)] <- "Unknown"
  }
  
  if(!all(is.na(data$Site))){
    data$Site[is.na(data$Site)] <- "Unknown"
  }
  
  data<-data%>%
    mutate(Age=round(time_length(interval(DOB,Sys.Date()),"years"),0))%>%
    select(-DOB,-Regdate)%>%
    arrange(ID,Role,Site)%>%
    group_by(ID) %>%
    mutate(Role=paste0(unique(Role),collapse = "+"),
           Site=paste0(unique(Site),collapse = "+"))%>%
    ungroup()%>%
    distinct()%>%
    mutate(value=1)%>%
    group_by(Site,Gender,Age,Education,Role,Worktime)%>%
    summarise(Number=sum(value))%>%
    ungroup()
  
  return(data)
  
}