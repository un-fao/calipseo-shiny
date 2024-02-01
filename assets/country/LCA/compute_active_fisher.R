###compute_active_fisher
compute_active_fisher<-function(con=NULL,year=NULL,month=NULL){
    
    ind_info <- accessIndividualInfo(con)
    ind_info <- ind_info[!names(ind_info)%in%c("First_name","Middle_name","Suffix_name","Salutations","FisherID")]

    country_params<-accessCountryParam(con)

     is_fisher_active_query<-subset(country_params,CODE=="ISFISHERACTIVE")$TEXT

    is_fisher_active_query<-gsub("NOW()",sprintf("'%s-%02d-01'",year,ifelse(is.null(month),"01",as.numeric(month))),is_fisher_active_query,fixed=T)

    if(length(is_fisher_active_query)>0){
      is_fisher_active_table<-suppressWarnings(dbGetQuery(con, is_fisher_active_query))
      names(is_fisher_active_table)<-c("ID","Active")
      is_fisher_active_table$Active[is_fisher_active_table$Active==0]<-"Fisher Non Active"
      is_fisher_active_table$Active[is_fisher_active_table$Active==1]<-"Fisher Active"
      is_fisher_active_table$Active[is.na(is_fisher_active_table$Active)]<-"Non Fisher"
      ind_info<-merge(ind_info,is_fisher_active_table)
    }else{
      ind_info$Active<-NA
    }

    ind_info<-subset(ind_info,Active=="Fisher Active")

  data<-ind_info

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
    mutate(Age=round(time_length(interval(DOB,as.Date(sprintf("%s-%02d-01",year,ifelse(is.null(month),"12",as.numeric(month))))),"years"),0))%>%
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