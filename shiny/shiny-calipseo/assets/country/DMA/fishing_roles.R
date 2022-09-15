fishing_roles <- function(){
  ind <- accessIndividualDetails(pool)
  return( unique(ind$FSH_ROLE[!is.na(ind$FSH_ROLE)]))
}


ind_roles_filter <- function(data, input=NULL, grouping = TRUE){
  if(isTRUE(grouping)){
    ifelse((is.null(input) || input == "All"),
           data <- data %>% group_by(NAME) %>% count(name = 'COUNT'),
           data <- data %>% filter(FSH_ROLE%in%input) %>% group_by(NAME) %>% count(name = 'COUNT'))
  }else{
    ifelse((is.null(input) || input == "All"),
           data,
           data <- data %>% filter(FSH_ROLE%in%input)) 
  }
  return(data) 
}