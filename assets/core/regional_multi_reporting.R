#compute_nominal_catches
compute_nominal_catches <- function(con, year = NULL, month = NULL){
  
  reporting_flow = repfishr::reporting_flow$new(
    sender = accessCountryISOCode(con), 
    sender_type = "country"
  )
  
  data = do.call("rbind", lapply(reporting_flow$getReceiverIds(), function(receiver){
    fdi_data = accessFDIFishingActivities(con, year = year, receiver = receiver)
    fdi_data = cbind(
      receiver = if(nrow(fdi_data)>0) receiver else character(0),
      fdi_data
    )
    #TODO duplicate by fishing_trip_type / fishing_trip_type_priority
    fdi_data = fdi_data |> dplyr::select(-c(fishing_trip, fishing_trip_type, fishing_trip_type_priority))
    return(fdi_data)
  }))
  
  return(data)
}

#iccat_t1nc
iccat_t1nc <- function(con, data, metadata, file){
  
  data_for_iccat = data[data$receiver == "ICCAT",]
  data_for_iccat$receiver = NULL
  reporting_flow = repfishr::reporting_flow$new(
    sender = accessCountryISOCode(con), 
    sender_type = "country"
  )
  task = reporting_flow$getReceiver("ICCAT")$getTaskDefinitionById("iccat_task_t1nc")
  task$report(data, metadata, path = file)
}