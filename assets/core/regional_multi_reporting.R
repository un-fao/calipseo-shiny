#compute_nominal_catches
compute_nominal_catches <- function(con, year = NULL, month = NULL, raised_artisanal_data = NULL){
  
  reporting_flow = repfishr::reporting_flow$new(
    sender = accessCountryISOCode(con), 
    sender_type = "country"
  )
  
  #condition to exclude or not landings forms
  #if nominal catches computation include a reference to raised artisanal_data
  exclude_landing_forms = if(is.null(raised_artisanal_data)) FALSE else TRUE
  
  #pull data with mapping to different receivers
  data = do.call("rbind", lapply(reporting_flow$getReceiverIds(), function(receiver){
    fdi_data = accessFDIFishingActivities(con, year = year, receiver = receiver, exclude_landing_forms = exclude_landing_forms)
    fdi_data = cbind(
      receiver = if(nrow(fdi_data)>0) receiver else character(0),
      fdi_data
    )
    #TODO duplicate by fishing_trip_type / fishing_trip_type_priority
    if(nrow(fdi_data)>0) fdi_data = fdi_data |> dplyr::select(-c(fishing_trip, fishing_trip_type, fishing_trip_type_priority))
    
    #TODO managed raised_artisanal data
    
    if(!is.null(raised_artisanal_data)){
      #we align output of raised artisanal data (assuming Artfish?) to the FDI max data requirements for NC
      #TODO some interrogations how to map raised artisnala data to maximum data requirements for NC
      raised_data = raised_artisanal_data |> dplyr::mutate(
        time_start = lubridate::make_date(year, month, 1),
        time_end   = lubridate::ceiling_date(time_start, unit = "month") - days(1)
      )
      
      raised_fdi_data = data.frame(
        receiver = receiver,
        vessel = NA,
        time_start = raised_data$time_start,
        time_end = raised_data$time_end,
        data_source = "",#TODO get data_source mapping name for LANDFORM (fishing_trip_type id = 2)
        fishing_activity = NA,
        fishing_zone = "",#TODO artisanal fisheries -> 
        longitude_start = NA,
        latitude_start = NA,
        longitude_end = NA,
        latitude_end = NA,
        species = raised_data$species,
        gear_type = "", #TODO reverse mapping from fishing units
        measurement = "catch",
        measurement_type = "NL",
        measurement_value = raised_data$catch_nominal_landed,
        measurement_unit = accessCountryPrefUnitWeight(con)$CODE
      )
      fdi_data <- rbind(fdi_data, raised_fdi_data)
    }
    
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
  task$report(data_for_iccat, metadata, path = file)
}

#fao_ns1
fao_ns1 <- function(con, data, metadata, file){
  
  data_for_unfao = data[data$receiver == "UN-FAO",]
  data_for_unfao$receiver = NULL
  reporting_flow = repfishr::reporting_flow$new(
    sender = accessCountryISOCode(con), 
    sender_type = "country"
  )
  task = reporting_flow$getReceiver("UN-FAO")$getTaskDefinitionById("unfao_task_ns1")
  task$report(data_for_unfao, metadata, path = file)
}