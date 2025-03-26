###artfish_estimates_by_fleet_segment
artfish_estimates_by_fleet_segment<-function(con,year=NULL,month=NULL,data_effort=NULL,data_landing=NULL){
  
  if(is.null(data_effort))data_effort=accessEffortDataByFleetSegment(con,year,month)
  if(is.null(data_landing))data_landing=accessLandingDataByFleetSegment(con,year,month)
  
  fishing_units<-accessFishingUnits(con)
  fishing_units<-subset(fishing_units,select=c(code,label))
  names(fishing_units)<-c("EST_BGC","EST_BGC_NAME")
  
  ref_species<-accessRefSpecies(con)
  ref_species$Species<-setNames(sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME),ref_species$ID)
  ref_species$ID<-as.character(ref_species$ID)
  ref_species<-subset(ref_species,select=c(ID,Species))
  names(ref_species)<-c("EST_SPC","EST_SPC_NAME")
  
  
  estimate<-artfishr::artfish_estimates_by_fleet_segment(
    data_effort = data_effort, data_landing = data_landing,
    ref_fishingunits = fishing_units, ref_species = ref_species,
    year = year, month = month
  )
  
  return(estimate)
}