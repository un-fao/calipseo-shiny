#readSQLScript
readSQLScript <- function(sqlfile, 
                          key = NULL, value = NULL,
                          add_filter_on_year = NULL, datetime_field = NULL,language = NULL
){
  sql <- paste(suppressWarnings(readLines(sqlfile)), collapse="")
  sql <- gsub("\\s+"," ",sql)
  
  
  if(!is.null(key) && !is.null(value)){
    sql <- sprintf("%s %s %s = %s", sql, ifelse(regexpr("WHERE", sql)>0, "AND", "WHERE"), key, value)
  }
  
  
  if(!is.null(add_filter_on_year)){
    if(is.null(datetime_field)){
      stop("Specify a datetime field in your sql statement to filter on year")
    }
    sql <- sprintf("%s %s year(%s) = %s", 
                   sql, ifelse(regexpr("WHERE", sql)>0, "AND", "WHERE"), datetime_field, add_filter_on_year)
  }
  
  
  if(is.null(language)){
    return(sql)
    
  }else{
    
    sql <- gsub("I18n_DEFAULT", sprintf("I18N_%s", toupper(language)), sql)
    return(sql)
  }
  
  return(sql)
}

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#DB ACCESSORS
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------


#accessCountryParamFromDB
accessCountryParamFromDB <- function(con){
  country_param_sql <- readSQLScript("data/core/sql/country_param.sql")
  country_param <- suppressWarnings(dbGetQuery(con, country_param_sql))
  return(country_param)
} 


#accessIndividualDetailsFromDB
accessIndividualDetailsFromDB <- function(con){
  individual_details_sql <- readSQLScript("data/core/sql/individual_details.sql")
  individual_details <- suppressWarnings(dbGetQuery(con, individual_details_sql))
  return(individual_details)
} 


#accessVesselQaNamesFromDB
accessVesselQaNamesFromDB <- function(con){
  vessel_qa_names_sql <- readSQLScript("data/core/sql/vessel_qa_names.sql")
  vessel_qa_names <- suppressWarnings(dbGetQuery(con, vessel_qa_names_sql))
  return(vessel_qa_names)
} 



#accessVesselQPortsFromDB
accessVesselQaPortsFromDB <- function(con){
  vessel_qa_ports_sql <- readSQLScript("data/core/sql/vessel_qa_ports.sql")
  vessel_qa_ports <- suppressWarnings(dbGetQuery(con, vessel_qa_ports_sql))
  return(vessel_qa_ports)
} 


#accessVesselQaCharacteristicsFromDB
accessVesselQaCharacteristicsFromDB <- function(con){
  vessel_qa_characteristics_sql <- readSQLScript("data/core/sql/vessel_qa_characteristics.sql")
  vessel_qa_characteristics <- suppressWarnings(dbGetQuery(con, vessel_qa_characteristics_sql))
  return(vessel_qa_characteristics)
} 


#accessRefSpeciesFromDB
accessRefSpeciesFromDB <- function(con){
  ref_species_sql <- readSQLScript("data/core/sql/ref_species.sql", language = appConfig$language)
  ref_species <- suppressWarnings(dbGetQuery(con, ref_species_sql))
  return(ref_species)
}  

#accessRefFishingUnitsFromDB
accessRefFishingUnitsFromDB <- function(con){
  ref_fishing_units_sql <- readSQLScript("data/core/sql/ref_fishing_units.sql", language = appConfig$language)
  ref_fishing_units <- suppressWarnings(dbGetQuery(con, ref_fishing_units_sql))
  return(ref_fishing_units)
}


#accessSpeciesCatchesYearFromDB
accessSpeciesCatchesYearFromDB <- function(con, registrationNumber){
  species_catches_year_sql <- readSQLScript("data/core/sql/fish_species_catches_totalbyyear.sql",
                                            key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"),
                                            language = appConfig$language)
  
  species_catches_year <- suppressWarnings(dbGetQuery(con,species_catches_year_sql))
  return(species_catches_year)
}

#accessLandingSitesFromDB
accessLandingSitesFromDB <- function(con){
  landingsites_sql <- readSQLScript("data/core/sql/landing_sites.sql",
                                    language = appConfig$language)
  landingsites <- suppressWarnings(dbGetQuery(con, landingsites_sql))
  landingsites <- sf::st_as_sf(landingsites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  return(landingsites)
}  

#accessLandingSiteNamesFromDB
accessLandingSiteNamesFromDB <- function(con){
  landingsites_sql <- readSQLScript("data/core/sql/landing_sites_names.sql")
  landingsites <- suppressWarnings(dbGetQuery(con, landingsites_sql))[,1]
  return(landingsites)
}  

#accessVesselsFromDB
accessVesselsFromDB <- function(con){
  vessels_sql <- readSQLScript("data/core/sql/vessels.sql", language = appConfig$language)
  vessels <- suppressWarnings(dbGetQuery(con, vessels_sql))
  return(vessels)
}

#accessVesselsCountFromDB
accessVesselsCountFromDB <- function(con){
  vessels_count_sql <- readSQLScript("data/core/sql/vessels_count.sql")
  vessels_count <- suppressWarnings(dbGetQuery(con, vessels_count_sql))
  return(vessels_count$COUNT)
}  

#accessVesselFromDB
accessVesselFromDB <- function(con, registrationNumber){
  vessel_sql <- readSQLScript("data/core/sql/vessels.sql", 
                              key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"), 
                              language = appConfig$language)
  vessel <- suppressWarnings(dbGetQuery(con, vessel_sql))
  return(vessel)
} 


#accessVesselHistoricalCharacteristicsFromDB
accessVesselHistoricalCharacteristicsFromDB <- function(con, registrationNumber){
  vessel_historical_char_sql <- readSQLScript("data/core/sql/vessel_historical_characteristics.sql", 
                                              key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"),
                                              language = appConfig$language)
  vessel_historical_char <- suppressWarnings(dbGetQuery(con, vessel_historical_char_sql))
  return(vessel_historical_char)
}

#accessVesselOwnersFromDB
accessVesselOwnersFromDB <- function(con, registrationNumber = NULL){
  vessel_owners_sql <- readSQLScript("data/core/sql/vessels_owners.sql",
                                     key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'")
  )
  vessel_owners <- suppressWarnings(dbGetQuery(con, vessel_owners_sql))
  return(vessel_owners)
}  

#accessVesselCatchesFromDB
accessVesselCatchesFromDB <- function(con, registrationNumber){
  landing_forms_sql <- readSQLScript("data/core/sql/fishing_activities.sql", 
                                     key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"),
                                     language = appConfig$language)
  landing_forms <- suppressWarnings(dbGetQuery(con, landing_forms_sql))
  return(landing_forms)
}

#accessVesselsCountByTypeFromDB
accessVesselsCountByTypeFromDB <- function(con){
  vesseltypes_count_sql <- readSQLScript("data/core/sql/vessels_types_count.sql",
                                         language = appConfig$language)
  suppressWarnings(dbGetQuery(con, vesseltypes_count_sql))
}

#accessVesselsCountByStatTypeFromDB
accessVesselsCountByStatTypeFromDB <- function(con){
  vesselstattypes_count_sql <- readSQLScript("data/core/sql/vessels_stat_types_count.sql",
                                             language = appConfig$language)
  suppressWarnings(dbGetQuery(con, vesselstattypes_count_sql))
}

#accessVesselLicensePermitFromDB
accessVesselLicensePermitFromDB <- function(con, registrationNumber = NULL){
  
  if(!is.null(registrationNumber)){
    licensePermit_sql <- readSQLScript("data/core/sql/vessel_license_permits.sql", 
                                       key = "vlp.PERMIT_NUMBER != ''AND v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"),
                                       language = appConfig$language)
    licensePermit <- suppressWarnings(dbGetQuery(con, licensePermit_sql))
  }else{
    
    licensePermit_sql <- readSQLScript("data/core/sql/vessel_license_permits.sql",
                                       language = appConfig$language)
    licensePermit <- suppressWarnings(dbGetQuery(con, licensePermit_sql))
    
  }
  return(licensePermit)
}

#accessVesselsCountByLandingSiteFromDB
accessVesselsCountByLandingSiteFromDB <- function(con){
  vesselsites_count_sql <- readSQLScript("data/core/sql/vessels_landing_sites_count.sql",
                                         language = appConfig$language)
  sites <- suppressWarnings(dbGetQuery(con,  vesselsites_count_sql))
  sites <- sf::st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  return(sites)
}

#vesselsLandingsitesVesselTypesCountFromDB
vesselsLandingSitesVesselTypesCountFromDB <- function(con){
  vesselsitesvesseltype_count_sql <- readSQLScript("data/core/sql/vessels_ landing_sites_vessel_types_count.sql",
                                                   language = appConfig$language)
  sitesvesseltypecount <- suppressWarnings(dbGetQuery(con, vesselsitesvesseltype_count_sql))
  return(sitesvesseltypecount)
}


#countVesselCaptainsFromDB
countVesselCaptainsFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/count_vessels_captains.sql")
  count <- suppressWarnings(dbGetQuery(con, sql))$COUNT
  return(count)
} 

#countVesselOwnersFromDB
countVesselOwnersFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/count_vessels_owners.sql")
  count <- suppressWarnings(dbGetQuery(con, sql))$COUNT
  return(count)
} 

#countVesselOwnersPerVesselFromDB
countVesselOwnersPerVesselFromDB <- function(con, registrationNumber){
  vessel_Owners_Per_vessel_sql <- readSQLScript("data/core/sql/count_vessel_owner_per_vessel.sql", 
                                                key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel_Owners_Per_vessel <- suppressWarnings(dbGetQuery(con, vessel_Owners_Per_vessel_sql))
  return(vessel_Owners_Per_vessel)
} 


#countVesselDaysAtSeaFromDB
countVesselDaysAtSeaFromDB <- function(con, registrationNumber){
  vessel_days_at_sea_sql <- readSQLScript("data/core/sql/count_vessel_daysatsea.sql", 
                                          key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel_days_at_sea <- suppressWarnings(dbGetQuery(con, vessel_days_at_sea_sql))
  return(vessel_days_at_sea)
}



#accessVesselsWithLogBooksFromDB
accessVesselsWithLogBooksFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/vessels_reporting_logbooks.sql")
  out <- suppressWarnings(dbGetQuery(con, sql))
  outids <- out$REGISTRATION_NUMBER
  names(outids) <- out$NAME
  return(outids) 
} 

#accessVesselsOwnersWithLogBooksFromDB
accessVesselsOwnersWithLogBooksFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/vessels_owners_reporting_logbooks.sql")
  out <- suppressWarnings(dbGetQuery(con, sql))
  return(out$NAME)
}  


#DATA

#countFishingTripsFromDB
countFishingTripsFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/count_fishing_trips.sql")
  count <- suppressWarnings(dbGetQuery(con, sql))$COUNT
}  


#countFishingTripsPerVesselFromDB
countFishingTripsPerVesselFromDB <- function(con, registrationNumber){
  fishingTripsPerVessel_sql <- readSQLScript("data/core/sql/count_fishing_trip_per_vessel.sql", 
                                             key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  
  if(!is.null(registrationNumber)){
    fishingTripsPerVessel_sql <- paste0(fishingTripsPerVessel_sql, " GROUP BY year(ft.DATE_TO);")
  }
  
  fishingTripsPerVessel_sql <- suppressWarnings(dbGetQuery(con, fishingTripsPerVessel_sql))
  return(fishingTripsPerVessel_sql)
}


#countVesselsWithOrWithoutFishingTripsFromDB
countVesselsWithOrWithoutFishingTripsFromDB <- function(con, ftpv_activity_year = NULL){
  
  VesselsWithOrWithoutFishingTrips_sql <- readSQLScript("data/core/sql/count_vessels_with_or_without_fishing_trips.sql")
  if(!is.null(ftpv_activity_year)){
    VesselsWithOrWithoutFishingTrips_sql <- paste0(VesselsWithOrWithoutFishingTrips_sql, " WHERE year(ft.DATE_FROM) = '", ftpv_activity_year, "'") 
  }
  
  
  VesselsWithOrWithoutFishingTrips <- suppressWarnings(dbGetQuery(con, VesselsWithOrWithoutFishingTrips_sql))
  
  return(VesselsWithOrWithoutFishingTrips)
}  


#countVesselFishingGearsFromDB
countVesselFishingGearsFromDB <- function(con, registrationNumber){
  vessel_fishing_gears_sql <- readSQLScript("data/core/sql/count_vessel_fishing_gears.sql", 
                                            key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel_fishing_gears <- suppressWarnings(dbGetQuery(con, vessel_fishing_gears_sql))
  return(vessel_fishing_gears)
} 


#countVesselLicensePermitFromDB
countVesselLicensePermitFromDB <- function(con, registrationNumber){
  vessel_license_permit_sql <- readSQLScript("data/core/sql/count_vessel_license_permits.sql", 
                                             key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel_license_permit <- suppressWarnings(dbGetQuery(con, vessel_license_permit_sql))
  return(vessel_license_permit)
} 


#accessAvailableYearsFromDB
accessAvailableYearsFromDB <- function(con){
  fishing_trip_years_sql <- readSQLScript("data/core/sql/fishing_trip_years.sql")
  fishing_trip_years <- suppressWarnings(dbGetQuery(con, fishing_trip_years_sql))$YEAR
  return(fishing_trip_years)
}

#accessFishingActivitiesFromDB
accessFishingActivitiesFromDB <- function(con, year, 
                                          vessel_stat_type = NULL, vesselId = NULL,
                                          entityOwner = NULL){
  fa_sql <- readSQLScript("data/core/sql/fishing_activities.sql",
                          add_filter_on_year = year, datetime_field = "ft.DATE_TO",
                          language = appConfig$language)
  if(!is.null(vessel_stat_type)){
    fa_sql <- paste0(fa_sql, " AND v.CL_APP_VESSEL_STAT_TYPE_ID = ", vessel_stat_type)
  }
  if(!is.null(vesselId)){
    fa_sql <- paste0(fa_sql, " AND v.REGISTRATION_NUMBER = '", vesselId, "'")
  }
  if(!is.null(entityOwner)){
    fa_sql <- paste0(fa_sql, " AND ent.NAME = '", entityOwner, "'")
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
} 

#accessFishingActivitiesMultiyearFromDB
accessFishingActivitiesMultiyearFromDB <- function(con,vessel_stat_type = NULL){
  fa_sql <- readSQLScript("data/core/sql/fishing_activities_multiyear.sql",
                          language = appConfig$language)
  if(!is.null(vessel_stat_type)){
    fa_sql <- paste0(fa_sql, " WHERE v.CL_APP_VESSEL_STAT_TYPE_ID = ", vessel_stat_type)
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
} 

#accessFishingTripsFromDB
accessFishingTripsFromDB <- function(con,vessel_stat_type = NULL,vesselId = NULL){
  fa_sql <- readSQLScript("data/core/sql/fishing_trips.sql",language = appConfig$language)
  if(!is.null(vessel_stat_type)){
    fa_sql <- paste0(fa_sql, " WHERE v.CL_APP_VESSEL_STAT_TYPE_ID = ", vessel_stat_type)
  }
  if(!is.null(vesselId)){
    fa_sql <- paste0(fa_sql, " WHERE v.REGISTRATION_NUMBER = '", vesselId, "'")
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessFishingTripDetailFromDB
accessFishingTripDetailFromDB <- function(con,trip_id = NULL){
  fa_sql <- readSQLScript("data/core/sql/fishing_trip_details.sql", 
                          language = appConfig$language)
  if(!is.null(trip_id)){
    fa_sql <- paste0(fa_sql, " WHERE ft.ID = ", trip_id)
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessLandingFormsFromDB
accessLandingFormsFromDB <- function(con, year, vesselId = NULL, entityOwner = NULL){
  accessFishingActivitiesFromDB(con, year, vessel_stat_type = 1, vesselId = vesselId, entityOwner = entityOwner)
}

#accessLogBooksFromDB
accessLogBooksFromDB <- function(con, year, vesselId = NULL, entityOwner = NULL){
  accessFishingActivitiesFromDB(con, year, vessel_stat_type = 2, vesselId = vesselId, entityOwner = entityOwner)
}

#accessLogBooksMultiyearFromDB
accessLogBooksMultiyearFromDB <- function(con){
  accessFishingActivitiesMultiyearFromDB(con,vessel_stat_type = 2)
}

#accessLogBooksTripsFromDB
accessLogBooksTripsFromDB <- function(con,vesselId=NULL){
  accessFishingTripsFromDB(con,vessel_stat_type = 2,vesselId = vesselId)
}

#accessMonthlyFishingActivityFromDB
accessMonthlyFishingActivityFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/fishing_activities_totalbymonth.sql")
  out <- suppressWarnings(dbGetQuery(con, sql))
  return(out)
} 

#accessSurveyDateAndStratumFromDB
accessSurveyDateAndStratumFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/survey_date.sql", language = appConfig$language)
  out <- suppressWarnings(dbGetQuery(con, sql))
  return(out)
}

#accessEffortDataFromDB
accessEffortDataFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/effort_data.sql")
  if(!is.null(month)&!is.null(year)&!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE s.YEAR = %s AND s.CL_APP_MONTH_ID = %s AND s.CL_FISH_FISHING_UNIT_ID = %s",year,month,fishing_unit ))
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessLandingDataFromDB
accessLandingDataFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/landing_data.sql")
  if(!is.null(month)&!is.null(year)&!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE l.year = %s AND l.month = %s AND l.fishing_unit = %s",year,month,fishing_unit ))
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#GENERIC DATA ACCESSORS (considering this needs to be replaced later by API calls)
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------


#accessCountryParamFromDB
accessCountryParam <- function(con){
  accessCountryParamFromDB(con)
}


#accessIndividualDetailsFromDB
accessIndividualDetails <- function(con){
  accessIndividualDetailsFromDB(con)
}


#accessVesselQaNamesFromDB
accessVesselQaNames <- function(con){
  accessVesselQaNamesFromDB(con)
}


#accessVesselQaPortsFromDB
accessVesselQaPorts <- function(con){
  accessVesselQaPortsFromDB(con)
}


#accessVesselQaCharacteristicsFromDB
accessVesselQaCharacteristics <- function(con){
  accessVesselQaCharacteristicsFromDB(con)
}


#accessRefSpecies
accessRefSpecies <- function(con){
  accessRefSpeciesFromDB(con)
}

#accessRefFishingUnits
accessRefFishingUnits <- function(con){
  accessRefFishingUnitsFromDB(con)
}

#accessSpeciesCatchesYear
accessSpeciesCatchesYear <- function(con, registrationNumber) {
  accessSpeciesCatchesYearFromDB(con, registrationNumber)
}

#accessLandingSites
accessLandingSites <- function(con){
  accessLandingSitesFromDB(con)
}

#accessLandingSiteNames
accessLandingSiteNames <- function(con){
  accessLandingSiteNamesFromDB(con)
}

#accessVessels
accessVessels <- function(con){
  accessVesselsFromDB(con)
}

#accessVesselsCount
accessVesselsCount <- function(con){
  accessVesselsCountFromDB(con)
}

#accessVessel
accessVessel <- function(con, registrationNumber){
  accessVesselFromDB(con, registrationNumber)
}


#accessVesselHistoricalCharacteristics
accessVesselHistoricalCharacteristics <- function(con, registrationNumber){
  accessVesselHistoricalCharacteristicsFromDB(con, registrationNumber)
}

#accessVesselOwners
accessVesselOwners <- function(con, registrationNumber = NULL){
  accessVesselOwnersFromDB(con, registrationNumber)
}

#accessVesselCatches
accessVesselCatches <- function(con, registrationNumber = NULL){
  accessVesselCatchesFromDB(con, registrationNumber)
}

#accessVesselsCountByType
accessVesselsCountByType <- function(con){
  accessVesselsCountByTypeFromDB(con)
}

#accessVesselsCountByStatType
accessVesselsCountByStatType <- function(con){
  accessVesselsCountByStatTypeFromDB(con)
}

#accessVesselLicensePermitFromDB
accessVesselLicensePermit <- function(con, registrationNumber){
  accessVesselLicensePermitFromDB(con, registrationNumber)
}

#accessVesselsCountByLandingSite
accessVesselsCountByLandingSite <- function(con){
  accessVesselsCountByLandingSiteFromDB(con)
}


#vesselsLandingsitesvesseltypesCount
vesselsLandingSitesVesselTypesCount <- function(con){
  vesselsLandingSitesVesselTypesCountFromDB(con)
}

#countVesselCaptains
countVesselCaptains <- function(con){
  countVesselCaptainsFromDB(con)
}

#countVesselCaptains
countVesselOwners <- function(con){
  countVesselOwnersFromDB(con)
}

#countVesselOwnersPerVessel
countVesselOwnersPerVessel <- function(con, registrationNumber) {
  countVesselOwnersPerVesselFromDB(con, registrationNumber)
}

#countVesselDaysAtSea
countVesselDaysAtSea <- function(con, registrationNumber) {
  countVesselDaysAtSeaFromDB(con, registrationNumber)
}

#accessVesselsWithLogBooks
accessVesselsWithLogBooks <- function(con){
  accessVesselsWithLogBooksFromDB(con)
}

#accessVesselsOwnersWithLogBooks
accessVesselsOwnersWithLogBooks <- function(con){
  accessVesselsOwnersWithLogBooksFromDB(con)
}

#DATA

#countFishingTrips
countFishingTrips <- function(con){
  countFishingTripsFromDB(con)
}


#countFishingTripPerVessel
countFishingTripsPerVessel <- function(con, registrationNumber){
  countFishingTripsPerVesselFromDB(con, registrationNumber)
}


#countVesselsWithOrWithoutFishingTripsFromDB
countVesselsWithOrWithoutFishingTrips <- function(con, ftpv_activity_year){
  countVesselsWithOrWithoutFishingTripsFromDB(con, ftpv_activity_year)
}


#countVesselFishingGears
countVesselFishingGears <- function(con, registrationNumber) {
  countVesselFishingGearsFromDB(con, registrationNumber)
}


#countVesselLicensePermit
countVesselLicensePermit <- function(con, registrationNumber) {
  countVesselLicensePermitFromDB(con, registrationNumber)
}


#accessAvailableYears
accessAvailableYears <- function(con){
  accessAvailableYearsFromDB(con)
}

#accessLandingForms
accessLandingForms <- function(con, year, vesselId = NULL, entityOwner = NULL){
  accessLandingFormsFromDB(con, year, vesselId = vesselId, entityOwner = entityOwner)
}

#accessLogBooks
accessLogBooks <- function(con, year, vesselId = NULL, entityOwner = NULL){
  accessLogBooksFromDB(con, year, vesselId = vesselId, entityOwner = entityOwner)
}

#accessLogBooksMultiyear
accessLogBooksMultiyear <- function(con){
  accessLogBooksMultiyearFromDB(con)
}

#accessLogBooksTrips
accessLogBooksTrips <- function(con){
  accessLogBooksTripsFromDB(con)
}

#accessFishingTrips
accessFishingTrips <- function(con,vessel_stat_type,vesselId = NULL)
  accessFishingTripsFromDB(con,vessel_stat_type,vesselId)

#accessFishingTripDetails
accessFishingTripDetails <- function(con,trip_id){
  accessFishingTripDetailFromDB(con,trip_id)
}

#accessMonthlyFishingActivity
accessMonthlyFishingActivity <- function(con){
  accessMonthlyFishingActivityFromDB(con)
}

#accessSurveyDateAndStratum
accessSurveyDateAndStratum <- function(con){
  accessSurveyDateAndStratumFromDB(con)
}

#accessEffortData
accessEffortData <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessEffortDataFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessLandingData
accessLandingData <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessLandingDataFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#Country profile
#-----------------------------------------------------------------------------------------------------
loadCountryProfile <- function(appConfig, con){
  if(is.null(appConfig$country_profile$iso3)){
    stop("Missing country ISO3 code in configuration file!")
  }
  country_data <- DBI::dbGetQuery(con, sprintf("select * from cl_ref_countries where ISO_3_CODE = '%s'", appConfig$country_profile$iso3))
  appConfig$country_profile$data <- country_data[1L,]
  return(appConfig)
}


#Local data accessors
#-----------------------------------------------------------------------------------------------------

#loadLocalDataset
loadLocalDataset <- function(filename){
  filesplits <- unlist(strsplit(filename, "/"))
  objectname <- unlist(strsplit(filesplits[length(filesplits)], "\\."))[1]
  data <- switch(mime::guess_type(filename),
                 "application/json" = jsonlite::read_json(filename),
                 "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = as.data.frame(readxl::read_xlsx(filename))
  )
  assign(objectname, data, envir = CALIPSEO_SHINY_ENV)
}

#loadLocalCountryDatasets
loadLocalCountryDatasets <- function(config){
  country_dir <- sprintf("./data/country/%s", config$country_profile$iso3)
  if(dir.exists(country_dir)){
    files <- list.files(path = country_dir, full.names = TRUE)
    for(file in files){
      message(sprintf("Loading local dataset '%s'", file))
      loadLocalDataset(file)
    }
  }
}

#getLocalCountryDataset
getLocalCountryDataset <- function(name){
  get(name, envir = CALIPSEO_SHINY_ENV)
}

#getLocalCountryDatasets
getLocalCountryDatasets <- function(config){
  out <- list()
  country_dir <- sprintf("./data/country/%s", config$country_profile$iso3)
  if(dir.exists(country_dir)){
    files <- list.files(path = country_dir, full.names = TRUE)
    out <- lapply(files, function(file){
      basefilename = unlist(strsplit(basename(file), "\\."))[1]
      return(get(basefilename, envir = CALIPSEO_SHINY_ENV))
    })
    names(out) <- lapply(files, function(file){
      basefilename = unlist(strsplit(basename(file), "\\."))[1]
      return(basefilename)
    })
  }
  return(out)
}

#loadRemoteReferenceDataset
loadRemoteReferenceDataset <- function(objectname,filename){
  data <- as.data.frame(readr::read_csv(filename))
  assign(objectname, data, envir = CALIPSEO_SHINY_ENV)
}

#getRemoteReferenceDataset
getRemoteReferenceDataset <- function(name){
  get(name, envir = CALIPSEO_SHINY_ENV)
}
