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

#accessCountryPrefUnitWeightFromDB
accessCountryPrefUnitWeightFromDB <- function(con){
  country_param_sql <- readSQLScript("data/core/sql/country_pref_unit_weight.sql")
  country_param <- suppressWarnings(dbGetQuery(con, country_param_sql))
  return(country_param)
} 

#accessCountryPrefCurrencyFromDB
accessCountryPrefCurrencyFromDB <- function(con){
  country_param_sql <- readSQLScript("data/core/sql/country_pref_currency.sql")
  country_param <- suppressWarnings(dbGetQuery(con, country_param_sql))
  return(country_param)
} 

#accessIndividualInfoFromDB
accessIndividualInfoFromDB <- function(con){
  individual_info_sql <- readSQLScript("data/core/sql/individual_info.sql")
  individual_info <- suppressWarnings(dbGetQuery(con, individual_info_sql))
  return(individual_info)
} 

#accessIndividualIsFisherFromDB
accessIndividualIsFisherFromDB <- function(con){
  individual_isfisher_sql <- readSQLScript("data/core/sql/individual_isfisher.sql")
  individual_isfisher <- suppressWarnings(dbGetQuery(con, individual_isfisher_sql))
  return(individual_isfisher)
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


#accessIndividualQaDOBFromDB
accessIndividualQaDOBFromDB <- function(con){
  individual_qa_dob_sql <- readSQLScript("data/core/sql/individual_qa_dob.sql")
  individual_qa_dob_sql <- suppressWarnings(dbGetQuery(con, individual_qa_dob_sql))
  return(individual_qa_dob_sql)
} 


#accessRefSpeciesFromDB
accessRefSpeciesFromDB <- function(con,year=NULL){
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
accessLandingSitesFromDB <- function(con, sf = TRUE){
  landingsites_sql <- readSQLScript("data/core/sql/landing_sites.sql",
                                    language = appConfig$language)
  landingsites <- suppressWarnings(dbGetQuery(con, landingsites_sql))
  if(sf){
    landingsites <- landingsites[!is.na(landingsites$LONGITUDE) & !is.na(landingsites$LATITUDE),]
    landingsites <- sf::st_as_sf(landingsites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  }
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

#accessVesselInfoFromDB
accessVesselInfoFromDB <- function(con){
  vessel_info_sql <- readSQLScript("data/core/sql/vessel_info.sql", language = appConfig$language)
  vessel_info <- suppressWarnings(dbGetQuery(con, vessel_info_sql))
  return(vessel_info)
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


#accessIndividualFromDB
accessIndividualFromDB <- function(con, individualNumber = NULL){
  
  if(!is.null(individualNumber)){
    individual_sql <- readSQLScript("data/core/sql/individuals.sql", 
                                    key = "ind.REG_ENTITY_ID", value = paste0("'", individualNumber, "'"), 
                                    language = appConfig$language)
  }else{
    individual_sql <- readSQLScript("data/core/sql/individuals.sql",
                                    language = appConfig$language)
  }
  individuals <- suppressWarnings(dbGetQuery(con, individual_sql))
  return(individuals)
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


#accessIndividualCountByGenderFromDB
accessIndividualCountByGenderFromDB <- function(con){
  individualgender_count_sql <- readSQLScript("data/core/sql/individual_gender_count.sql",
                                              language = appConfig$language)
  suppressWarnings(dbGetQuery(con, individualgender_count_sql))
}


#accessIndividualCountByEdulevelFromDB
accessIndividualCountByEdulevelFromDB <- function(con, gender_id = NULL){
  
  if(gender_id=="All"){
    
    individualedulevel_count_sql <- readSQLScript("data/core/sql/individual_edulevel_count.sql",
                                                  language = appConfig$language)
    
  }else{
    
    individualedulevel_count_sql <- readSQLScript("data/core/sql/individual_edulevel_count.sql",
                                                  key = "gend.ID", value = paste0("'", gender_id, "'"),
                                                  language = appConfig$language)
    
  }
  
  individualedulevel_count_sql <- suppressWarnings(dbGetQuery(con, individualedulevel_count_sql)) 
}


#accessIndividualOverviewFromDB
accessIndividualOverviewFromDB <- function(con){
  ind_overview_sql <- readSQLScript("data/core/sql/individual_overview.sql")
  ind_overview_sql <- suppressWarnings(dbGetQuery(con, ind_overview_sql))
  return(ind_overview_sql)
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
accessVesselsCountByLandingSiteFromDB <- function(con, sf = FALSE){
  vesselsites_count_sql <- readSQLScript("data/core/sql/vessels_landing_sites_count.sql",
                                         language = appConfig$language)
  sites <- suppressWarnings(dbGetQuery(con,  vesselsites_count_sql))
  if(sf){
    sites <- sites[!is.na(sites$LONGITUDE) & !is.na(sites$LATITUDE),]
    sites <- sf::st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  }
  return(sites)
}

#accessVesselsLandingSiteFromDB
accessVesselsLandingSiteFromDB <- function(con){
  vesselsites_sql <- readSQLScript("data/core/sql/vessels_landing_sites_list.sql",
                                         language = appConfig$language)
  sites <- suppressWarnings(dbGetQuery(con,  vesselsites_sql))
  return(sites)
}


#vesselsLandingsitesVesselTypesCountFromDB
vesselsLandingSitesVesselTypesCountFromDB <- function(con, sf = FALSE){
  vesselsitesvesseltype_count_sql <- readSQLScript("data/core/sql/vessels_landing_sites_vessel_types_count.sql",
                                                   language = appConfig$language)
  sites <- suppressWarnings(dbGetQuery(con, vesselsitesvesseltype_count_sql))
  if(sf){
    sites <- sites[!is.na(sites$LONGITUDE) & !is.na(sites$LATITUDE),]
    sites.sf <- sf::st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    sites = cbind(sites.sf, LONGITUDE = sites$LONGITUDE, LATITUDE = sites$LATITUDE)
  }
  return(sites)
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
  fishing_trip_years <- suppressWarnings(dbGetQuery(con, fishing_trip_years_sql))
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

#accessFishingActivitiesWecafcFromDB
accessFishingActivitiesWecafcFromDB <- function(con,year,vessel_stat_type = NULL){
  fa_sql <- readSQLScript("data/core/sql/fishing_activities_wecafc.sql",
                          language = appConfig$language)
  if(!is.null(vessel_stat_type)){
    fa_sql <- paste0(fa_sql, " AND t.CL_APP_VESSEL_STAT_TYPE_ID = ", vessel_stat_type)
  }
  if(!is.null(year)){
    fa_sql <- paste0(fa_sql, " AND t.year = ", year)
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

#accessLogBooksWecafcFromDB
accessLogBooksWecafcFromDB <- function(con, year){
  accessFishingActivitiesWecafcFromDB(con, year, vessel_stat_type = 2)
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

#accessSurveyPeriodsFromDB
accessSurveyPeriodsFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/survey_periods.sql")
  out <- suppressWarnings(dbGetQuery(con, sql))
  return(out)
}

#accessEffortSurveyPeriodsFromDB
accessEffortSurveyPeriodsFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/effort_survey_periods.sql")
  out <- suppressWarnings(dbGetQuery(con, sql))
  return(out)
}

#accessLandingsSurveyPeriodsFromDB
accessLandingsSurveyPeriodsFromDB <- function(con){
  sql <- readSQLScript("data/core/sql/landings_survey_periods.sql")
  out <- suppressWarnings(dbGetQuery(con, sql))
  return(out)
}

#accessEffortDataFromDB
accessEffortDataFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/effort_data.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE s.YEAR = %s AND s.CL_APP_MONTH_ID = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND s.CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessEffortDataByFleetSegmentFromDB
accessEffortDataByFleetSegmentFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/effort_by_fleet_segment_data.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE s.YEAR = %s AND s.CL_APP_MONTH_ID = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND s.CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessLandingDataFromDB
accessLandingDataFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/landing_data.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE l.year = %s AND l.month = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND l.fishing_unit = %s",fishing_unit ))
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessLandingDataByFleetSegmentFromDB
accessLandingDataByFleetSegmentFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/landing_by_fleet_segment_data.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE l.year = %s AND l.month = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND l.fishing_unit = %s",fishing_unit ))
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessFishingUnitsFromDB
accessFishingUnitsFromDB <- function(con){
  fa_sql <- readSQLScript("data/core/sql/fishing_units.sql")
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessFishingDaysPerMonthFromDB
accessFishingDaysPerMonthFromDB <- function(con){
  fa_sql <- readSQLScript("data/core/sql/dt_fishing_days_per_month.sql")
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessEffortSurveyFromDB
accessEffortSurveyFromDB <- function(con){
  fa_sql <- readSQLScript("data/core/sql/dt_effort_survey.sql")
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessTripDetailByFleetSegmentFromDB
accessTripDetailByFleetSegmentFromDB <- function(con,year = NULL,month=NULL){
  fa_sql <- readSQLScript("data/core/sql/species_trip_detail_by_fleet_segment.sql")
  if(!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE year(ft.DATE_TO) = %s",year))
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessArtfishAFromDB
accessArtfishAFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/artfish_A_active_vessels.sql")
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf("CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  
    fa_sql <- paste(fa_sql, "GROUP BY YEAR, CL_APP_MONTH_ID, CL_FISH_LANDING_SITE_ID, CL_FISH_FISHING_UNIT_ID")
  
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessArtfishB1FromDB
accessArtfishB1FromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/artfish_B1_effort.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE s.YEAR = %s AND s.CL_APP_MONTH_ID = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND s.CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessArtfishCFromDB
accessArtfishCFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/artfish_C_active_days.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE YEAR = %s AND CL_APP_MONTH_ID = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  
  fa_sql <- paste(fa_sql, "GROUP BY YEAR, CL_APP_MONTH_ID, CL_FISH_LANDING_SITE_ID, CL_FISH_FISHING_UNIT_ID")
  
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessArtfishDFromDB
accessArtfishDFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/artfish_D_landings.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE year = %s AND month = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND ft.CL_TO_PORT_SITE_ID = %s",fishing_unit ))
  }
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessArtfishARegionFromDB
accessArtfishARegionFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/artfish_A_active_vessels_region.sql")
  
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf("CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  
  fa <- suppressWarnings(dbGetQuery(con, fa_sql))
  return(fa)
}

#accessArtfishAFleetSegmentFromDB
accessArtfishAFleetSegmentFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQLScript("data/core/sql/artfish_A_active_vessels_fleet_segment.sql")
  
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf("fs.CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
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

#accessCountryPrefUnitWeightFromDB
accessCountryPrefUnitWeight <- function(con){
  accessCountryPrefUnitWeightFromDB(con)
}

#accessCountryPrefCurrencyFromDB
accessCountryPrefCurrency <- function(con){
  accessCountryPrefCurrencyFromDB(con)
}

#accessIndividualInfoFromDB
accessIndividualInfo <- function(con){
  accessIndividualInfoFromDB(con)
}

#accessIndividualIsFisherFromDB
accessIndividualIsFisher <- function(con){
  accessIndividualIsFisherFromDB(con)
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


#accessIndividualQaDOBFromDB
accessIndividualQaDOB <- function(con){
  accessIndividualQaDOBFromDB(con)
}


#accessRefSpecies
accessRefSpecies <- function(con,year=NULL){
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
accessLandingSites <- function(con, sf = TRUE){
  accessLandingSitesFromDB(con, sf = sf)
}

#accessLandingSiteNames
accessLandingSiteNames <- function(con){
  accessLandingSiteNamesFromDB(con)
}

#accessVessels
accessVessels <- function(con){
  accessVesselsFromDB(con)
}

#accessVesselInfo
accessVesselInfo <- function(con){
  accessVesselInfoFromDB(con)
}

#accessVesselsCount
accessVesselsCount <- function(con){
  accessVesselsCountFromDB(con)
}

#accessVessel
accessVessel <- function(con, registrationNumber){
  accessVesselFromDB(con, registrationNumber)
}


#accessIndividual
accessIndividual <- function(con, individualNumber = NULL){
  accessIndividualFromDB(con, individualNumber)
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


#accessIndividualCountByGender
accessIndividualCountByGender <- function(con){
  accessIndividualCountByGenderFromDB(con)
}


#accessIndividualCountByEdulevel
accessIndividualCountByEdulevel <- function(con, gender_id){
  accessIndividualCountByEdulevelFromDB(con, gender_id)
}


#accessIndividualOverview
accessIndividualOverview <- function(con){
  accessIndividualOverviewFromDB(con)
}



#accessVesselLicensePermitFromDB
accessVesselLicensePermit <- function(con, registrationNumber){
  accessVesselLicensePermitFromDB(con, registrationNumber)
}

#accessVesselsCountByLandingSite
accessVesselsCountByLandingSite <- function(con, sf = FALSE){
  accessVesselsCountByLandingSiteFromDB(con, sf)
}

#accessVesselsLandingSite
accessVesselsLandingSite <- function(con){
  accessVesselsLandingSiteFromDB(con)
}


#vesselsLandingsitesvesseltypesCount
vesselsLandingSitesVesselTypesCount <- function(con, sf = FALSE){
  vesselsLandingSitesVesselTypesCountFromDB(con, sf = sf)
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

#accessLogBooksWecafc
accessLogBooksWecafc <- function(con, year){
  accessLogBooksWecafcFromDB(con, year)
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

#accessSurveyPeriods
accessSurveyPeriods <- function(con){
  accessSurveyPeriodsFromDB(con)
}

#accessEffortSurveyPeriods
accessEffortSurveyPeriods <- function(con){
  accessEffortSurveyPeriodsFromDB(con)
}

#accessLandingsSurveyPeriods
accessLandingsSurveyPeriods <- function(con){
  accessLandingsSurveyPeriodsFromDB(con)
}

#accessEffortData
accessEffortData <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessEffortDataFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessEffortDataByFleetSegment
accessEffortDataByFleetSegment <- function(con,year=NULL,month=NULL,fishing_unit = NULL){
  accessEffortDataByFleetSegmentFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessLandingData
accessLandingData <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessLandingDataFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessLandingData
accessLandingDataByFleetSegment <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessLandingDataByFleetSegmentFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessFishingUnits
accessFishingUnits<- function(con){
  accessFishingUnitsFromDB(con)
}

#accessTripDetailByFleetSegment
accessTripDetailByFleetSegment <- function(con,year=NULL,month=NULL){
  accessTripDetailByFleetSegmentFromDB(con,year=year,month=month)
}

#accessFishingDaysPerMonth
accessFishingDaysPerMonth <- function(con){
  accessFishingDaysPerMonthFromDB(con)
}

#accessEffortSurvey
accessEffortSurvey <- function(con){
  accessEffortSurveyFromDB(con)
}

#accessArtfishA
accessArtfishA <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessArtfishAFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessArtfishB1
accessArtfishB1 <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessArtfishB1FromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessArtfishC
accessArtfishC <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessArtfishCFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessArtfishD
accessArtfishD <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessArtfishDFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessArtfishARegion
accessArtfishARegion <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessArtfishARegionFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
}

#accessArtfishAFleetSegment
accessArtfishAFleetSegment <- function(con,year=NULL,month=NULL,fishing_unit=NULL){
  accessArtfishAFleetSegmentFromDB(con,year=year,month=month,fishing_unit=fishing_unit)
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
#@deprecated
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
#@deprecated
loadLocalCountryDatasets <- function(config){
  local_dir <- if(config$local) "../calipseo-data" else "data"
  country_dir <- sprintf("%s/country/%s", local_dir, config$country_profile$iso3)
  if(dir.exists(country_dir)){
    files <- list.files(path = country_dir, full.names = TRUE)
    for(file in files){
      message(sprintf("Loading local dataset '%s'", file))
      loadLocalDataset(file)
    }
  }
}

#getLocalCountryDataset
getLocalCountryDataset <- function(config,filename){
  local_dir <- if(config$local) "../calipseo-data" else "data"
  country_dir <- sprintf("%s/country/%s", local_dir, config$country_profile$iso3)
  filename <- file.path(country_dir, filename)
  data <- switch(mime::guess_type(filename),
   "application/json" = jsonlite::read_json(filename),
   "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = as.data.frame(readxl::read_xlsx(filename))
  )
  return(data)
}

#getLocalCountryDatasets
#@deprecated
getLocalCountryDatasets <- function(config){
  out <- list()
  local_dir <- if(config$local) "../calipseo-data" else "data"
  country_dir <- sprintf("%s/country/%s", local_dir, config$country_profile$iso3)
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

#getProcessOutput
getProcessOutputs <- function(config, id, year, quarter = NULL, month = NULL, target = "release"){
  if(target == "release+staging"){
    out <- rbind(
      getProcessOutputs(config, id, year, quarter, month, target = "release"),
      getProcessOutputs(config, id, year, quarter, month, target = "staging")
    )
  }else{
    filepath <- file.path(config$store, target, id, year)
    if(!is.null(quarter)) filepath <- file.path(filepath, paste0("Q",quarter))
    if(!is.null(month)) filepath <- file.path(filepath, paste0("M",month))
    files <- list.files(filepath,recursive = T,full.names = T, pattern = ".csv")
    print(files)
    out <- do.call("rbind", lapply(files, readr::read_csv))
  }
  
  return(out)
  
}

#getStatPeriods
getStatPeriods <- function(config, id,target = "release"){
  
  out <- data.frame(
    year = integer(0),
    quarter = integer(0),
    month = integer(0),
    file = integer(0)
  )
  
  if(target == "release+staging"){
    out <- rbind(
      getStatPeriods(config, id, target = "release"),
      getStatPeriods(config, id, target = "staging")
    )
  }else{
    
    target_folder<-sprintf("%s/%s/%s",config$store,target, id)
    
    full_path<-list.files(target_folder,recursive = T,full.names = T)
    files<-list.files(target_folder,recursive = T,full.names = F)
    
    if(length(files)>0){
      x<-strsplit(files,"/")
      years<-unlist(lapply(x, function(l) l[[1]]))
      
      by_year<-2%in%unique(unlist(lapply(x, function(l) length(l))))
      
      if(by_year){
        out <- data.frame(year = years,file=full_path)
      }else{
        
        month_quarter<-unlist(lapply(x, function(l) l[[2]]))
        
        by_quarter = any(sapply(month_quarter, startsWith, "Q"))
        by_month = any(sapply(month_quarter, startsWith, "M"))
        
        if(by_quarter){
          out <- data.frame(year = years, quarter = month_quarter, file=full_path)
        }
        if(by_month){
          out <- data.frame(year = years, month = month_quarter, file=full_path)
        }
        
      }
    }
  }
  

  
  return(out)
}


