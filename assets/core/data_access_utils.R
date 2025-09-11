#readSQL
readSQL <- function(sqlfile, 
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

#executeSQL
getFromSQL <- function(con, sql){
  start <- Sys.time()
  sql_data <- suppressWarnings(dbGetQuery(con, sql))
  end <- Sys.time()
  time = end-start
  DEBUG("=> Query processing time: %s %s", as(time, "numeric"), attr(time, "units"))
  return(sql_data)
}

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#DB ACCESSORS
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#<COMMON>
#accessAvailableYearsFromDB
accessAvailableYearsFromDB <- function(con){
  DEBUG("Query available years")
  fishing_trip_years_sql <- readSQL("data/core/sql/fishing_trip_years.sql")
  fishing_trip_years <- getFromSQL(con, fishing_trip_years_sql)
  return(fishing_trip_years)
}

#<COUNTRY PARAMETERS>
#accessCountryParamFromDB
accessCountryParamFromDB <- function(con){
  DEBUG("Query country parameters")
  country_param_sql <- readSQL("data/core/sql/country_param.sql")
  country_param <- getFromSQL(con, country_param_sql)
  return(country_param)
} 

#accessCountryPrefUnitWeightFromDB
accessCountryPrefUnitWeightFromDB <- function(con){
  DEBUG("Query country parameter - preferred weight unit")
  country_param_sql <- readSQL("data/core/sql/country_pref_unit_weight.sql")
  country_param <- getFromSQL(con, country_param_sql)
  return(country_param)
} 

#accessCountryPrefCurrencyFromDB
accessCountryPrefCurrencyFromDB <- function(con){
  DEBUG("Query country parameter - preferred currency")
  country_param_sql <- readSQL("data/core/sql/country_pref_currency.sql")
  country_param <- getFromSQL(con, country_param_sql)
  return(country_param)
}

#<MODULE:HOME>
#countVesselsFromDB
countVesselsFromDB <- function(con){
  DEBUG("Count vessels")
  sql <- readSQL("data/core/sql/count_vessels.sql")
  count <- getFromSQL(con, sql)
  return(count$COUNT)
}
#countVesselOwnersFromDB
countVesselOwnersFromDB <- function(con){
  DEBUG("Count vessel owners")
  sql <- readSQL("data/core/sql/count_vessels_owners.sql")
  count <- getFromSQL(con, sql)$COUNT
  return(count)
} 
#countVesselCaptainsFromDB
countVesselCaptainsFromDB <- function(con){
  DEBUG("Count vessel captains")
  sql <- readSQL("data/core/sql/count_vessels_captains.sql")
  count <- getFromSQL(con, sql)$COUNT
  return(count)
} 
#countFishingTripsFromDB
countFishingTripsFromDB <- function(con){
  DEBUG("Count fishing trips")
  sql <- readSQL("data/core/sql/count_fishing_trips.sql")
  count <- getFromSQL(con, sql)$COUNT
}
#countLandingSitesFromDB
countLandingSitesFromDB <- function(con){
  DEBUG("Count landing sites")
  sql <- readSQL("data/core/sql/count_landing_sites.sql")
  count <- getFromSQL(con, sql)$COUNT
}

#<COMMON:VESSELS>
#accessVesselsFromDB
accessVesselsFromDB <- function(con){
  DEBUG("Query Vessel list")
  vessels_sql <- readSQL("data/core/sql/vessels.sql", language = appConfig$language)
  vessels <- getFromSQL(con, vessels_sql)
  return(vessels)
}
#accessVesselLicensePermitFromDB
accessVesselLicensePermitFromDB <- function(con, registrationNumber = NULL){
  
  if(!is.null(registrationNumber)){
    DEBUG("Query vessel license permits for '%s'", registrationNumber)
    licensePermit_sql <- readSQL("data/core/sql/vessel_license_permits.sql", 
                                 key = "vlp.PERMIT_NUMBER != ''AND v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"),
                                 language = appConfig$language)
    licensePermit <- getFromSQL(con, licensePermit_sql)
  }else{
    DEBUG("Query vessel license permits")
    licensePermit_sql <- readSQL("data/core/sql/vessel_license_permits.sql",
                                 language = appConfig$language)
    licensePermit <- getFromSQL(con, licensePermit_sql)
    
  }
  return(licensePermit)
}

#<MODULE:VESSEL_LIST>
#accessVesselsFromDB - see <COMMON:VESSELS>
#accessVesselLicensePermitFromDB - see <COMMON:VESSELS>

#<INDIVIDUALS>
#accessIndividualInfoFromDB
accessIndividualInfoFromDB <- function(con){
  DEBUG("Query country parameter - preferred weight unit")
  individual_info_sql <- readSQL("data/core/sql/individual_info.sql")
  individual_info <- getFromSQL(con, individual_info_sql)
  return(individual_info)
} 

#accessIndividualIsFisherFromDB
accessIndividualIsFisherFromDB <- function(con){
  individual_isfisher_sql <- readSQL("data/core/sql/individual_isfisher.sql")
  individual_isfisher <- getFromSQL(con, individual_isfisher_sql)
  return(individual_isfisher)
} 

#accessVesselQaNamesFromDB
accessVesselQaNamesFromDB <- function(con){
  DEBUG("Query Vessel QA names")
  vessel_qa_names_sql <- readSQL("data/core/sql/vessel_qa_names.sql")
  vessel_qa_names <- getFromSQL(con, vessel_qa_names_sql)
  return(vessel_qa_names)
}

#accessVesselQPortsFromDB
accessVesselQaPortsFromDB <- function(con){
  DEBUG("Query Vessel QA ports")
  vessel_qa_ports_sql <- readSQL("data/core/sql/vessel_qa_ports.sql")
  vessel_qa_ports <- getFromSQL(con, vessel_qa_ports_sql)
  return(vessel_qa_ports)
} 

#accessVesselQaCharacteristicsFromDB
accessVesselQaCharacteristicsFromDB <- function(con){
  DEBUG("Query Vessel QA characteristics")
  vessel_qa_characteristics_sql <- readSQL("data/core/sql/vessel_qa_characteristics.sql")
  vessel_qa_characteristics <- getFromSQL(con, vessel_qa_characteristics_sql)
  return(vessel_qa_characteristics)
} 

#accessIndividualQaDOBFromDB
accessIndividualQaDOBFromDB <- function(con){
  DEBUG("Query Individuals QA DOB")
  individual_qa_dob_sql <- readSQL("data/core/sql/individual_qa_dob.sql")
  individual_qa_dob_sql <- getFromSQL(con, individual_qa_dob_sql)
  return(individual_qa_dob_sql)
} 

#accessRefSpeciesFromDB
accessRefSpeciesFromDB <- function(con,year=NULL){
  DEBUG("Query Ref Species")
  ref_species_sql <- readSQL("data/core/sql/ref_species.sql", language = appConfig$language)
  ref_species <- getFromSQL(con, ref_species_sql)
  return(ref_species)
}  

#accessRefFishingUnitsFromDB
accessRefFishingUnitsFromDB <- function(con){
  DEBUG("Query Ref fishing units")
  ref_fishing_units_sql <- readSQL("data/core/sql/ref_fishing_units.sql", language = appConfig$language)
  ref_fishing_units <- getFromSQL(con, ref_fishing_units_sql)
  return(ref_fishing_units)
}

#accessSpeciesCatchesYearFromDB
accessSpeciesCatchesYearFromDB <- function(con, registrationNumber){
  DEBUG("Query Species catches total by year for vessel '%s'", registrationNumber)
  species_catches_year_sql <- readSQL("data/core/sql/fish_species_catches_totalbyyear.sql",
                                            key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"),
                                            language = appConfig$language)
  
  species_catches_year <- getFromSQL(con,species_catches_year_sql)
  return(species_catches_year)
}

#accessLandingSitesFromDB
accessLandingSitesFromDB <- function(con, sf = TRUE){
  DEBUG("Query landing sites")
  landingsites_sql <- readSQL("data/core/sql/landing_sites.sql",
                                    language = appConfig$language)
  landingsites <- getFromSQL(con, landingsites_sql)
  if(sf){
    landingsites <- landingsites[!is.na(landingsites$LONGITUDE) & !is.na(landingsites$LATITUDE),]
    landingsites <- sf::st_as_sf(landingsites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  }
  return(landingsites)
}  

#accessLandingSiteNamesFromDB
accessLandingSiteNamesFromDB <- function(con){
  DEBUG("Query landing site names")
  landingsites_sql <- readSQL("data/core/sql/landing_sites_names.sql")
  landingsites <- getFromSQL(con, landingsites_sql)[,1]
  return(landingsites)
}  

#accessVesselInfoFromDB
accessVesselInfoFromDB <- function(con){
  DEBUG("Query Vessel info list")
  vessel_info_sql <- readSQL("data/core/sql/vessel_info.sql", language = appConfig$language)
  vessel_info <- getFromSQL(con, vessel_info_sql)
  return(vessel_info)
}

#accessVesselFromDB
accessVesselFromDB <- function(con, registrationNumber){
  DEBUG("Query vessel '%s'", registrationNumber)
  vessel_sql <- readSQL("data/core/sql/vessels.sql", 
                              key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"), 
                              language = appConfig$language)
  vessel <- getFromSQL(con, vessel_sql)
  return(vessel)
}

#accessIndividualFromDB
accessIndividualFromDB <- function(con, individualNumber = NULL){
  if(!is.null(individualNumber)){
    DEBUG("Query vessel '%s'", registrationNumber)
    individual_sql <- readSQL("data/core/sql/individuals.sql", 
                                    key = "ind.REG_ENTITY_ID", value = paste0("'", individualNumber, "'"), 
                                    language = appConfig$language)
  }else{
    DEBUG("Query individuals")
    individual_sql <- readSQL("data/core/sql/individuals.sql",
                                    language = appConfig$language)
  }
  individuals <- getFromSQL(con, individual_sql)
  return(individuals)
} 

#accessVesselHistoricalCharacteristicsFromDB
accessVesselHistoricalCharacteristicsFromDB <- function(con, registrationNumber){
  DEBUG("Query vessel historical characteristics for '%s'", registrationNumber)
  vessel_historical_char_sql <- readSQL("data/core/sql/vessel_historical_characteristics.sql", 
                                              key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"),
                                              language = appConfig$language)
  vessel_historical_char <- getFromSQL(con, vessel_historical_char_sql)
  return(vessel_historical_char)
}

#accessVesselOwnersFromDB
accessVesselOwnersFromDB <- function(con, registrationNumber = NULL){
  DEBUG("Query vessel owners for '%s'", registrationNumber)
  vessel_owners_sql <- readSQL("data/core/sql/vessels_owners.sql",
                                     key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'")
  )
  vessel_owners <- getFromSQL(con, vessel_owners_sql)
  return(vessel_owners)
}  

#accessVesselCatchesFromDB
accessVesselCatchesFromDB <- function(con, registrationNumber){
  DEBUG("Query vessel catches for '%s'", registrationNumber)
  landing_forms_sql <- readSQL("data/core/sql/fishing_activities.sql", 
                                     key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"),
                                     language = appConfig$language)
  landing_forms <- getFromSQL(con, landing_forms_sql)
  return(landing_forms)
}

#accessVesselsCountByTypeFromDB
accessVesselsCountByTypeFromDB <- function(con){
  DEBUG("Count vessel by type")
  vesseltypes_count_sql <- readSQL("data/core/sql/vessels_types_count.sql",
                                         language = appConfig$language)
  getFromSQL(con, vesseltypes_count_sql)
}

#accessIndividualCountByGenderFromDB
accessIndividualCountByGenderFromDB <- function(con){
  DEBUG("Count individuals by gender")
  individualgender_count_sql <- readSQL("data/core/sql/individual_gender_count.sql",
                                              language = appConfig$language)
  getFromSQL(con, individualgender_count_sql)
}

#accessIndividualCountByEdulevelFromDB
accessIndividualCountByEdulevelFromDB <- function(con, gender_id = NULL){
  
  if(gender_id=="All"){
    DEBUG("Count individuals by gender type")
    individualedulevel_count_sql <- readSQL("data/core/sql/individual_edulevel_count.sql",
                                                  language = appConfig$language)
    
  }else{
    DEBUG("Count individuals for gender type '%s'", gender_id)
    individualedulevel_count_sql <- readSQL("data/core/sql/individual_edulevel_count.sql",
                                                  key = "gend.ID", value = paste0("'", gender_id, "'"),
                                                  language = appConfig$language)
    
  }
  
  individualedulevel_count_sql <- getFromSQL(con, individualedulevel_count_sql)
}


#accessIndividualOverviewFromDB
accessIndividualOverviewFromDB <- function(con){
  DEBUG("Query individual overview")
  ind_overview_sql <- readSQL("data/core/sql/individual_overview.sql")
  ind_overview_sql <- getFromSQL(con, ind_overview_sql)
  return(ind_overview_sql)
} 

#accessVesselsCountByStatTypeFromDB
accessVesselsCountByStatTypeFromDB <- function(con){
  DEBUG("Count vessel by stat type")
  vesselstattypes_count_sql <- readSQL("data/core/sql/vessels_stat_types_count.sql",
                                             language = appConfig$language)
  getFromSQL(con, vesselstattypes_count_sql)
}

#accessVesselsCountByLandingSiteFromDB
accessVesselsCountByLandingSiteFromDB <- function(con, sf = FALSE){
  DEBUG("Count vessels by landing site")
  vesselsites_count_sql <- readSQL("data/core/sql/vessels_landing_sites_count.sql",
                                         language = appConfig$language)
  sites <- getFromSQL(con,  vesselsites_count_sql)
  if(sf){
    sites <- sites[!is.na(sites$LONGITUDE) & !is.na(sites$LATITUDE),]
    sites <- sf::st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  }
  return(sites)
}

#accessVesselsLandingSiteFromDB
accessVesselsLandingSiteFromDB <- function(con){
  DEBUG("Query vessel landing sites")
  vesselsites_sql <- readSQL("data/core/sql/vessels_landing_sites_list.sql",
                                         language = appConfig$language)
  sites <- getFromSQL(con,  vesselsites_sql)
  return(sites)
}


#vesselsLandingsitesVesselTypesCountFromDB
vesselsLandingSitesVesselTypesCountFromDB <- function(con, sf = FALSE){
  DEBUG("Count vessel types by landing site")
  vesselsitesvesseltype_count_sql <- readSQL("data/core/sql/vessels_landing_sites_vessel_types_count.sql",
                                                   language = appConfig$language)
  sites <- getFromSQL(con, vesselsitesvesseltype_count_sql)
  if(sf){
    sites <- sites[!is.na(sites$LONGITUDE) & !is.na(sites$LATITUDE),]
    sites.sf <- sf::st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    sites = cbind(sites.sf, LONGITUDE = sites$LONGITUDE, LATITUDE = sites$LATITUDE)
  }
  return(sites)
}



#countVesselOwnersPerVesselFromDB
countVesselOwnersPerVesselFromDB <- function(con, registrationNumber){
  DEBUG("Count vessel owners for vessel '%s'", registrationNumber)
  vessel_Owners_Per_vessel_sql <- readSQL("data/core/sql/count_vessel_owner_per_vessel.sql", 
                                                key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel_Owners_Per_vessel <- getFromSQL(con, vessel_Owners_Per_vessel_sql)
  return(vessel_Owners_Per_vessel)
} 


#countVesselDaysAtSeaFromDB
countVesselDaysAtSeaFromDB <- function(con, registrationNumber){
  DEBUG("Count vessel days at sea for vessel '%s'", registrationNumber)
  vessel_days_at_sea_sql <- readSQL("data/core/sql/count_vessel_daysatsea.sql", 
                                          key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel_days_at_sea <- getFromSQL(con, vessel_days_at_sea_sql)
  return(vessel_days_at_sea)
}



#accessVesselsWithLogBooksFromDB
accessVesselsWithLogBooksFromDB <- function(con){
  DEBUG("Query vessels with logbooks")
  sql <- readSQL("data/core/sql/vessels_reporting_logbooks.sql")
  out <- getFromSQL(con, sql)
  outids <- out$REGISTRATION_NUMBER
  names(outids) <- out$NAME
  return(outids) 
} 

#accessVesselsOwnersWithLogBooksFromDB
accessVesselsOwnersWithLogBooksFromDB <- function(con){
  DEBUG("Query vessel owners with logbooks")
  sql <- readSQL("data/core/sql/vessels_owners_reporting_logbooks.sql")
  out <- getFromSQL(con, sql)
  return(out$NAME)
}  


#DATA


#countFishingTripsPerVesselFromDB
countFishingTripsPerVesselFromDB <- function(con, registrationNumber){
  fishingTripsPerVessel_sql <- readSQL("data/core/sql/count_fishing_trip_per_vessel.sql", 
                                             key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  
  if(!is.null(registrationNumber)){
    fishingTripsPerVessel_sql <- paste0(fishingTripsPerVessel_sql, " GROUP BY year(ft.DATE_TO);")
  }
  
  fishingTripsPerVessel_sql <- getFromSQL(con, fishingTripsPerVessel_sql)
  return(fishingTripsPerVessel_sql)
}


#countVesselsWithOrWithoutFishingTripsFromDB
countVesselsWithOrWithoutFishingTripsFromDB <- function(con, ftpv_activity_year = NULL){
  
  VesselsWithOrWithoutFishingTrips_sql <- readSQL("data/core/sql/count_vessels_with_or_without_fishing_trips.sql")
  if(!is.null(ftpv_activity_year)){
    VesselsWithOrWithoutFishingTrips_sql <- paste0(VesselsWithOrWithoutFishingTrips_sql, " WHERE year(ft.DATE_FROM) = '", ftpv_activity_year, "'") 
  }
  
  
  VesselsWithOrWithoutFishingTrips <- getFromSQL(con, VesselsWithOrWithoutFishingTrips_sql)
  
  return(VesselsWithOrWithoutFishingTrips)
}  


#countVesselFishingGearsFromDB
countVesselFishingGearsFromDB <- function(con, registrationNumber){
  vessel_fishing_gears_sql <- readSQL("data/core/sql/count_vessel_fishing_gears.sql", 
                                            key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel_fishing_gears <- getFromSQL(con, vessel_fishing_gears_sql)
  return(vessel_fishing_gears)
} 


#countVesselLicensePermitFromDB
countVesselLicensePermitFromDB <- function(con, registrationNumber){
  vessel_license_permit_sql <- readSQL("data/core/sql/count_vessel_license_permits.sql", 
                                             key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel_license_permit <- getFromSQL(con, vessel_license_permit_sql)
  return(vessel_license_permit)
} 




#accessFishingActivitiesFromDB
accessFishingActivitiesFromDB <- function(con, year, 
                                          vessel_stat_type = NULL, vesselId = NULL,
                                          entityOwner = NULL){
  fa_sql <- readSQL("data/core/sql/fishing_activities.sql",
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
  fa <- getFromSQL(con, fa_sql)
  return(fa)
} 

#accessFishingActivitiesMultiyearFromDB
accessFishingActivitiesMultiyearFromDB <- function(con,vessel_stat_type = NULL){
  fa_sql <- readSQL("data/core/sql/fishing_activities_multiyear.sql",
                          language = appConfig$language)
  if(!is.null(vessel_stat_type)){
    fa_sql <- paste0(fa_sql, " WHERE v.CL_APP_VESSEL_STAT_TYPE_ID = ", vessel_stat_type)
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
} 

#accessFishingTripsFromDB
accessFishingTripsFromDB <- function(con,vessel_stat_type = NULL,vesselId = NULL){
  fa_sql <- readSQL("data/core/sql/fishing_trips.sql",language = appConfig$language)
  if(!is.null(vessel_stat_type)){
    fa_sql <- paste0(fa_sql, " WHERE v.CL_APP_VESSEL_STAT_TYPE_ID = ", vessel_stat_type)
  }
  if(!is.null(vesselId)){
    fa_sql <- paste0(fa_sql, " WHERE v.REGISTRATION_NUMBER = '", vesselId, "'")
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessFishingTripDetailFromDB
accessFishingTripDetailFromDB <- function(con,trip_id = NULL){
  fa_sql <- readSQL("data/core/sql/fishing_trip_details.sql", 
                          language = appConfig$language)
  if(!is.null(trip_id)){
    fa_sql <- paste0(fa_sql, " WHERE ft.ID = ", trip_id)
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessFishingActivitiesWecafcFromDB
accessFishingActivitiesWecafcFromDB <- function(con,year,vessel_stat_type = NULL){
  fa_sql <- readSQL("data/core/sql/fishing_activities_wecafc.sql",
                          language = appConfig$language)
  if(!is.null(vessel_stat_type)){
    fa_sql <- paste0(fa_sql, " AND t.CL_APP_VESSEL_STAT_TYPE_ID = ", vessel_stat_type)
  }
  if(!is.null(year)){
    fa_sql <- paste0(fa_sql, " AND t.year = ", year)
  }
  fa <- getFromSQL(con, fa_sql)
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
  sql <- readSQL("data/core/sql/fishing_activities_totalbymonth.sql")
  out <- getFromSQL(con, sql)
  return(out)
} 

#accessSurveyDateAndStratumFromDB
accessSurveyDateAndStratumFromDB <- function(con){
  sql <- readSQL("data/core/sql/survey_date.sql", language = appConfig$language)
  out <- getFromSQL(con, sql)
  return(out)
}

#accessSurveyPeriodsFromDB
accessSurveyPeriodsFromDB <- function(con){
  sql <- readSQL("data/core/sql/survey_periods.sql")
  out <- getFromSQL(con, sql)
  return(out)
}

#accessEffortSurveyPeriodsFromDB
accessEffortSurveyPeriodsFromDB <- function(con){
  sql <- readSQL("data/core/sql/effort_survey_periods.sql")
  out <- getFromSQL(con, sql)
  return(out)
}

#accessLandingsSurveyPeriodsFromDB
accessLandingsSurveyPeriodsFromDB <- function(con){
  sql <- readSQL("data/core/sql/landings_survey_periods.sql")
  out <- getFromSQL(con, sql)
  return(out)
}

#accessEffortDataFromDB
accessEffortDataFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/effort_data.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE s.YEAR = %s AND s.CL_APP_MONTH_ID = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND s.CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessEffortDataByFleetSegmentFromDB
accessEffortDataByFleetSegmentFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/effort_by_fleet_segment_data.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE s.YEAR = %s AND s.CL_APP_MONTH_ID = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND s.CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessLandingDataFromDB
accessLandingDataFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/landing_data.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE l.year = %s AND l.month = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND l.fishing_unit = %s",fishing_unit ))
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessLandingDataByFleetSegmentFromDB
accessLandingDataByFleetSegmentFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/landing_by_fleet_segment_data.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE l.year = %s AND l.month = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND l.fishing_unit = %s",fishing_unit ))
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessFishingUnitsFromDB
accessFishingUnitsFromDB <- function(con){
  fa_sql <- readSQL("data/core/sql/fishing_units.sql")
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessFishingDaysPerMonthFromDB
accessFishingDaysPerMonthFromDB <- function(con){
  fa_sql <- readSQL("data/core/sql/dt_fishing_days_per_month.sql")
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessEffortSurveyFromDB
accessEffortSurveyFromDB <- function(con){
  fa_sql <- readSQL("data/core/sql/dt_effort_survey.sql")
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessTripDetailByFleetSegmentFromDB
accessTripDetailByFleetSegmentFromDB <- function(con,year = NULL,month=NULL){
  fa_sql <- readSQL("data/core/sql/species_trip_detail_by_fleet_segment.sql")
  if(!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE year(ft.DATE_TO) = %s",year))
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessArtfishAFromDB
accessArtfishAFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/artfish_A_active_vessels.sql")
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf("CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  
    fa_sql <- paste(fa_sql, "GROUP BY YEAR, CL_APP_MONTH_ID, CL_FISH_LANDING_SITE_ID, CL_FISH_FISHING_UNIT_ID")
  
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessArtfishB1FromDB
accessArtfishB1FromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/artfish_B1_effort.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE s.YEAR = %s AND s.CL_APP_MONTH_ID = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND s.CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessArtfishCFromDB
accessArtfishCFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/artfish_C_active_days.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE YEAR = %s AND CL_APP_MONTH_ID = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  
  fa_sql <- paste(fa_sql, "GROUP BY YEAR, CL_APP_MONTH_ID, CL_FISH_LANDING_SITE_ID, CL_FISH_FISHING_UNIT_ID")
  
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessArtfishDFromDB
accessArtfishDFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/artfish_D_landings.sql")
  if(!is.null(month)&!is.null(year)){
    fa_sql <- paste0(fa_sql, sprintf(" WHERE year = %s AND month = %s ",year,month))
  }
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf(" AND ft.CL_TO_PORT_SITE_ID = %s",fishing_unit ))
  }
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessArtfishARegionFromDB
accessArtfishARegionFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/artfish_A_active_vessels_region.sql")
  
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf("CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}

#accessArtfishAFleetSegmentFromDB
accessArtfishAFleetSegmentFromDB <- function(con,year = NULL,month=NULL,fishing_unit = NULL){
  fa_sql <- readSQL("data/core/sql/artfish_A_active_vessels_fleet_segment.sql")
  
  if(!is.null(fishing_unit)){
    fa_sql <- paste0(fa_sql, sprintf("fs.CL_FISH_FISHING_UNIT_ID = %s",fishing_unit ))
  }
  
  fa <- getFromSQL(con, fa_sql)
  return(fa)
}


#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#GENERIC DATA ACCESSORS (considering this needs to be replaced later by API calls)
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#<COMMON>
accessAvailableYears <- function(con){ accessAvailableYearsFromDB(con) }

#<COUNTRY PARAMETERS>
accessCountryParam <- function(con){ accessCountryParamFromDB(con) }
accessCountryPrefUnitWeight <- function(con){ accessCountryPrefUnitWeightFromDB(con) }
accessCountryPrefCurrency <- function(con){ accessCountryPrefCurrencyFromDB(con) }

#<MODULE:HOME>
countVessels <- function(con){ countVesselsFromDB(con) }
countVesselOwners <- function(con){ countVesselOwnersFromDB(con) }
countVesselCaptains <- function(con){ countVesselCaptainsFromDB(con) }
countFishingTrips <- function(con){ countFishingTripsFromDB(con) }
countLandingSites <- function(con){ countLandingSitesFromDB(con) }

#<COMMON:VESSELS>
accessVessels <- function(con){ accessVesselsFromDB(con) }
accessVesselLicensePermit <- function(con, registrationNumber){ accessVesselLicensePermitFromDB(con, registrationNumber) }

#<MODULE:VESSEL_LIST>
#accessVessels - see <COMMON:VESSELS>
#accessVesselLicensePermit - see <COMMON:VESSELS>

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



#accessVesselInfo
accessVesselInfo <- function(con){
  accessVesselInfoFromDB(con)
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


