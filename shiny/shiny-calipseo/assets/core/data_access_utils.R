#readSQLScript
readSQLScript <- function(sqlfile, 
                          key = NULL, value = NULL,
                          add_filter_on_year = NULL, datetime_field = NULL
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
  
  return(sql)
}

#DB callers
#-----------------------------------------------------------------------------------------------------

#accessLandingSitesFromDB
accessLandingSitesFromDB <- function(con){
  landingsites_sql <- readSQLScript("data/core/sql/landing_sites.sql")
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
  vessels_sql <- readSQLScript("data/core/sql/vessels.sql")
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
                              key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel <- suppressWarnings(dbGetQuery(con, vessel_sql))
  return(vessel)
}

#accessVesselOwnersFromDB
accessVesselOwnersFromDB <- function(con, registrationNumber = NULL){
  vessel_owners_sql <- readSQLScript("data/core/sql/vessels_owners.sql",
                                     key = if(!is.null(registrationNumber)) "vo.REG_VESSEL_ID" else NULL,
                                     value = if(!is.null(registrationNumber)) paste("'", registrationNumber, "'") else NULL
  )
  vessel_owners <- suppressWarnings(dbGetQuery(con, vessel_owners_sql))
  return(vessel_owners)
}

#accessVesselCatchesFromDB
accessVesselCatchesFromDB <- function(con, registrationNumber){
  landing_forms_sql <- readSQLScript("data/core/sql/fishing_activities.sql", 
                                     key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  landing_forms <- suppressWarnings(dbGetQuery(con, landing_forms_sql))
  return(landing_forms)
}

#accessVesselsCountByTypeFromDB
accessVesselsCountByTypeFromDB <- function(con){
  vesseltypes_count_sql <- readSQLScript("data/core/sql/vessels_types_count.sql")
  suppressWarnings(dbGetQuery(con, vesseltypes_count_sql))
}

#accessVesselsCountByLandingSiteFromDB
accessVesselsCountByLandingSiteFromDB <- function(con){
  vesselsites_count_sql <- readSQLScript("data/core/sql/vessels_landing_sites_count.sql")
  sites <- suppressWarnings(dbGetQuery(con,  vesselsites_count_sql))
  sites <- sf::st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  return(sites)
}

#accessAvailableYearsFromDB
accessAvailableYearsFromDB <- function(con){
  fishing_trip_years_sql <- readSQLScript("data/core/sql/fishing_trip_years.sql")
  fishing_trip_years <- suppressWarnings(dbGetQuery(con, fishing_trip_years_sql))
  return(fishing_trip_years)
}

#accessLandingFormsFromDB
accessLandingFormsFromDB <- function(con, year){
  landing_forms_sql <- readSQLScript("data/core/sql/fishing_activities.sql", 
                                     add_filter_on_year = year, datetime_field = "ft.DATE_TO")
  landing_forms <- suppressWarnings(dbGetQuery(con, landing_forms_sql))
  return(landing_forms)
}

#generic data callers (considering this needs to be replaced later by API calls)
#-----------------------------------------------------------------------------------------------------

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

#accessVesselsCountByLandingSite
accessVesselsCountByLandingSite <- function(con){
  accessVesselsCountByLandingSiteFromDB(con)
}

#accessAvailableYears
accessAvailableYears <- function(con){
  accessAvailableYearsFromDB(con)
}

#accessLandingForms
accessLandingForms <- function(con, year){
  accessLandingFormsFromDB(con, year)
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
