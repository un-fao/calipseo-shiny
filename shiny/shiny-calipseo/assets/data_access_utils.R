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
    if(is.null(datetime_field))
      stop("Specify a datetime field in your sql statement to filter on year")
    sql <- sprintf("%s %s date_part('year', %s) = %s", 
                   sql, ifelse(regexpr("WHERE", sql)>0, "AND", "WHERE"), datetime_field, add_filter_on_year)
  }
  
  return(sql)
}

#accessVesselsFromDB
accessVesselsFromDB <- function(con){
  vessels_sql <- readSQLScript("data/sql/vessels.sql")
  vessels <- dbGetQuery(con, vessels_sql)
  return(vessels)
}

#accessVesselsCountFromDB
accessVesselsCountFromDB <- function(con){
  vessels_count_sql <- readSQLScript("data/sql/vessels_count.sql")
  vessels_count <- dbGetQuery(con, vessels_count_sql)
  return(vessels_count$COUNT)
}

#accessVesselFromDB
accessVesselFromDB <- function(con, registrationNumber){
  vessel_sql <- readSQLScript("data/sql/vessels.sql", key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  vessel <- dbGetQuery(con, vessel_sql)
  return(vessel)
}

#accessVesselOwnersFromDB
accessVesselOwnersFromDB <- function(con, registrationNumber = NULL){
  vessel_owners_sql <- readSQLScript("data/sql/vessels_owners.sql",
                                     key = if(!is.null(registrationNumber)) "vo.REG_VESSEL_ID" else NULL,
                                     value = if(!is.null(registrationNumber)) paste("'", registrationNumber, "'") else NULL
  )
  vessel_owners <- dbGetQuery(con, vessel_owners_sql)
  return(vessel_owners)
}

#accessVesselCatchesFromDB
accessVesselCatchesFromDB <- function(con, registrationNumber){
  landing_forms_sql <- readSQLScript("data/sql/fishing_activities.sql", key = "v.REGISTRATION_NUMBER", value = paste0("'", registrationNumber, "'"))
  landing_forms <- dbGetQuery(con, landing_forms_sql)
  return(landing_forms)
}

#generic data callers (considering this needs to be replaced later by API calls)

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
