#' @name convertTripToSQL
#' @aliases convertTripToSQL
#' @title convertTripToSQL
#' @description \code{convertTripToSQL} validate and convert data to upload (excel format) to sql file ready to upload
#'
#' @usage convertTripToSQL(filename, pool,ft_idx,fa_idx,fag_idx,fas_idx)
#'                 
#' @param filename the filepath to the dataset
#' @param pool the db connection to access to the table
#' @param ft_idx the last index number of dt_fishing_trip table
#' @param fa_idx the last index number of dt_fishing_activities
#' @param fag_idx the last index number of dt_fishing_activities_gear
#' @param fas_idx the last index number of dt_fishing_activities_species
#' 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}
#' @export
#'    
convertTripToSQL <- function(filename, pool,ft_idx,fa_idx,fag_idx,fas_idx){
  
data <- readxl::read_excel(filename)

print(sprintf("Analysis of datafile '%s",filname))

#Checking validity of data file

#validity of columns

print("Cheching the validity of document")

check_col<-setdiff(c("Trip_#","Vessel_Name","Vessel_Registration","Departure_date","Arrival_date","Time_spent_fishing","Species_ASFIS","Landed_weight_kg","Processing","Landing_site_code","Landing_site_name"),names(data))

if(length(check_col)==0|check_col=="Processing"){
print("Mandatory columns : Validated") 
}else{
print(sprintf("Mandatory columns : PROBLEM ... columns missing : %s",paste0(check_col,collapse = " ; "))) 
}

#valididy of cell content
for(i in 1:ncol(data)){
empty<-data[is.na(data[,i]),]

if(nrow(empty)!=0){
  print(paste0(sprintf("%s empty values detected in column '%s'",nrow(empty),names(data[,i]))))
  print(empty)
}
}

data$Vessel_Registration<-ifelse(nchar(data$Vessel_Registration)==5,
                                        paste0(substr(data$Vessel_Registration,1,2),"00",substr(data$Vessel_Registration,3,5)),data$Vessel_Registration)

if(is.numeric(data$Departure_date)){
data$Departure_date<-as.Date(data$Departure_date, origin = "1899-12-30")
}

if(is.numeric(data$Arrival_date)){
  data$Arrival_date<-as.Date(data$Arrival_date, origin = "1899-12-30")
}

data$identifier<-sprintf("%s-%s - %s - %s",format(data$Arrival_date, format = "%Y"),gsub(" 0", " ", format(data$Arrival_date, format = "%m")),data$Vessel_Registration,data$`Trip_#`)

trips<-data%>%
      select(-c(Species_ASFIS,Landed_weight_kg,starts_with("Processing")))%>%
      distinct()

species<-data%>%
          select(identifier,Species_ASFIS,Landed_weight_kg,starts_with("Processing"))

now<-as.character(format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"))
comment<-paste0("Upload-from-logbook;file:",basename(filename),";")

# By trip

data_sql<-paste(lapply(1:nrow(trips), function(i){
  
  trip<-trips[i,]
  trip_species<-subset(species,identifier==trip$identifier)
  
  ft_idx<<-ft_idx+1
  fa_idx<<-fa_idx+1
  fag_idx<<-fag_idx+1
  
  ##dt_fishing_trip
FT_ID = ft_idx
FT_REG_VESSEL_ID = dbGetQuery(pool, sprintf("SELECT ID FROM surcalipseo.reg_vessels where REGISTRATION_NUMBER = '%s'",trip$Vessel_Registration))
FT_REG_REPORTING_OFFICER_ID = 1 
FT_REG_CAPTAIN_ID = 1
FT_CL_FISH_FISHING_TRIP_TYPE_ID = 1 
FT_DATE_FROM = format(trip$Departure_date, format = "%Y-%m-%d %H:%M:%S")
FT_DATE_TO = format(trip$Arrival_date, format = "%Y-%m-%d %H:%M:%S")
FT_CL_FROM_PORT_LOCATION_ID = trip$Landing_site_code
FT_CL_TO_PORT_LOCATION_ID = trip$Landing_site_code
FT_CL_FROM_PORT_SITE_ID = trip$Landing_site_code
FT_CL_TO_PORT_SITE_ID = trip$Landing_site_code
FT_CL_FISH_FISHING_ZONE_ID = 1
FT_TIME_SPENT_FISHING_ZONE = as.character(difftime(trip$Arrival_date, trip$Departure_date, units = "days"))
FT_CL_TIME_SPENT_FISHING_UNIT_ID = 18
FT_TIME_SPENT_FISHING_IN_FISHING_ZONE = 'null'
FT_CL_TIME_SPENT_FISHING_ZONE_UNIT_ID = 'null'
FT_CREW_NUMBER = 'null' 
FT_IDENTIFIER = trip$identifier
FT_NUMBER_OF_DINGHLES = "null"
FT_FOOD_COST = "null"
FT_FUEL_AMOUNT = "null"
FT_FUEL_COST = "null"
FT_UPDATER_ID = 2 # Emmanuel Blondel
FT_COMMENT = comment
FT_CREATED_AT = now
FT_UPDATED_AT = now

sql_note <- sprintf("-- Fishing trip '%s'\n",trip$identifier)

sql_fishing_trip <- sprintf("INSERT INTO dt_fishing_trip(`ID`,`REG_VESSEL_ID`,`REG_REPORTING_OFFICER_ID`,`REG_CAPTAIN_ID`,`CL_FISH_FISHING_TRIP_TYPE_ID`,`DATE_FROM`,`DATE_TO`,`CL_FROM_PORT_LOCATION_ID`,`CL_TO_PORT_LOCATION_ID`,`CL_FROM_PORT_SITE_ID`,`CL_TO_PORT_SITE_ID`,`CL_FISH_FISHING_ZONE_ID`,`TIME_SPENT_FISHING_ZONE`,`CL_TIME_SPENT_FISHING_UNIT_ID`,`TIME_SPENT_FISHING_IN_FISHING_ZONE`,`CL_TIME_SPENT_FISHING_ZONE_UNIT_ID`,`CREW_NUMBER`,`IDENTIFIER`,`NUMBER_OF_DINGHLES`,`FOOD_COST`,`FUEL_AMOUNT`,`FUEL_COST`,`UPDATER_ID`,`COMMENT`,`CREATED_AT`,`UPDATED_AT`) VALUES (%s,%s,%s,%s,%s,'%s','%s',%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,'%s',%s,%s,%s,%s,%s,'%s','%s','%s');\n", FT_ID,FT_REG_VESSEL_ID,FT_REG_REPORTING_OFFICER_ID,FT_REG_CAPTAIN_ID,FT_CL_FISH_FISHING_TRIP_TYPE_ID,FT_DATE_FROM,FT_DATE_TO,FT_CL_FROM_PORT_LOCATION_ID,FT_CL_TO_PORT_LOCATION_ID,FT_CL_FROM_PORT_SITE_ID,FT_CL_TO_PORT_SITE_ID,FT_CL_FISH_FISHING_ZONE_ID,FT_TIME_SPENT_FISHING_ZONE,FT_CL_TIME_SPENT_FISHING_UNIT_ID,FT_TIME_SPENT_FISHING_IN_FISHING_ZONE,FT_CL_TIME_SPENT_FISHING_ZONE_UNIT_ID,FT_CREW_NUMBER,FT_IDENTIFIER,FT_NUMBER_OF_DINGHLES,FT_FOOD_COST,FT_FUEL_AMOUNT,FT_FUEL_COST,FT_UPDATER_ID,FT_COMMENT,FT_CREATED_AT,FT_UPDATED_AT)

##dt_fishing_activities

FA_ID = fa_idx
FA_DT_FISHING_TRIP_ID = ft_idx
FA_CL_FISH_FISHING_ACTIVITY_TYPE_ID = 1
FA_DATE_FROM = format(trip$Departure_date, format = "%Y-%m-%d %H:%M:%S")
FA_DATE_TO = format(trip$Arrival_date, format = "%Y-%m-%d %H:%M:%S")
FA_UPDATER_ID = 2
FA_COMMENT = comment
FA_CREATED_AT = now
FA_UPDATED_AT = now

sql_fishing_activities <- sprintf("INSERT INTO dt_fishing_activities (`ID`,`DT_FISHING_TRIP_ID`,`CL_FISH_FISHING_ACTIVITY_TYPE_ID`,`DATE_FROM`,`DATE_TO`,`UPDATER_ID`,`COMMENT`,`CREATED_AT`,`UPDATED_AT`) VALUES (%s,%s,%s,'%s','%s',%s,'%s','%s','%s');\n", FA_ID, FA_DT_FISHING_TRIP_ID,FA_CL_FISH_FISHING_ACTIVITY_TYPE_ID,FA_DATE_FROM,FA_DATE_TO,FA_UPDATER_ID,FA_COMMENT, FA_CREATED_AT, FA_UPDATED_AT)

##dt_fishing_activities_gear

FAG_ID = fag_idx
FAG_DT_FISHING_ACTIVITY_ID = fa_idx 
FAG_CL_REF_GEAR_ID = 2
FAG_UPDATER_ID = 2
FAG_COMMENT = comment
FAG_CREATED_AT = now
FAG_UPDATED_AT = now

sql_fishing_activities_gear <- sprintf("INSERT INTO dt_fishing_activities_gear (`ID`,`DT_FISHING_ACTIVITY_ID`,`CL_REF_GEAR_ID`,`UPDATER_ID`,`COMMENT`,`CREATED_AT`,`UPDATED_AT`) VALUES (%s,%s,%s,%s,'%s','%s','%s');\n", FAG_ID, FAG_DT_FISHING_ACTIVITY_ID, FAG_CL_REF_GEAR_ID, FAG_UPDATER_ID, FAG_COMMENT, FAG_CREATED_AT, FAG_UPDATED_AT)

##dt_fishing_activities_species

sql_fishing_activities_species <-paste(lapply(1:nrow(trip_species),function(j){
  
sp<-trip_species[j,]
  
fas_idx<<-fas_idx+1
  
FAS_ID = fas_idx
FAS_DT_FISHING_ACTIVITY_ID = fa_idx
FAS_CL_REF_SPECIES_ID = dbGetQuery(pool, sprintf("SELECT ID FROM surcalipseo.cl_ref_species where ASFIS_CODE = '%s'",sp$Species_ASFIS))
FAS_QUANTITY = sp$Landed_weight_kg
FAS_CL_APP_QUANTITY_UNIT_ID = 7
FAS_TOTAL_VALUE = "null"
FAS_UPDATER_ID = 2
FAS_COMMENT = paste0(comment,ifelse(any("processing"%in%names(sp)),paste0(";processing:",tolower(sp$Processing)),""))
FAS_CREATED_AT = now
FAS_UPDATED_AT = now
FAS_CATCH_NUMBER = "null"
FAS_CL_REF_SPECIES_SIZE_ID = "null"
FAS_CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT = sp$Landed_weight_kg
FAS_DISCARD_QUANTITY = "null"
FAS_CL_APP_DISCARD_QUANTITY_UNIT_ID = "null"
FAS_TOTAL_VALUE_CURRENCY = "null"

sql_fas_ind = sprintf("INSERT INTO dt_fishing_activities_species (`ID`,`DT_FISHING_ACTIVITY_ID`,`CL_REF_SPECIES_ID`,`QUANTITY`,`CL_APP_QUANTITY_UNIT_ID`,`TOTAL_VALUE`,`UPDATER_ID`,`COMMENT`,`CREATED_AT`,`UPDATED_AT`,`CATCH_NUMBER`,`CL_REF_SPECIES_SIZE_ID`,`CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT`,`DISCARD_QUANTITY`,`CL_APP_DISCARD_QUANTITY_UNIT_ID`,`TOTAL_VALUE_CURRENCY`) VALUES (%s,%s,%s,%s,%s,%s,%s,'%s','%s','%s',%s,%s,%s,%s,%s,%s);\n",FAS_ID, FAS_DT_FISHING_ACTIVITY_ID, FAS_CL_REF_SPECIES_ID, FAS_QUANTITY, FAS_CL_APP_QUANTITY_UNIT_ID,FAS_TOTAL_VALUE,FAS_UPDATER_ID,FAS_COMMENT,FAS_CREATED_AT,FAS_UPDATED_AT,FAS_CATCH_NUMBER,FAS_CL_REF_SPECIES_SIZE_ID,FAS_CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT,FAS_DISCARD_QUANTITY,FAS_CL_APP_DISCARD_QUANTITY_UNIT_ID,FAS_TOTAL_VALUE_CURRENCY)

return(sql_fas_ind)
}),collapse='\n')

return(paste(sql_note,sql_fishing_trip,sql_fishing_activities,sql_fishing_activities_gear,sql_fishing_activities_species,sep='\n'))
}),collapse='\n')

writeLines(data_sql, paste0(strsplit(basename(filename),".xlsx")[[1]],".sql"))
}



