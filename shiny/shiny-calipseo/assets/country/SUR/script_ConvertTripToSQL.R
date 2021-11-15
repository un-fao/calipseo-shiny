#' @name convertTripToSQL
#' @aliases convertTripToSQL
#' @title convertTripToSQL
#' @description \code{convertTripToSQL} validate and convert data to upload (excel format) to sql file ready to upload
#'
#' @usage convertTripToSQL(filename, pool,ft_idx,fa_idx,fag_idx,fas_idx)
#'                 
#' @param filename the file path to the dataset
#' @param pool the db connection to access to the table
#' @param monitor parameter to trace stage of computation
#' 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}
#' @export
#'    
convertTripToSQL <- function(filename, pool,monitor=NULL){
  
  ft_idx = 0
  last_ft = dbGetQuery(pool, "SELECT * FROM dt_fishing_trip")
  if(nrow(last_ft)>0) ft_idx = max(last_ft$ID)
  fa_idx = 0
  last_fa = dbGetQuery(pool, "SELECT * FROM dt_fishing_activities")
  if(nrow(last_fa)>0) fa_idx = max(last_fa$ID)
  fag_idx = 0
  last_fag = dbGetQuery(pool, "SELECT * FROM dt_fishing_activities_gear")
  if(nrow(last_fag)>0) fag_idx = max(last_fag$ID)
  fas_idx = 0
  last_fas = dbGetQuery(pool, "SELECT * FROM dt_fishing_activities_species")
  if(nrow(last_fas)>0) fas_idx = max(last_fas$ID)
  
  data <- readxl::read_excel(filename,col_types = "text")
  
  errors<-data.frame(
                     trip_id=character(),
                     vessel_registration=character(),
                     type=character(),
                     category=character(),
                     message=character()
                     )
  
  referentials<-data.frame(
    table=character(),
    value=character(),
    description=character()
  )
  
  print(sprintf("Analysis of logbook data file '%s'",filename))
  
  #Checking integrity of the data file
  
  print("Cheching the validity of the document...")
  
  #validity of columns
  
  # check_col<-setdiff(c("Trip_#","Vessel_Name","Vessel_Registration","Departure_date","Arrival_date","Time_spent_fishing","Species_ASFIS","Landed_weight_kg","Processing","Landing_site_code","Landing_site_name"),names(data))
  # 
  # if(length(check_col)==0|check_col=="Processing"){
  #   print("Mandatory columns : Validated") 
  # }else{
  #   err_msg<-sprintf("Mandatory columns : PROBLEM ... columns missing : %s",paste0(check_col,collapse = " ; "))
  #   stop(err_msg) 
  # }
  
  #validity of ID column
  
  noID<-subset(as.data.frame(data),is.na(`Trip_#`))
  if(nrow(noID)>0){
    err_msg<-sprintf("No Trip ID for row(s) :%s",paste0(row.names(noID),collapse = ","))
    #stop(err_msg)
    errors<<-rbind(errors,data.frame(trip_id="_",vessel_registration=noID$Vessel_Registration[1],type="ERROR",message="missing trip number"))
    print(err_msg)
  }
  
  #validity of vessel registration
  
  noVesReg<-subset(as.data.frame(data),is.na(Vessel_Registration),select=`Trip_#`)
  if(nrow(noVesReg)>0){
    err_msg<-sprintf("No Vessel Registration for row(s) :%s",paste0(noVesReg,collapse = ","))
    stop(err_msg)
  }
  
  #standardize vessel registration if need
  data$Vessel_Registration<-ifelse(nchar(data$Vessel_Registration)==5,
                                   paste0(substr(data$Vessel_Registration,1,2),"00",substr(data$Vessel_Registration,3,5)),data$Vessel_Registration)
  
  #Create unique ID of trip
  data$ID<-paste0(data$Vessel_Registration,"-",data$`Trip_#`)
  
  #Check consistency between vessel name and registration number
  # 
  # test<- data%>%
  #   select(Vessel_Name,Vessel_Registration)%>%
  #   mutate(Vessel_Registration=paste0(substr(Vessel_Registration,1,2),"00",substr(Vessel_Registration,3,5)),Vessel_Registration)%>%
  #   distinct()%>%
  #   rowwise()%>%
  #   mutate(ref_Vessel_Name_by_Reg=dbGetQuery(pool, sprintf("SELECT NAME FROM reg_vessels where REGISTRATION_NUMBER = '%s'",Vessel_Registration))[1,1])%>%
  #   mutate(test=if(ref_Vessel_Name_by_Reg==Vessel_Name){TRUE}else{FALSE})%>%
  #   filter(isFALSE(test))
  # 
  # print(test)
  
  #standardize dates
  
  data$Departure_date<-ifelse(!is.na(as.numeric(data$Departure_date)),
               as.character(as.Date(as.numeric(data$Departure_date), origin = "1899-12-30")),
               as.character(as.Date(data$Departure_date, format="%d/%m/%Y"))
  )
  
  data$Departure_date<-as.Date(data$Departure_date,format = "%Y-%m-%d")
  data$Departure_date<-format(data$Departure_date, format = "%Y-%m-%d %H:%M:%S")
  
  data$Arrival_date<-ifelse(!is.na(as.numeric(data$Arrival_date)),
                              as.character(as.Date(as.numeric(data$Arrival_date), origin = "1899-12-30")),
                              as.character(as.Date(data$Arrival_date, format="%d/%m/%Y"))
  )
  
  data$Arrival_date<-as.Date(data$Arrival_date,format = "%Y-%m-%d")
  data$Arrival_date<-format(data$Arrival_date, format = "%Y-%m-%d %H:%M:%S")
               
  trips<-data%>%
        select(-c(Species_ASFIS,Landed_weight_kg,starts_with("Processing")))%>%
        distinct()
  
  species<-data%>%
            select(`Trip_#`,Vessel_Registration,ID,Species_ASFIS,Landed_weight_kg,starts_with("Processing"))
  
  now<-as.character(format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"))
  comment<-paste0("Upload-from-logbook;file:",basename(filename),";")
  
  sp_register<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ITEM.csv", col_names = T)
  sp_register<-subset(sp_register,select=c('Alpha3_Code','Scientific_Name','Name_En'))
  
  # By trip
  
  step=0
  max=length(unique(trips$ID))
  
  data_sql<-paste(lapply(unique(trips$ID), function(i){
    
    
    step<<-step+1
    value<-step/max
    if(!is.null(monitor)) monitor(value=value,step=step,max=max,trip_id=i)
    
    print(sprintf("TRIP : %s",i))
    
    trip<-subset(trips,ID==i)
    
    trip_species<-subset(species,ID==i)
    
    #validity of cell content
    
    #Departure Date

    noDepDate<-trip[is.na(trip$Departure_date),1]
      
    if(nrow(noDepDate)!=0){
      errors<<-rbind(errors,data.frame(trip_id=trip$`Trip_#`[1],vessel_registration=trip$Vessel_Registration[1],type="ERROR",message="missing or invalid departure date"))
      }
    
    if(length(unique(trip$Departure_date))>1){
      errors<<-rbind(errors,data.frame(trip_id=trip$`Trip_#`[1],vessel_registration=trip$Vessel_Registration[1],type="WARNING",message="multiple departure date for a same trip. first selected"))
    }
    
    #Arrival Date
    
    noArrDate<-trip[is.na(trip$Arrival_date),1]
    
    if(nrow(noArrDate)!=0){
      errors<<-rbind(errors,data.frame(trip_id=trip$`Trip_#`[1],vessel_registration=trip$Vessel_Registration[1],type="ERROR",message="missing or invalid arrival date"))
    }
    
    if(length(unique(trip$Arrival_date))>1){
      errors<<-rbind(errors,data.frame(trip_id=trip$`Trip_#`[1],vessel_registration=trip$Vessel_Registration[1],type="WARNING",message="multiple arrival date for a same trip. first selected"))
    }
    
    #Trip duration
    trip_duration<-as.numeric(difftime(trip$Arrival_date[1], trip$Departure_date[1], units = "days"))
    
    if(trip_duration<0){
      errors<<-rbind(errors,data.frame(trip_id=trip$`Trip_#`[1],vessel_registration=trip$Vessel_Registration[1],type="ERROR",message="Arrival date must be greater than departure date"))
    }else if(trip_duration>365){
      errors<<-rbind(errors,data.frame(trip_id=trip$`Trip_#`[1],vessel_registration=trip$Vessel_Registration[1],type="ERROR",message=sprintf("Trip duration of '%s' seems so high",trip_duration)))
    }
    
    #Check consistency between vessel name and registration number
    ves_name_by_reg<-dbGetQuery(pool, sprintf("SELECT NAME FROM reg_vessels where REGISTRATION_NUMBER = '%s'",trip$Vessel_Registration))
    clean_ves_name<-gsub("\\s+", " ", trip$Vessel_Name)
    if(nrow(ves_name_by_reg)>0){
      if(toupper(clean_ves_name)!=toupper(ves_name_by_reg)){
        clean_ves_name<-gsub("-"," ",clean_ves_name)
        if(toupper(clean_ves_name)!=toupper(ves_name_by_reg)){
        errors<<-rbind(errors,data.frame(trip_id=trip$`Trip_#`[1],vessel_registration=trip$Vessel_Registration[1],type="ERROR",message=sprintf("Vessel registration '%s' in reg_vessel not corresponding to vessel name '%s' but '%s', please verify and modify the wrong value",trip$Vessel_Registration,trip$Vessel_Name,ves_name_by_reg)))
        }
      }
    }
    
    #Vessel registration referential
    vessel_id<-dbGetQuery(pool, sprintf("SELECT ID FROM reg_vessels where REGISTRATION_NUMBER = '%s'",trip$Vessel_Registration[1]))
    
    if(nrow(vessel_id)==0){
      referentials<<-rbind(referentials,data.frame(table="reg_vessels",value=trip$Vessel_Registration[1],description=sprintf("Missing referential data for vessel. No value for '%s' in reg_vessels",trip$Vessel_Registration[1])))
    }
    
    #Landing_site
    
    if(length(unique(trip$Landing_site_code))>1){
      errors<<-rbind(errors,data.frame(trip_id=trip$`Trip_#`[1],vessel_registration=trip$Vessel_Registration[1],type="WARNING",message="multiple landing site for a same trip. first selected"))
    }
    
    #Trip identifier
    trip_identifier<-sprintf(sprintf("%s-%s - %s - %s",format(trip$Arrival_date[1], format = "%Y"),gsub("0", "", format(trip$Arrival_date[1], format = "%m")),trip$Vessel_Registration[1],trip$`Trip_#`[1]))
    
    #Indexes incrementation
    ft_idx<<-ft_idx+1
    fa_idx<<-fa_idx+1
    fag_idx<<-fag_idx+1
    
    ##dt_fishing_trip
    FT_ID = ft_idx
    FT_REG_VESSEL_ID = vessel_id
    FT_REG_REPORTING_OFFICER_ID = 1 
    FT_REG_CAPTAIN_ID = 1
    FT_CL_FISH_FISHING_TRIP_TYPE_ID = 1 
    FT_DATE_FROM = trip$Departure_date[1]
    FT_DATE_TO = trip$Arrival_date[1]
    FT_CL_FROM_PORT_LOCATION_ID = trip$Landing_site_code[1]
    FT_CL_TO_PORT_LOCATION_ID = trip$Landing_site_code[1]
    FT_CL_FROM_PORT_SITE_ID = trip$Landing_site_code[1]
    FT_CL_TO_PORT_SITE_ID = trip$Landing_site_code[1]
    FT_CL_FISH_FISHING_ZONE_ID = 1
    FT_TIME_SPENT_FISHING_ZONE = trip_duration
    FT_CL_TIME_SPENT_FISHING_UNIT_ID = 18
    FT_TIME_SPENT_FISHING_IN_FISHING_ZONE = 'null'
    FT_CL_TIME_SPENT_FISHING_ZONE_UNIT_ID = 'null'
    FT_CREW_NUMBER = 'null' 
    FT_IDENTIFIER = trip_identifier
    FT_NUMBER_OF_DINGHLES = "null"
    FT_FOOD_COST = "null"
    FT_FUEL_AMOUNT = "null"
    FT_FUEL_COST = "null"
    FT_UPDATER_ID = 2 # Emmanuel Blondel
    FT_COMMENT = comment
    FT_CREATED_AT = now
    FT_UPDATED_AT = now
  
  sql_note <- sprintf("-- Fishing trip '%s'\n",trip_identifier)
  
  sql_fishing_trip <- sprintf("INSERT INTO dt_fishing_trip(`ID`,`REG_VESSEL_ID`,`REG_REPORTING_OFFICER_ID`,`REG_CAPTAIN_ID`,`CL_FISH_FISHING_TRIP_TYPE_ID`,`DATE_FROM`,`DATE_TO`,`CL_FROM_PORT_LOCATION_ID`,`CL_TO_PORT_LOCATION_ID`,`CL_FROM_PORT_SITE_ID`,`CL_TO_PORT_SITE_ID`,`CL_FISH_FISHING_ZONE_ID`,`TIME_SPENT_FISHING_ZONE`,`CL_TIME_SPENT_FISHING_UNIT_ID`,`TIME_SPENT_FISHING_IN_FISHING_ZONE`,`CL_TIME_SPENT_FISHING_ZONE_UNIT_ID`,`CREW_NUMBER`,`IDENTIFIER`,`NUMBER_OF_DINGHLES`,`FOOD_COST`,`FUEL_AMOUNT`,`FUEL_COST`,`UPDATER_ID`,`COMMENT`,`CREATED_AT`,`UPDATED_AT`) VALUES (%s,%s,%s,%s,%s,'%s','%s',%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,'%s',%s,%s,%s,%s,%s,'%s','%s','%s');\n", FT_ID,FT_REG_VESSEL_ID,FT_REG_REPORTING_OFFICER_ID,FT_REG_CAPTAIN_ID,FT_CL_FISH_FISHING_TRIP_TYPE_ID,FT_DATE_FROM,FT_DATE_TO,FT_CL_FROM_PORT_LOCATION_ID,FT_CL_TO_PORT_LOCATION_ID,FT_CL_FROM_PORT_SITE_ID,FT_CL_TO_PORT_SITE_ID,FT_CL_FISH_FISHING_ZONE_ID,FT_TIME_SPENT_FISHING_ZONE,FT_CL_TIME_SPENT_FISHING_UNIT_ID,FT_TIME_SPENT_FISHING_IN_FISHING_ZONE,FT_CL_TIME_SPENT_FISHING_ZONE_UNIT_ID,FT_CREW_NUMBER,FT_IDENTIFIER,FT_NUMBER_OF_DINGHLES,FT_FOOD_COST,FT_FUEL_AMOUNT,FT_FUEL_COST,FT_UPDATER_ID,FT_COMMENT,FT_CREATED_AT,FT_UPDATED_AT)
  
  ##dt_fishing_activities
  
  FA_ID = fa_idx
  FA_DT_FISHING_TRIP_ID = ft_idx
  FA_CL_FISH_FISHING_ACTIVITY_TYPE_ID = 1
  FA_DATE_FROM = trip$Departure_date[1]
  FA_DATE_TO = trip$Arrival_date[1]
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
  
  #Validate quantity value  
  if(is.na(sp$Landed_weight_kg)){
    target<-subset(sp_register,Alpha3_Code==sp$Species_ASFIS[1]) 
    sp_name<-sprintf("%s (%s - %s)",target$Alpha3_Code,target$Name_En,target$Scientific_Name)
    errors<<-rbind(errors,data.frame(trip_id=trip$`Trip_#`[1],vessel_registration=trip$Vessel_Registration[1],type="ERROR",message=sprintf("Missing quantity value for species '%s'",sp_name)))
  }
  
  #Validate species in referential

#test if species is already in table
  
  #test if species value
  if(is.na(sp$Species_ASFIS[1])){
    errors<<-rbind(errors,data.frame(trip_id=sp$`Trip_#`[1],vessel_registration=sp$Vessel_Registration[1],type="ERROR",message="Missing value for species"))
    sp_code<-NA  
  }else{
      
    #Test ASFIS_CODE
    sp_code<-dbGetQuery(pool, sprintf("SELECT ID FROM cl_ref_species where ASFIS_CODE = '%s'",sp$Species_ASFIS))
    
    #Test SCIENTIFI_NAME
    if(nrow(sp_code)==0){
      sp_code<-dbGetQuery(pool, sprintf("SELECT ID FROM cl_ref_species where SCIENTIFIC_NAME = '%s'",paste(toupper(substr(sp$Species_ASFIS, 1, 1)), tolower(substr(sp$Species_ASFIS, 2, nchar(sp$Species_ASFIS))), sep="")))
      if(nrow(sp_code)>0){
        sp_asfis_code<-dbGetQuery(pool, sprintf("SELECT ASFIS_CODE FROM cl_ref_species where SCIENTIFIC_NAME = '%s'",paste(toupper(substr(sp$Species_ASFIS, 1, 1)), tolower(substr(sp$Species_ASFIS, 2, nchar(sp$Species_ASFIS))), sep="")))
        errors<<-rbind(errors,data.frame(trip_id=sp$`Trip_#`[1],vessel_registration=sp$Vessel_Registration[1],type="WARNING",message=sprintf("Species is reported '%s' but should be reported '%s'",sp$Species_ASFIS,sp_asfis_code)))
      }
    }
  
    #Test ASFIS_NAME_EN
    if(nrow(sp_code)==0){
      sp_code<-dbGetQuery(pool, sprintf("SELECT ID FROM cl_ref_species where ASFIS_NAME_EN = '%s'",paste(toupper(substr(sp$Species_ASFIS, 1, 1)), tolower(substr(sp$Species_ASFIS, 2, nchar(sp$Species_ASFIS))), sep="")))
      if(nrow(sp_code)>0){
        sp_asfis_code<-dbGetQuery(pool, sprintf("SELECT ASFIS_CODE FROM cl_ref_species where SCIENTIFIC_NAME = '%s'",paste(toupper(substr(sp$Species_ASFIS, 1, 1)), tolower(substr(sp$Species_ASFIS, 2, nchar(sp$Species_ASFIS))), sep="")))
        errors<<-rbind(errors,data.frame(trip_id=sp$`Trip_#`[1],vessel_registration=sp$Vessel_Registration[1],type="WARNING",message=sprintf("Species is reported '%s' but should be reported '%s'",sp$Species_ASFIS,sp_asfis_code)))
      }
    }
    
    #Uniqueness of referential
    if(nrow(sp_code)>1){
      referentials<<-rbind(referentials,data.frame(table="cl_ref_species",value=sp$Species_ASFIS[1],description=sprintf("Multiple referential data for species '%s' : ",sp$Species_ASFIS,paste0(sp_code$ID,collapse = ";"))))
    }
  
    #New species, add to referential
    if(nrow(sp_code)==0){
      if(nchar(sp$Species_ASFIS[1])!=3){
        target<-subset(sp_register,Scientific_Name==paste(toupper(substr(sp$Species_ASFIS, 1, 1)), tolower(substr(sp$Species_ASFIS, 2, nchar(sp$Species_ASFIS))), sep=""))
        if(nrow(target)==0){
          target<-subset(sp_register,Name_En==paste(toupper(substr(sp$Species_ASFIS, 1, 1)), tolower(substr(sp$Species_ASFIS, 2, nchar(sp$Species_ASFIS))), sep=""))
          errors<<-rbind(errors,data.frame(trip_id=sp$`Trip_#`[1],vessel_registration=sp$Vessel_Registration[1],type="WARNING",message=sprintf("Species '%s' not have yet be referred in referntial, please confirm no typo exist in name '%s'",sp$Species_ASFIS,sp$Species_ASFIS)))
        }
      }else{
        target<-subset(sp_register,Alpha3_Code==sp$Species_ASFIS)  
      }
      
      if(nrow(target)==1){
          sp_name<-sprintf("%s (%s - %s)",target$Alpha3_Code,target$Name_En,target$Scientific_Name)
      }else{
          sp_name<-sp$Species_ASFIS
        }
  
      referentials<<-rbind(referentials,data.frame(table="cl_ref_species",value=sp$Species_ASFIS[1],description=sprintf("Missing referential data for species '%s'",sp_name)))
    }
  }
  
  FAS_ID = fas_idx
  FAS_DT_FISHING_ACTIVITY_ID = fa_idx
  FAS_CL_REF_SPECIES_ID = sp_code[1]
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
  
  referentials<-unique(referentials)
  
  if(nrow(referentials)>0|nrow(subset(errors,type=="ERROR"))>0){
    valid<-FALSE
  }else{
    valid<-TRUE
  }
  out = list(
    result = data_sql,
    referentials = referentials,
    errors = errors,
    valid = valid
  )
  
  return(out)
}




