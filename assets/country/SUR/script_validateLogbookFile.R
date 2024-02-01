#' @name validateLogbookFile
#' @aliases validateLogbookFile
#' @title validateLogbookFile
#' @description \code{validateLogbookFile} validate and convert data to upload (excel format) to sql file ready to upload
#'
#' @usage validateLogbookFile(filename, pool,ft_idx,fa_idx,fag_idx,fas_idx)
#'                 
#' @param filename the file path to the dataset
#' @param pool the db connection to access to the table
#' @param monitor parameter to trace stage of computation
#' 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}
#' @export
#'
#'    

validateLogbookFile <- function(filename, pool,monitor=NULL){
  
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
  
  data <- readxl::read_excel(if(is.character(filename)){filename}else{filename$datapath},col_types = "text")
  names(data)<-tolower(names(data))
  
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
    type=character(),
    description=character()
  )
  
  print(sprintf("Analysis of logbook data file '%s'",filename))
  
  #Checking integrity of the data file
  
  print("Cheching the validity of the document...")
  
  
  
  #validity of columns
  
  # check_col<-setdiff(c("trip_#","vessel_name","vessel_registration","departure_date","arrival_date","Time_spent_fishing","species_asfis","landed_weight_kg","processing","landing_site_code","Landing_site_name"),names(data))
  # 
  # if(length(check_col)==0|check_col=="processing"){
  #   print("Mandatory columns : Validated")
  # }else{
  #   err_msg<-sprintf("Mandatory columns : PROBLEM ... columns missing : %s",paste0(check_col,collapse = " ; "))
  #   stop(err_msg)
  # }
  
  #validity of ID column
  
  noID<-subset(as.data.frame(data),is.na(`trip_#`))
  if(nrow(noID)>0){
    err_msg<-sprintf("No Trip ID for row(s) :%s",paste0(row.names(noID),collapse = ","))
    #stop(err_msg)
    errors<<-rbind(errors,data.frame(trip_id="_",vessel_registration=noID$vessel_registration[1],type="ERROR",category="trip issue",message="missing trip number"))
    print(err_msg)
  }
  
  #validity of vessel registration
  
  noVesReg<-subset(as.data.frame(data),is.na(vessel_registration),select=`trip_#`)
  if(nrow(noVesReg)>0){
    err_msg<-sprintf("No Vessel Registration for row(s) :%s",paste0(noVesReg,collapse = ","))
    stop(err_msg)
  }
  
  #standardize vessel registration if need
  data$vessel_registration<-ifelse(nchar(data$vessel_registration)==5,
                                   paste0(substr(data$vessel_registration,1,2),"00",substr(data$vessel_registration,3,5)),data$vessel_registration)
  
  #Create unique ID of trip
  data$ID<-paste0(data$vessel_registration,"-",data$`trip_#`)
  
  
  
  #Check consistency between vessel name and registration number
  # 
  # test<- data%>%
  #   select(vessel_name,vessel_registration)%>%
  #   mutate(vessel_registration=paste0(substr(vessel_registration,1,2),"00",substr(vessel_registration,3,5)),vessel_registration)%>%
  #   distinct()%>%
  #   rowwise()%>%
  #   mutate(ref_vessel_name_by_Reg=dbGetQuery(pool, sprintf("SELECT NAME FROM reg_vessels where REGISTRATION_NUMBER = '%s'",vessel_registration))[1,1])%>%
  #   mutate(test=if(ref_vessel_name_by_Reg==vessel_name){TRUE}else{FALSE})%>%
  #   filter(isFALSE(test))
  # 
  # print(test)
  
  #standardize dates
  
  data$departure_date<-ifelse(!is.na(as.numeric(data$departure_date)),
                              as.character(as.Date(as.numeric(data$departure_date), origin = "1899-12-30")),
                              as.character(as.Date(data$departure_date, format="%d/%m/%Y"))
  )
  
  data$departure_date<-as.Date(data$departure_date,format = "%Y-%m-%d")
  data$departure_date<-format(data$departure_date, format = "%Y-%m-%d %H:%M:%S")
  
  data$arrival_date<-ifelse(!is.na(as.numeric(data$arrival_date)),
                            as.character(as.Date(as.numeric(data$arrival_date), origin = "1899-12-30")),
                            as.character(as.Date(data$arrival_date, format="%d/%m/%Y"))
  )
  
  data$arrival_date<-as.Date(data$arrival_date,format = "%Y-%m-%d")
  data$arrival_date<-format(data$arrival_date, format = "%Y-%m-%d %H:%M:%S")
  
  trips<-data%>%
    select(-c(species_asfis,landed_weight_kg,starts_with("processing")))%>%
    distinct()
  
  species<-data%>%
    select(`trip_#`,vessel_registration,ID,species_asfis,landed_weight_kg,starts_with("processing"))
  
  now<-as.character(format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"))
  comment<-paste0("Upload-from-logbook;file:",if(is.character(filename)){basename(filename)}else{filename$name},";")
  
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
    
    noDepDate<-trip[is.na(trip$departure_date),1]
    
    
    if(nrow(noDepDate)!=0){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="ERROR",category="date issue",message="missing or invalid departure date"))
    }
    
    if(length(unique(trip$departure_date))>1){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="ERROR",category="date issue",message="multiple departure date for a same trip"))
    }
    
    if(trip$departure_date[1]>Sys.Date()){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="ERROR",category="date issue",message="departure date is after present day"))
    }
    
    
    #Arrival Date
    
    noArrDate<-trip[is.na(trip$arrival_date),1]
    
    if(nrow(noArrDate)!=0){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="ERROR",category="date issue",message="missing or invalid arrival date"))
    }
    
    if(length(unique(trip$arrival_date))>1){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="ERROR",category="date issue",message="multiple arrival date for a same trip"))
    }
    
    if(trip$arrival_date[1]>Sys.Date()){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="ERROR",category="date issue",message="arrival date is after present day"))
    }
    
    #Trip duration
    trip_duration<-as.numeric(difftime(trip$arrival_date[1], trip$departure_date[1], units = "days"))
    trip_duration<-trip_duration+1
    
    if(trip_duration<0){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="ERROR",category="date issue",message="Arrival date must be greater than departure date"))
    }else if(trip_duration>365){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="ERROR",category="date issue",message=sprintf("Trip duration of '%s' seems so high",trip_duration)))
    }
    
    #Check consistency between vessel name and registration number
    ves_name_by_reg<-dbGetQuery(pool, sprintf("SELECT NAME FROM reg_vessels where REGISTRATION_NUMBER = '%s'",trip$vessel_registration[1]))
    clean_ves_name<-gsub("\\s+", " ", trip$vessel_name)
    if(nrow(ves_name_by_reg)>0){
      if(toupper(clean_ves_name)!=toupper(ves_name_by_reg)){
        clean_ves_name<-gsub("-"," ",clean_ves_name)
        if(toupper(clean_ves_name)!=toupper(ves_name_by_reg)){
          errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="WARNING",category="vessel issue",message=sprintf("Vessel registration '%s' in reg_vessel not corresponding to vessel name '%s' but '%s', please verify and modify the wrong value",trip$vessel_registration,trip$vessel_name,ves_name_by_reg)))
        }
      }
    }
    
    #Vessel registration referential
    vessel_id<-dbGetQuery(pool, sprintf("SELECT ID FROM reg_vessels where REGISTRATION_NUMBER = '%s'",trip$vessel_registration[1]))
    
    if(nrow(vessel_id)==0){
      referentials<<-rbind(referentials,data.frame(table="reg_vessels",value=trip$vessel_registration[1],type="ERROR",description=sprintf("Missing referential data for vessel. No value for '%s' in reg_vessels",trip$vessel_registration[1])))
    }
    
    #Trip identifier
    trip_identifier<-sprintf("%s-%s - %s - %s",substring(as.character(trip$arrival_date[1]),1,4),gsub("0", "", substring(as.character(trip$arrival_date[1]),6,7)),trip$vessel_registration[1],trip$`trip_#`[1])
    
    #Landing_site
    
    l_site_id<-dbGetQuery(pool, sprintf("SELECT ID FROM cl_fish_landing_sites where CODE = '%s'",trip$landing_site_code[1]))
    
    if(nrow(l_site_id)==0){
      referentials<<-rbind(referentials,data.frame(table="cl_fish_landing_sites",value=trip$landing_site_code[1],type="ERROR",description=sprintf("Missing referential data for landing site. No value for '%s' in cl_fish_landing_sites",trip$landing_site_code[1])))
    }
    
    if(length(unique(trip$landing_site_code))>1){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="WARNING",category="landing site issue",message="Multiple landing sites for a same trip. first selected"))
    }
    
    #Fishing Gear referential
    gear_id<-dbGetQuery(pool, sprintf("SELECT ID FROM cl_ref_gears where CODE = '%s'",trip$fishing_gear_code[1]))
    
    if(nrow(gear_id)==0){
      referentials<<-rbind(referentials,data.frame(table="cl_ref_gears",value=trip$fishing_gear_code[1],type="ERROR",description=sprintf("Missing referential data for gear. No value for '%s' in cl_ref_gears",trip$fishing_gear_code[1])))
    }
    
    if(length(unique(trip$fishing_gear_code))>1){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="WARNING",category="gear issue",message="Multiple gears for a same trip. first selected"))
    }
    
    #Fishing Zone referential
    f_zone_id<-dbGetQuery(pool, sprintf("SELECT ID FROM cl_fish_fishing_zones where CODE = '%s'",trip$fishing_zone_code[1]))
    
    if(nrow(f_zone_id)==0){
      referentials<<-rbind(referentials,data.frame(table="cl_fish_fishing_zones",value=trip$fishing_zone_code[1],type="ERROR",description=sprintf("Missing referential data for fishing zone. No value for '%s' in cl_fish_fishing_zones",trip$fishing_zone_code[1])))
    }
    
    if(length(unique(trip$fishing_zone_code))>1){
      errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="WARNING",category="fishing zone issue",message="Multiple fishing zones for a same trip. first selected"))
    }
    
    
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
    FT_DATE_FROM = trip$departure_date[1]
    FT_DATE_TO = trip$arrival_date[1]
    FT_CL_FROM_PORT_LOCATION_ID = l_site_id[1]
    FT_CL_TO_PORT_LOCATION_ID = l_site_id[1]
    FT_CL_FROM_PORT_SITE_ID = l_site_id[1]
    FT_CL_TO_PORT_SITE_ID = l_site_id[1]
    FT_CL_FISH_FISHING_ZONE_ID = f_zone_id[1]
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
    FA_DATE_FROM = trip$departure_date[1]
    FA_DATE_TO = trip$arrival_date[1]
    FA_UPDATER_ID = 2
    FA_COMMENT = comment
    FA_CREATED_AT = now
    FA_UPDATED_AT = now
    
    sql_fishing_activities <- sprintf("INSERT INTO dt_fishing_activities (`ID`,`DT_FISHING_TRIP_ID`,`CL_FISH_FISHING_ACTIVITY_TYPE_ID`,`DATE_FROM`,`DATE_TO`,`UPDATER_ID`,`COMMENT`,`CREATED_AT`,`UPDATED_AT`) VALUES (%s,%s,%s,'%s','%s',%s,'%s','%s','%s');\n", FA_ID, FA_DT_FISHING_TRIP_ID,FA_CL_FISH_FISHING_ACTIVITY_TYPE_ID,FA_DATE_FROM,FA_DATE_TO,FA_UPDATER_ID,FA_COMMENT, FA_CREATED_AT, FA_UPDATED_AT)
    
    ##dt_fishing_activities_gear
    
    FAG_ID = fag_idx
    FAG_DT_FISHING_ACTIVITY_ID = fa_idx 
    FAG_CL_REF_GEAR_ID = gear_id[1]
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
      if(is.na(sp$landed_weight_kg)){
        target<-subset(sp_register,Alpha3_Code==sp$species_asfis[1]) 
        sp_name<-sprintf("%s (%s - %s)",target$Alpha3_Code,target$Name_En,target$Scientific_Name)
        errors<<-rbind(errors,data.frame(trip_id=trip$`trip_#`[1],vessel_registration=trip$vessel_registration[1],type="ERROR",category="species issue",message=sprintf("Missing quantity value for species '%s'",sp_name)))
      }
      
      #test processing code presence
      
      processing_id<-1
      
      if(!"processing"%in%names(sp)){
        errors<<-rbind(errors,data.frame(trip_id=sp$`trip_#`[1],vessel_registration=sp$vessel_registration[1],type="WARNING",category="processing issue",message="Missing information about processing on species, no coefficient apply"))
        sp$landed_weight_kg_equivalent<-as.numeric(sp$landed_weight_kg[1])*1
      }else if(is.na(sp$processing[1])|sp$processing[1]==""){
          errors<<-rbind(errors,data.frame(trip_id=sp$`trip_#`[1],vessel_registration=sp$vessel_registration[1],type="WARNING",category="processing issue",message="Missing information about processing on species, no coefficient apply"))
          sp$landed_weight_kg_equivalent<-as.numeric(sp$landed_weight_kg[1])*1
      }else{
        processing_coef<-dbGetQuery(pool, sprintf("SELECT ID,COEFFICIENT_LIVE_WEIGHT FROM surcalipseo.cl_ref_fishery_products where CODE = '%s'",sp$processing))
        if(nrow(processing_coef)==0){
          referentials<<-rbind(referentials,data.frame(table="cl_ref_fishery_products",value=sp$processing[1],type="ERROR",description=sprintf("Missing referential data for species processing '%s'",sp$processing[1])))
          sp$landed_weight_kg_equivalent<-as.numeric(sp$landed_weight_kg[1])*1
        }else{
          sp$landed_weight_kg_equivalent<-as.numeric(sp$landed_weight_kg[1])*processing_coef$COEFFICIENT_LIVE_WEIGHT
          processing_id<-processing_coef$ID
        }
      }
      
      #Validate species in referential
      
      #test if species is already in table
      
      #test if species value
      if(is.na(sp$species_asfis[1])){
        errors<<-rbind(errors,data.frame(trip_id=sp$`trip_#`[1],vessel_registration=sp$vessel_registration[1],type="ERROR",category="species issue",message="Missing value for species"))
        sp_code<-NA  
      }else{
        
        #Test ASFIS_CODE
        sp_code<-dbGetQuery(pool, sprintf("SELECT ID FROM cl_ref_species where ASFIS_CODE = '%s'",sp$species_asfis))
        
        #Test SCIENTIFI_NAME
        if(nrow(sp_code)==0){
          sp_code<-dbGetQuery(pool, sprintf("SELECT ID FROM cl_ref_species where SCIENTIFIC_NAME = '%s'",paste(toupper(substr(sp$species_asfis, 1, 1)), tolower(substr(sp$species_asfis, 2, nchar(sp$species_asfis))), sep="")))
          if(nrow(sp_code)>0){
            sp_asfis_code<-dbGetQuery(pool, sprintf("SELECT ASFIS_CODE FROM cl_ref_species where SCIENTIFIC_NAME = '%s'",paste(toupper(substr(sp$species_asfis, 1, 1)), tolower(substr(sp$species_asfis, 2, nchar(sp$species_asfis))), sep="")))
            errors<<-rbind(errors,data.frame(trip_id=sp$`trip_#`[1],vessel_registration=sp$vessel_registration[1],type="WARNING",category="species issue",message=sprintf("Species is reported '%s' but should be reported '%s'",sp$species_asfis,sp_asfis_code)))
          }
        }
        
        #Test ASFIS_NAME_EN
        if(nrow(sp_code)==0){
          sp_code<-dbGetQuery(pool, sprintf("SELECT ID FROM cl_ref_species where ASFIS_NAME_EN = '%s'",paste(toupper(substr(sp$species_asfis, 1, 1)), tolower(substr(sp$species_asfis, 2, nchar(sp$species_asfis))), sep="")))
          if(nrow(sp_code)>0){
            sp_asfis_code<-dbGetQuery(pool, sprintf("SELECT ASFIS_CODE FROM cl_ref_species where SCIENTIFIC_NAME = '%s'",paste(toupper(substr(sp$species_asfis, 1, 1)), tolower(substr(sp$species_asfis, 2, nchar(sp$species_asfis))), sep="")))
            errors<<-rbind(errors,data.frame(trip_id=sp$`trip_#`[1],vessel_registration=sp$vessel_registration[1],type="WARNING",category="species issue",message=sprintf("Species is reported '%s' but should be reported '%s'",sp$species_asfis,sp_asfis_code)))
          }
        }
        
        #Uniqueness of referential
        if(nrow(sp_code)>1){
          referentials<<-rbind(referentials,data.frame(table="cl_ref_species",value=sp$species_asfis[1],type="WARNING",description=sprintf("Multiple referential data for species '%s': %s (first is choosed)",sp$species_asfis,paste0(sp_code$ID,collapse = ";"))))
        }
        
        #New species, add to referential
        if(nrow(sp_code)==0){
          if(nchar(sp$species_asfis[1])!=3){
            target<-subset(sp_register,Scientific_Name==paste(toupper(substr(sp$species_asfis, 1, 1)), tolower(substr(sp$species_asfis, 2, nchar(sp$species_asfis))), sep=""))
            if(nrow(target)==0){
              target<-subset(sp_register,Name_En==paste(toupper(substr(sp$species_asfis, 1, 1)), tolower(substr(sp$species_asfis, 2, nchar(sp$species_asfis))), sep=""))
              errors<<-rbind(errors,data.frame(trip_id=sp$`trip_#`[1],vessel_registration=sp$vessel_registration[1],type="WARNING",category="species issue",message=sprintf("Species '%s' not have yet be referred in referntial, please confirm no typo exist in name '%s'",sp$species_asfis,sp$species_asfis)))
            }
          }else{
            target<-subset(sp_register,Alpha3_Code==sp$species_asfis)  
          }
          
          if(nrow(target)==1){
            sp_name<-sprintf("%s (%s - %s)",target$Alpha3_Code,target$Name_En,target$Scientific_Name)
          }else{
            sp_name<-sp$species_asfis
          }
          
          referentials<<-rbind(referentials,data.frame(table="cl_ref_species",value=sp$species_asfis[1],type="ERROR",description=sprintf("Missing referential data for species '%s'",sp_name)))
        }
      }
      
      FAS_ID = fas_idx
      FAS_DT_FISHING_ACTIVITY_ID = fa_idx
      FAS_CL_REF_SPECIES_ID = sp_code[1]
      FAS_QUANTITY = sp$landed_weight_kg
      FAS_CL_APP_QUANTITY_UNIT_ID = 7
      FAS_TOTAL_VALUE = "null"
      FAS_UPDATER_ID = 2
      #FAS_COMMENT = paste0(comment,ifelse(any("processing"%in%names(sp)),paste0(";processing:",tolower(sp$processing)),""))
      FAS_COMMENT = comment
      FAS_CREATED_AT = now
      FAS_UPDATED_AT = now
      FAS_CATCH_NUMBER = "null"
      FAS_CL_REF_FISHERY_PRODUCT_ID = processing_id
      FAS_CL_REF_SPECIES_SIZE_ID = "null"
      FAS_CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT = sp$landed_weight_kg_equivalent
      FAS_DISCARD_QUANTITY = "null"
      FAS_CL_APP_DISCARD_QUANTITY_UNIT_ID = "null"
      FAS_TOTAL_VALUE_CURRENCY = "null"
      
      sql_fas_ind = sprintf("INSERT INTO dt_fishing_activities_species (`ID`,`DT_FISHING_ACTIVITY_ID`,`CL_REF_SPECIES_ID`,`QUANTITY`,`CL_APP_QUANTITY_UNIT_ID`,`TOTAL_VALUE`,`UPDATER_ID`,`COMMENT`,`CREATED_AT`,`UPDATED_AT`,`CATCH_NUMBER`,`CL_REF_FISHERY_PRODUCT_ID`,`CL_REF_SPECIES_SIZE_ID`,`CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT`,`DISCARD_QUANTITY`,`CL_APP_DISCARD_QUANTITY_UNIT_ID`,`TOTAL_VALUE_CURRENCY`) VALUES (%s,%s,%s,%s,%s,%s,%s,'%s','%s','%s',%s,%s,%s,%s,%s,%s,%s);\n",FAS_ID, FAS_DT_FISHING_ACTIVITY_ID, FAS_CL_REF_SPECIES_ID, FAS_QUANTITY, FAS_CL_APP_QUANTITY_UNIT_ID,FAS_TOTAL_VALUE,FAS_UPDATER_ID,FAS_COMMENT,FAS_CREATED_AT,FAS_UPDATED_AT,FAS_CATCH_NUMBER,FAS_CL_REF_FISHERY_PRODUCT_ID,FAS_CL_REF_SPECIES_SIZE_ID,FAS_CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT,FAS_DISCARD_QUANTITY,FAS_CL_APP_DISCARD_QUANTITY_UNIT_ID,FAS_TOTAL_VALUE_CURRENCY)
      
      return(sql_fas_ind)
    }),collapse='\n')
    
    return(paste(sql_note,sql_fishing_trip,sql_fishing_activities,sql_fishing_activities_gear,sql_fishing_activities_species,sep='\n'))
  }),collapse='\n')
  
  referentials<-unique(referentials)
  
  if(nrow(subset(referentials,type=="ERROR"))>0|nrow(subset(errors,type=="ERROR"))>0){
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


