validateArtisanalFile <- function(filename, pool,monitor=NULL){
  
data <- readxl::read_excel(if(is.character(filename)){filename}else{filename$datapath},sheet="catch data",col_types = "text")

#data<-readxl::read_excel("C:/Users/alexa/Downloads/ART_LAND_2024_2025_check2.xlsx",sheet="catch data",col_types = "text")

#source("D:/Calipseo/importCalipseoModel.R")
#importCalipseoModel(pool,db_name="surcalipseo")

if(!is.null(monitor))monitor(0.1,"Loading of calipseo model")
calipseo<-calipseoManagerR::CalipseoModelManager$new(pool)

#Global setting
updater="11"
comment=""
now<-as.character(format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"))


errors<-data.frame(
  row=integer(),
  column=character(),
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

#CL
new_cl_app_quantity_units<-calipseo$load_table(table_name ="cl_app_quantity_units")
new_cl_ref_species<-calipseo$load_table(table_name ="cl_ref_species")
new_cl_ref_gears<-calipseo$load_table(table_name ="cl_ref_gears")
new_cl_ref_currencies<-calipseo$load_table(table_name ="cl_ref_currencies")
new_cl_ref_fishery_products<-calipseo$load_table(table_name ="cl_ref_fishery_products")
new_cl_stat_effort_survey_types<-calipseo$load_table(table_name ="cl_stat_effort_survey_types")
new_cl_fish_landing_sites<-calipseo$load_table(table_name ="cl_fish_landing_sites")
new_cl_fish_fishing_zones<-calipseo$load_table(table_name ="cl_fish_fishing_zones")
new_cl_fish_fishing_units<-calipseo$load_table(table_name ="cl_fish_fishing_units")
new_cl_fish_fishing_trip_types<-calipseo$load_table(table_name ="cl_fish_fishing_trip_types")
new_cl_fish_fishing_activities_types<-calipseo$load_table(table_name ="cl_fish_fishing_activities_types")

#REG
new_reg_entities<-calipseo$load_table(table_name ="reg_entities")
new_reg_entity_individuals<-calipseo$load_table(table_name ="reg_entity_individuals")
new_reg_vessels<-calipseo$load_table(table_name ="reg_vessels")
new_reg_vessel_gears<-calipseo$load_table(table_name ="reg_vessel_gears")

#DT
new_dt_fishing_trip<-calipseo$load_table(table_name ="dt_fishing_trip")
new_dt_fishing_activities<-calipseo$load_table(table_name ="dt_fishing_activities")
new_dt_fishing_activities_gear<-calipseo$load_table(table_name ="dt_fishing_activities_gear")
new_dt_fishing_activities_species<-calipseo$load_table(table_name ="dt_fishing_activities_species")
new_dt_fishing_activity_gear_characteristics<-calipseo$load_table(table_name ="dt_fishing_activity_gear_characteristics")
new_dt_effort_survey<-calipseo$load_table(table_name ="dt_effort_survey")

new_custom_reg_entity_individuals<-list()
new_custom_reg_entity_individuals$VIEW_DB_TABLE<-function(){
new_reg_entity_individuals$VIEW_DB_TABLE()%>%select(ID, REG_ENTITY_ID, FIRST_NAME, MIDDLE_NAME, SUFFIX_NAME)%>%
  left_join(new_reg_entities$VIEW_DB_TABLE()%>%select(ID,NAME),by=c("REG_ENTITY_ID"="ID"))%>%
  mutate(FULL_NAME=paste(FIRST_NAME, MIDDLE_NAME, SUFFIX_NAME, NAME, sep=" ")%>% gsub("\\s+", " ", .) %>%trimws())%>%
  ungroup()%>%
  rename(REG_INDIVIDUAL_ID=ID)
}

#Preload of DB tables 
calipseo_cl_ref_species<-new_cl_ref_species$VIEW_DB_TABLE()
calipseo_reg_vessels<-new_reg_vessels$VIEW_DB_TABLE()
calipseo_cl_ref_currencies<-new_cl_ref_currencies$VIEW_DB_TABLE()
calipseo_cl_fish_landing_sites<-new_cl_fish_landing_sites$VIEW_DB_TABLE()
calipseo_cl_fish_fishing_units<-new_cl_fish_fishing_units$VIEW_DB_TABLE()
calipseo_custom_reg_entity_individuals<-new_custom_reg_entity_individuals$VIEW_DB_TABLE()
calipseo_cl_app_quantity_units<-new_cl_app_quantity_units$VIEW_DB_TABLE()
calipseo_cl_ref_fishery_products<-new_cl_ref_fishery_products$VIEW_DB_TABLE()
calipseo_cl_stat_effort_survey_types<-new_cl_stat_effort_survey_types$VIEW_DB_TABLE()
calipseo_cl_fish_fishing_trip_types<-new_cl_fish_fishing_trip_types$VIEW_DB_TABLE()
calipseo_cl_fish_fishing_activities_types<-new_cl_fish_fishing_activities_types$VIEW_DB_TABLE()
calipseo_cl_fish_fishing_zones<-new_cl_fish_fishing_zones$VIEW_DB_TABLE()
calipseo_cl_ref_gears<-new_cl_ref_gears$VIEW_DB_TABLE()

#GENERIC FUNCTION
normalize_name <- function(x) {
  x %>%
    tolower() %>%                               
    stringi::stri_trans_general("Latin-ASCII") %>% 
    gsub("[[:punct:]]", "", .) %>%              
    gsub("\\s+", " ", .) %>%                    
    trimws()                                    
}

mapping_table<-function(value,db_table,match_column="NAME",return_column="ID",exact=TRUE){
  
  table<-get(paste0("calipseo_",db_table))
  
  if(exact){
    result<-table %>%
      filter(.data[[match_column]] == value) %>%
      pull(!!sym(return_column))
  }else{
    result<-table %>%
      filter(normalize_name(.data[[match_column]]) == normalize_name(value)) %>%
      pull(!!sym(return_column))
  }
  
  
  if(length(result)==0){
    print(sprintf("No match for value '%s'",value))
    return(NA)
  }else{
    return(result)
  }
}

apply_product_factor<-function(value,x,column="ID"){
  factor<-new_cl_ref_fishery_products$VIEW_DB_TABLE() %>%
    filter(.data[[column]] == x) %>%
    pull(COEFFICIENT_LIVE_WEIGHT)
  
  return(value*factor)
  
}

convert_dates <- function(date) {
  
  # If date is at format jj.mm.yyyy or jj.m.yyyy
  if (grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$", date)) {
    return(as.Date(date, format = "%d.%m.%Y"))
  }
  
  # If date is at format jj/mm/yyyy or jj/m/yyyy
  if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date)) {
    return(as.Date(date, format = "%d/%m/%Y"))
  }
  
  # ife date is a number (Excel format)
  if (!is.na(as.numeric(date))) {
    return(as.Date(as.numeric(date)- 2, origin = "1900-01-01"))
  }
  
  # If date is on unrecognized format
  return(date)
}

print(sprintf("Analysis of data file '%s'",filename))

#Checking integrity of the data file

print("Cheching the validity of the document...")

if(!is.null(monitor))monitor(0.1,"Normalize dataset")
#Global data cleaning
data<-data%>%
  mutate(across(everything(), ~ na_if(., "-9999")),
         across(everything(), ~ na_if(., "NA")))%>%
  rowwise()%>%
  mutate(
    survey_type=ifelse(startsWith(vessel_registration_nr,"BV"),"INTERVIEW5",ifelse(startsWith(vessel_registration_nr,"SK"),"INTERVIEW30",NA)),
    recording_date=convert_dates(recording_date),
    departure_date=convert_dates(departure_date),
    arrival_date=convert_dates(arrival_date),
    unit_time_spent_fishing=ifelse(unit_time_spent_fishing=="Hours","Hour",ifelse(unit_time_spent_fishing=="Days","Day",unit_time_spent_fishing))
  )%>%
  ungroup()%>%
  mutate(row_id = row_number())%>%
  relocate(row_id,.before = everything())

## "identifier"
validate_identifier<-function(data,monitor){
  print(sprintf("Validating column : 'identifier'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'identifier'")
  
  
  data%>%
    select(row_id,identifier)%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(identifier) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(identifier) & (!is.character(identifier)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",identifier)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="identifier")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "vessel_registration_nr"
validate_vessel_registration<-function(data,monitor){
  print(sprintf("Validating column : 'vessel_registration_nr'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'vessel_registration_nr'")
  data%>%
    select(row_id,vessel_registration_nr,vessel_name)%>%
    left_join(calipseo_reg_vessels%>%select(ID,REGISTRATION_NUMBER,NAME)%>%group_by(REGISTRATION_NUMBER)%>%
                summarise(n=length(ID),NAME=NAME[1],ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("vessel_registration_nr"="REGISTRATION_NUMBER"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(vessel_registration_nr) ~ 
          list(list(
            type="WARNING",
            category="Missing value",
            message="Vessel missing"
          )),
        !is.na(vessel_registration_nr) & (!is.character(vessel_registration_nr)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",vessel_registration_nr)
          )),
        !is.na(vessel_registration_nr) & (!is.character(vessel_registration_nr)) & grepl("^(BV|SK)",vessel_registration_nr) ~ 
          list(list(
            type="WARNING",
            category="Invalid format",
            message=sprintf("Value '%s' should start by 'BV' or 'SK'",vessel_registration_nr)
          )),
        !is.na(vessel_registration_nr) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",vessel_registration_nr,"reg_vessels")
          )),
        !is.na(vessel_registration_nr) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value'%s' return multiple ID (%s) in '%s'",vessel_registration_nr,ID,"reg_vessels")
          )),
        !is.na(vessel_registration_nr) & !is.na(ID) & !is.na(vessel_name) & normalize_name(vessel_name)!=normalize_name(NAME) ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' refers to '%s' in '%s' and not '%s'",vessel_registration_nr,NAME,"reg_vessels",vessel_name)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="vessel_registration_nr")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "departure_site_name"
validate_departure_site_name<-function(data,monitor){
  print(sprintf("Validating column : 'departure_site_name'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'departure_site_name'")
  data%>%
    select(row_id,departure_site_name)%>%
    left_join(calipseo_cl_fish_landing_sites%>%select(ID,NAME)%>%group_by(NAME)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("departure_site_name"="NAME"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(departure_site_name) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(departure_site_name) & (!is.character(departure_site_name)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",departure_site_name)
          )),
        !is.na(departure_site_name) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",departure_site_name,"cl_fish_landing_sites")
          )),
        !is.na(departure_site_name) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value'%s' return multiple ID (%s) in '%s'",departure_site_name,ID,"cl_fish_landing_sites")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="departure_site_name")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "landing_site_name"
validate_landing_site_name<-function(data,monitor){
  print(sprintf("Validating column : 'landing_site_name'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'landing_site_name'")
  data%>%
    select(row_id,landing_site_name)%>%
    left_join(calipseo_cl_fish_landing_sites%>%select(ID,NAME)%>%group_by(NAME)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("landing_site_name"="NAME"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(landing_site_name) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(landing_site_name) & (!is.character(landing_site_name)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",landing_site_name)
          )),
        !is.na(landing_site_name) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",landing_site_name,"cl_fish_landing_sites")
          )),
        !is.na(landing_site_name) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value'%s' return multiple ID (%s) in '%s'",landing_site_name,ID,"cl_fish_landing_sites")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="landing_site_name")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "fishing_unit_name" 
validate_fishing_unit_name<-function(data,monitor){
  print(sprintf("Validating column : 'fishing_unit_name'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'fishing_unit_name'")
  data%>%
    select(row_id,fishing_unit_name)%>%
    left_join(calipseo_cl_fish_fishing_units%>%select(ID,NAME)%>%group_by(NAME)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("fishing_unit_name"="NAME"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(fishing_unit_name) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(fishing_unit_name) & (!is.character(fishing_unit_name)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",fishing_unit_name)
          )),
        !is.na(fishing_unit_name) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",fishing_unit_name,"cl_fish_fishing_units")
          )),
        !is.na(fishing_unit_name) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value'%s' return multiple ID (%s) in '%s'",fishing_unit_name,ID,"cl_fish_fishing_units")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="fishing_unit_name")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "gear_name" 
validate_gear_name<-function(data,monitor){
  print(sprintf("Validating column : 'gear_name'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'gear_name'")
  data%>%
    select(row_id,gear_name)%>%
    left_join(calipseo_cl_ref_gears%>%select(ID,NAME)%>%group_by(NAME)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("gear_name"="NAME"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(gear_name) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(gear_name) & (!is.character(gear_name)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",gear_name)
          )),
        !is.na(gear_name) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",gear_name,"cl_ref_gears")
          )),
        !is.na(gear_name) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value'%s' return multiple ID (%s) in '%s'",gear_name,ID,"cl_ref_gears")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="gear_name")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "fishing_zone_name" 
validate_fishing_zone_name<-function(data,monitor){
  print(sprintf("Validating column : 'fishing_zone_name'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'fishing_zone_name'")
  
  data%>%
    select(row_id,fishing_zone_name)%>%
    left_join(calipseo_cl_fish_fishing_zones%>%select(ID,NAME)%>%group_by(NAME)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("fishing_zone_name"="NAME"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(fishing_zone_name) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(fishing_zone_name) & (!is.character(fishing_zone_name)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",fishing_zone_name)
          )),
        !is.na(fishing_zone_name) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",fishing_zone_name,"cl_fish_fishing_zones")
          )),
        !is.na(fishing_zone_name) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value'%s' return multiple ID (%s) in '%s'",fishing_zone_name,ID,"cl_fish_fishing_zones")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="fishing_zone_name")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "recorder_name" 
validate_recorder_name<-function(data,monitor){
  print(sprintf("Validating column : 'recorder_name'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'recorder_name'")
  data%>%
    select(row_id,recorder_name)%>%
    mutate(n_recorder_name=normalize_name(recorder_name))%>%
    left_join(calipseo_custom_reg_entity_individuals%>%select(REG_INDIVIDUAL_ID,FULL_NAME)%>%mutate(FULL_NAME=normalize_name(FULL_NAME))%>%group_by(FULL_NAME)%>%
                summarise(n=length(REG_INDIVIDUAL_ID),REG_INDIVIDUAL_ID=paste0(REG_INDIVIDUAL_ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("n_recorder_name"="FULL_NAME"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(recorder_name) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(recorder_name) & (!is.character(recorder_name)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",recorder_name)
          )),
        !is.na(recorder_name) & is.na(REG_INDIVIDUAL_ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",recorder_name,"reg_entity_individuals")
          )),
        !is.na(recorder_name) & !is.na(REG_INDIVIDUAL_ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value'%s' return multiple ID (%s) in '%s'",recorder_name,REG_INDIVIDUAL_ID,"reg_entity_individuals")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="recorder_name")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "species_code"  
validate_species<-function(data,monitor){
  print(sprintf("Validating column : 'species'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'species'")
  data%>%
    select(row_id,species_code,species_name_scientific,landed_weight_kg)%>%
    left_join(calipseo_cl_ref_species%>%select(ID,ASFIS_CODE,SCIENTIFIC_NAME)%>%group_by(ASFIS_CODE)%>%
                summarise(n=length(ID),SCIENTIFIC_NAME=SCIENTIFIC_NAME[1],ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("species_code"="ASFIS_CODE"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(species_code) & !is.na(landed_weight_kg) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message=sprintf("Required value in %s when %s is present","species_code","landed_weight_kg")
          )),
        !is.na(species_code) & (!is.character(species_code) | !grepl("^[A-Z]{3}$", species_code)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value'%s' is not a valid format",species_code)
          )),
        !is.na(species_code) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",species_code,"cl_ref_species")
          )),
        !is.na(species_code) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value '%s' return multiple ID (%s) in '%s'",species_code,ID,"cl_ref_species")
          )),
        !is.na(species_code) & !is.na(ID) & !is.na(species_name_scientific) & normalize_name(species_name_scientific)!=normalize_name(SCIENTIFIC_NAME) ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' refers to '%s' in '%s' and not '%s'",species_code,SCIENTIFIC_NAME,"cl_ref_species",species_name_scientific)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="species_code")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "fish_product_code"
validate_fish_product_code<-function(data,monitor){
  print(sprintf("Validating column : 'fish_product_code'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'fish_product_code'")
  data%>%
    select(row_id,fish_product_code,species_code)%>%
    left_join(calipseo_cl_ref_fishery_products%>%select(ID,CODE)%>%group_by(CODE)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("fish_product_code"="CODE"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(fish_product_code) & !is.na(species_code) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message=sprintf("Required value in %s when %s is present","fish_product_code","species_code")
          )),
        !is.na(fish_product_code) & !is.character(fish_product_code) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value'%s' is not a valid format",fish_product_code)
          )),
        !is.na(fish_product_code) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",fish_product_code,"cl_ref_fishery_products")
          )),
        !is.na(fish_product_code) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value '%s' return multiple ID (%s) in '%s'",fish_product_code,ID,"cl_ref_fishery_products")
          )),
        !is.na(fish_product_code) & !is.na(ID) & fish_product_code!="N/A" & !is.na(species_code) & substr(fish_product_code,1,3) != species_code ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' refers to '%s' in '%s' and not '%s'",fish_product_code,substr(fish_product_code,1,3),"cl_ref_species",species_code)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="fish_product_code")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "currency_catch_price" 
validate_currency_catch_price<-function(data,monitor){
  print(sprintf("Validating column : 'currency_catch_price'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'currency_catch_price'")
  data%>%
    select(row_id,currency_catch_price,catch_price)%>%
    left_join(calipseo_cl_ref_currencies%>%select(ID,CODE)%>%group_by(CODE)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("currency_catch_price"="CODE"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(currency_catch_price) & !is.na(catch_price) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message=sprintf("Required value in %s when %s is present","currency_catch_price","catch_price")
          )),
        !is.na(currency_catch_price) & (!is.character(currency_catch_price)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value ''%s' is not a valid format",currency_catch_price)
          )),
        !is.na(currency_catch_price) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",currency_catch_price,"cl_ref_currencies")
          )),
        !is.na(currency_catch_price) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value '%s' return multiple ID (%s) in '%s'",currency_catch_price,ID,"cl_ref_currencies")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="currency_catch_price")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "currency_food_cost" 
validate_currency_food_cost<-function(data,monitor){
  print(sprintf("Validating column : 'currency_food_cost'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'currency_food_cost'")
  data%>%
    select(row_id,currency_food_cost,food_cost)%>%
    left_join(calipseo_cl_ref_currencies%>%select(ID,CODE)%>%group_by(CODE)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("currency_food_cost"="CODE"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(currency_food_cost) & !is.na(food_cost) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message=sprintf("Required value in %s when %s is present","currency_food_cost","food_cost")
          )),
        !is.na(currency_food_cost) & (!is.character(currency_food_cost)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value ''%s' is not a valid format",currency_food_cost)
          )),
        !is.na(currency_food_cost) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",currency_food_cost,"cl_ref_currencies")
          )),
        !is.na(currency_food_cost) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value '%s' return multiple ID (%s) in '%s'",currency_food_cost,ID,"cl_ref_currencies")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="currency_food_cost")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "currency_fuel_price" 
validate_currency_fuel_price<-function(data,monitor){
  print(sprintf("Validating column : 'currency_fuel_price'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'currency_fuel_price'")
  data%>%
    select(row_id,currency_fuel_price,fuel_price)%>%
    left_join(calipseo_cl_ref_currencies%>%select(ID,CODE)%>%group_by(CODE)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("currency_fuel_price"="CODE"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(currency_fuel_price) & !is.na(fuel_price) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message=sprintf("Required value in %s when %s is present","currency_fuel_price","fuel_price")
          )),
        !is.na(currency_fuel_price) & (!is.character(currency_fuel_price)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value ''%s' is not a valid format",currency_fuel_price)
          )),
        !is.na(currency_fuel_price) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",currency_fuel_price,"cl_ref_currencies")
          )),
        !is.na(currency_fuel_price) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value '%s' return multiple ID (%s) in '%s'",currency_fuel_price,ID,"cl_ref_currencies")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="currency_fuel_price")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "currency_ice_price" 
validate_currency_ice_price<-function(data,monitor){
  print(sprintf("Validating column : 'currency_ice_price'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'currency_ice_price'")
  data%>%
    select(row_id,currency_ice_price,ice_price)%>%
    left_join(calipseo_cl_ref_currencies%>%select(ID,CODE)%>%group_by(CODE)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("currency_ice_price"="CODE"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(currency_ice_price) & !is.na(ice_price) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message=sprintf("Required value in %s when %s is present","currency_ice_price","ice_price")
          )),
        !is.na(currency_ice_price) & (!is.character(currency_ice_price)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value ''%s' is not a valid format",currency_ice_price)
          )),
        !is.na(currency_ice_price) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",currency_ice_price,"cl_ref_currencies")
          )),
        !is.na(currency_ice_price) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value '%s' return multiple ID (%s) in '%s'",currency_ice_price,ID,"cl_ref_currencies")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="currency_ice_price")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "unit_time_spent_fishing" 
validate_unit_time_spent_fishing<-function(data,monitor){
  print(sprintf("Validating column : 'unit_time_spent_fishing'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'unit_time_spent_fishing'")
  data%>%
    select(row_id,unit_time_spent_fishing,time_spent_fishing)%>%
    left_join(calipseo_cl_app_quantity_units%>%select(ID,NAME)%>%group_by(NAME)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("unit_time_spent_fishing"="NAME"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(unit_time_spent_fishing) & !is.na(time_spent_fishing) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message=sprintf("Required value in %s when %s is present","unit_time_spent_fishing","time_spent_fishing")
          )),
        !is.na(unit_time_spent_fishing) & (!is.character(unit_time_spent_fishing)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value ''%s' is not a valid format",unit_time_spent_fishing)
          )),
        !is.na(unit_time_spent_fishing) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",unit_time_spent_fishing,"cl_app_quantity_units")
          )),
        !is.na(unit_time_spent_fishing) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value '%s' return multiple ID (%s) in '%s'",unit_time_spent_fishing,ID,"cl_app_quantity_units")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="unit_time_spent_fishing")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "time_spent_fishing" 
validate_time_spent_fishing<-function(data,monitor){
  print(sprintf("Validating column : 'time_spent_fishing'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'time_spent_fishing'")
  data%>%
    select(row_id,time_spent_fishing)%>%
    rowwise()%>%
    mutate(n_time_spent_fishing=suppressWarnings(as.numeric(time_spent_fishing)))%>%
    mutate(
      validation = case_when(
        is.na(time_spent_fishing) & !is.na(time_spent_fishing) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(time_spent_fishing) & is.na(n_time_spent_fishing) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",time_spent_fishing)
          )),
        !is.na(time_spent_fishing) & !is.na(n_time_spent_fishing) & n_time_spent_fishing<=0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative or null",time_spent_fishing)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="time_spent_fishing")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "landed_weight_kg" 
validate_landed_weight_kg<-function(data,monitor){
  print(sprintf("Validating column : 'landed_weight_kg'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'landed_weight_kg'")
  data%>%
    select(row_id,landed_weight_kg,species_code)%>%
    rowwise()%>%
    mutate(n_landed_weight_kg=suppressWarnings(as.numeric(landed_weight_kg)))%>%
    mutate(
      validation = case_when(
        is.na(landed_weight_kg) & !is.na(species_code) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message=sprintf("Required value in %s when %s is present","landed_weight_kg","species_code")
          )),
        !is.na(landed_weight_kg) & is.na(n_landed_weight_kg) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",landed_weight_kg)
          )),
        !is.na(landed_weight_kg) & !is.na(n_landed_weight_kg) & n_landed_weight_kg<0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative",landed_weight_kg)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="landed_weight_kg")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "catch_price" 
validate_catch_price<-function(data,monitor){
  print(sprintf("Validating column : 'catch_price'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'catch_price'")
  data%>%
    select(row_id,catch_price)%>%
    rowwise()%>%
    mutate(n_catch_price=suppressWarnings(as.numeric(catch_price)))%>%
    mutate(
      validation = case_when(
        !is.na(catch_price) & is.na(n_catch_price) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",catch_price)
          )),
        !is.na(catch_price) & !is.na(n_catch_price) & n_catch_price<0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative",catch_price)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="catch_price")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "food_cost" 
validate_food_cost<-function(data,monitor){
  print(sprintf("Validating column : 'food_cost'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'food_cost'")
  data%>%
    select(row_id,food_cost)%>%
    rowwise()%>%
    mutate(n_food_cost=suppressWarnings(as.numeric(food_cost)))%>%
    mutate(
      validation = case_when(
        !is.na(food_cost) & is.na(n_food_cost) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",food_cost)
          )),
        !is.na(food_cost) & !is.na(n_food_cost) & n_food_cost<0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative",food_cost)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="food_cost")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "fuel_consumed_liter" 
validate_fuel_consumed_liter<-function(data,monitor){
  print(sprintf("Validating column : 'fuel_consumed_liter'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'fuel_consumed_liter'")
  data%>%
    select(row_id,fuel_consumed_liter)%>%
    rowwise()%>%
    mutate(n_fuel_consumed_liter=suppressWarnings(as.numeric(fuel_consumed_liter)))%>%
    mutate(
      validation = case_when(
        !is.na(fuel_consumed_liter) & is.na(n_fuel_consumed_liter) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",fuel_consumed_liter)
          )),
        !is.na(fuel_consumed_liter) & !is.na(n_fuel_consumed_liter) & n_fuel_consumed_liter<0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative",fuel_consumed_liter)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="fuel_consumed_liter")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "fuel_price" 
validate_fuel_price<-function(data,monitor){
  print(sprintf("Validating column : 'fuel_price'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'fuel_price'")
  data%>%
    select(row_id,fuel_price)%>%
    rowwise()%>%
    mutate(n_fuel_price=suppressWarnings(as.numeric(fuel_price)))%>%
    mutate(
      validation = case_when(
        !is.na(fuel_price) & is.na(n_fuel_price) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",fuel_price)
          )),
        !is.na(fuel_price) & !is.na(n_fuel_price) & n_fuel_price<0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative",fuel_price)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="fuel_price")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "ice_bought_m3" 
validate_ice_bought_m3<-function(data,monitor){
  print(sprintf("Validating column : 'ice_bought_m3'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'ice_bought_m3'")
  data%>%
    select(row_id,ice_bought_m3)%>%
    rowwise()%>%
    mutate(n_ice_bought_m3=suppressWarnings(as.numeric(ice_bought_m3)))%>%
    mutate(
      validation = case_when(
        !is.na(ice_bought_m3) & is.na(n_ice_bought_m3) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",ice_bought_m3)
          )),
        !is.na(ice_bought_m3) & !is.na(n_ice_bought_m3) & n_ice_bought_m3<0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative",ice_bought_m3)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="ice_bought_m3")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "ice_price" 
validate_ice_price<-function(data,monitor){
  print(sprintf("Validating column : 'ice_price'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'ice_price'")
  data%>%
    select(row_id,ice_price)%>%
    rowwise()%>%
    mutate(n_ice_price=suppressWarnings(as.numeric(ice_price)))%>%
    mutate(
      validation = case_when(
        !is.na(ice_price) & is.na(n_ice_price) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",ice_price)
          )),
        !is.na(ice_price) & !is.na(n_ice_price) & n_ice_price<0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative",ice_price)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="ice_price")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "nr_fishers" 
validate_nr_fishers<-function(data,monitor){
  print(sprintf("Validating column : 'nr_fishers'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'nr_fishers'")
  data%>%
    select(row_id,nr_fishers)%>%
    rowwise()%>%
    mutate(n_nr_fishers=suppressWarnings(as.integer(nr_fishers)))%>%
    mutate(
      validation = case_when(
        !is.na(nr_fishers) & is.na(n_nr_fishers) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",nr_fishers)
          )),
        !is.na(nr_fishers) & !is.na(n_nr_fishers) & n_nr_fishers<0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative",nr_fishers)
          )),
        !is.na(nr_fishers) & !is.na(n_nr_fishers) & n_nr_fishers>20 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is abnormally high",nr_fishers)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="nr_fishers")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "boat_activity_days" 
validate_boat_activity_days<-function(data,monitor){
  print(sprintf("Validating column : 'boat_activity_days'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'boat_activity_days'")
  data%>%
    select(row_id,boat_activity_days)%>%
    rowwise()%>%
    mutate(n_boat_activity_days=suppressWarnings(as.integer(boat_activity_days)))%>%
    mutate(
      validation = case_when(
        is.na(boat_activity_days) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(boat_activity_days) & is.na(n_boat_activity_days) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",boat_activity_days)
          )),
        !is.na(boat_activity_days) & !is.na(n_boat_activity_days) & n_boat_activity_days<0 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is negative",boat_activity_days)
          )),
        !is.na(boat_activity_days) & !is.na(n_boat_activity_days) & n_boat_activity_days>31 ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is over '31'",boat_activity_days)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="boat_activity_days")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "departure_date" 
validate_departure_date<-function(data,monitor){
  print(sprintf("Validating column : 'departure_date'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'departure_date'")
  data%>%
    select(row_id,departure_date,arrival_date)%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(departure_date) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(departure_date) & (!is.Date(departure_date)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",departure_date)
          )),
        !is.na(departure_date) & (is.Date(departure_date)) & departure_date > today() ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is in the future",departure_date)
          )),
        !is.na(departure_date) & (is.Date(departure_date)) & (!is.na(arrival_date)) & (is.Date(departure_date)) & departure_date > arrival_date  ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' should not be after %s '%s'",departure_date,"arrival_date",arrival_date)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="departure_date")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "arrival_date" 
validate_arrival_date<-function(data,monitor){
  print(sprintf("Validating column : 'arrival_date'"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'arrival_date'")
  data%>%
    select(row_id,departure_date,arrival_date)%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(arrival_date) ~ 
          list(list(
            type="ERROR",
            category="Missing mandatory value",
            message="Required value"
          )),
        !is.na(arrival_date) & (!is.Date(arrival_date)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value '%s' is not a valid format",arrival_date)
          )),
        !is.na(arrival_date) & (is.Date(arrival_date)) & arrival_date > today() ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' is in the future",arrival_date)
          )),
        !is.na(arrival_date) & (is.Date(arrival_date)) & (!is.na(departure_date)) & (is.Date(departure_date)) & arrival_date < departure_date  ~ 
          list(list(
            type="WARNING",
            category="Incoherent value",
            message=sprintf("Value '%s' should not be before %s '%s'",arrival_date,"departure_date",departure_date)
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="arrival_date")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "survey_type" 
validate_survey_type<-function(data,monitor){
  print(sprintf("Validating column : 'survey_type' (derived from vessel_registration_nr)"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'survey_type' (derived from vessel_registration_nr)")
  data%>%
    select(row_id,survey_type)%>%
    left_join(calipseo_cl_stat_effort_survey_types%>%select(ID,CODE)%>%group_by(CODE)%>%
                summarise(n=length(ID),ID=paste0(ID,collapse=";"))%>%
                ungroup()%>%
                mutate(DUPLICATE=ifelse(n>1,TRUE,FALSE)),by=c("survey_type"="CODE"))%>%
    rowwise()%>%
    mutate(
      validation = case_when(
        is.na(survey_type) ~ 
          list(list(
            type="WARNING",
            category="Missing value",
            message="Survey type not deducted due to invalid registration_nr format"
          )),
        !is.na(survey_type) & (!is.character(survey_type)) ~ 
          list(list(
            type="ERROR",
            category="Invalid format",
            message=sprintf("Value ''%s' is not a valid format",survey_type)
          )),
        !is.na(survey_type) & is.na(ID) ~ 
          list(list(
            type="ERROR",
            category="No match with reference table",
            message=sprintf("Value '%s' not found in reference table '%s'",survey_type,"cl_stat_effort_survey_types")
          )),
        !is.na(survey_type) & !is.na(ID) & DUPLICATE ~ 
          list(list(
            type="ERROR",
            category="Multiple match with reference table",
            message=sprintf("Value '%s' return multiple ID (%s) in '%s'",survey_type,ID,"cl_stat_effort_survey_types")
          )),
        TRUE ~ 
          list(list(
            type="VALID",
            category=NA_character_,
            message=NA_character_
          ))
      )
    )%>%
    ungroup() %>%
    unnest_wider(validation)%>%
    mutate(column="survey_type")%>%
    select(row_id,column,type,category,message)%>%
    rename(row=row_id)
}

## "landed_weight_kg" (Duplicate landing for a same combination)
validate_quantity_duplicates <- function(data,monitor) {
  print(sprintf("Validating column : 'landed_weight_kg' (unicity check)"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'landed_weight_kg' (unicity check)")
  
  all_cols <- c("identifier", "fishing_unit_name", "departure_date", "arrival_date",
                "landing_site_name", "fishing_zone_name", "species_code",
                "processing_name", "fish_product_code", "landed_weight_kg")
  
  data %>%
    mutate(duplicate_key = do.call(paste, c(across(all_of(all_cols)), sep = "_"))) %>%
    group_by(duplicate_key) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    distinct(row_id) %>%
    mutate(
      row = row_id,
      column = "landed_weight_kg",
      type = "WARNING",
      category = "duplicate",
      message = "Multiple same entry for this combination trip - species - product - quantity"
    ) %>%
    select(row, column, type, category, message)
}

## "landed_weight_kg" (Multiple different landing for a same combination)
validate_conflict_quantity <- function(data,monitor) {
  print(sprintf("Validating column : 'landed_weight_kg' (conflict pseudo duplicate)"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'landed_weight_kg' (conflict pseudo duplicate)")
  base_cols <- c("identifier", "fishing_unit_name", "departure_date", "arrival_date",
                 "landing_site_name", "fishing_zone_name", "species_code",
                 "processing_name", "fish_product_code")
  
  data %>%
    mutate(combination_key = do.call(paste, c(across(all_of(base_cols)), sep = "_"))) %>%
    group_by(combination_key) %>%
    filter(n_distinct(landed_weight_kg) > 1) %>%
    ungroup() %>%
    distinct(row_id) %>%
    mutate(
      row = row_id,
      column = "landed_weight_kg",
      type = "WARNING",
      category = "conflict values",
      message = "Conflict different quantity for this combination trip - species - product"
    ) %>%
    select(row, column, type, category, message)
}

## "identifier" (Same identifier for multiple combination)
validate_conflict_identifier <- function(data,monitor) {
  print(sprintf("Validating column : 'identifier' (conflict in identifier)"))
  if(!is.null(monitor))monitor(0.01,"Validating column : 'identifier' (conflict in identifier)")
  
  sub_data <- data %>%
    select(identifier, row_id, recorder_name, vessel_registration_nr, boat_activity_days,
           fishing_unit_name, departure_date, arrival_date, departure_site_name,
           landing_site_name, fishing_zone_name, nr_fishers, food_cost,
           currency_food_cost, ice_bought_m3, ice_price, currency_ice_price,
           fuel_consumed_liter, fuel_price, currency_fuel_price) %>%
    distinct()
  
  other_cols <- setdiff(names(sub_data), c("identifier", "row_id"))
  
  sub_data_conflict <- sub_data %>%
    group_by(identifier) %>%
    filter(n() > 1) %>%
    summarise(
      problem_columns = {
        current_data <- cur_data_all()
        problems <- purrr::map_chr(
          other_cols,
          function(col) {
            vals <- unique(current_data[[col]])
            if (length(vals) > 1) paste0(col, "(", length(vals), ")") else NA_character_
          }
        )
        paste(na.omit(problems), collapse = "; ")
      },
      .groups = "drop"
    ) %>%
    filter(problem_columns != "")  # filtrer les cas où aucun conflit n'a été détecté
  
  if (nrow(sub_data_conflict) == 0) return(NULL)
  
  data %>%
    semi_join(sub_data_conflict, by = "identifier") %>%
    left_join(sub_data_conflict, by = "identifier") %>%
    mutate(
      type = "ERROR",
      category = "conflict value",
      column = "identifier",
      message = sprintf(
        "conflict different values for a same identifier '%s' : %s",
        identifier,
        problem_columns
      )
    ) %>%
    select(row = row_id, column, type, category, message)
}

#Run and merge validation_function
validation_functions <- list(
  identifier = validate_identifier,
  conflict_identifier = validate_conflict_identifier,
  recorder_name = validate_recorder_name,
  landing_site_name = validate_landing_site_name,
  vessel_registration_nr = validate_vessel_registration,
  survey_type = validate_survey_type,
  fishing_unit_name = validate_fishing_unit_name,
  gear_name = validate_gear_name,
  fishing_zone_name = validate_fishing_zone_name,
  departure_date = validate_departure_date,
  arrival_date = validate_arrival_date,
  time_spent_fishing = validate_time_spent_fishing,
  unit_time_spent_fishing = validate_unit_time_spent_fishing,
  boat_activity_days = validate_boat_activity_days,
  species_code = validate_species,
  fish_product_code = validate_fish_product_code,
  landed_weight_kg = validate_landed_weight_kg,
  quantity_duplicates = validate_quantity_duplicates,
  conflict_quantity = validate_conflict_quantity,
  catch_price = validate_catch_price,
  currency_catch_price = validate_currency_catch_price,
  nr_fishers = validate_nr_fishers,
  food_cost = validate_food_cost,
  currency_food_cost = validate_currency_food_cost,
  fuel_consumed_liter = validate_fuel_consumed_liter,
  fuel_price = validate_fuel_price,
  currency_fuel_price = validate_currency_fuel_price,
  ice_bought_m3 = validate_ice_bought_m3,
  ice_price = validate_ice_price,
  currency_ice_price = validate_currency_ice_price,
  departure_site_name = validate_departure_site_name
)

errors <- purrr::imap_dfr(validation_functions, function(f, name) {
  tryCatch(
    {
      f(data,monitor)
    },
    error = function(e) {
      tibble::tibble(
        row = NA_integer_,
        column = name,
        type = "ERROR",
        category = "Validation crash",
        message = sprintf("Validation '%s' failed: %s", name, e$message)
      )
    }
  )
})


# #Create summary table
# type_levels <- c("VALID", "WARNING", "ERROR")
# 
# summary_table <- errors %>%
#   filter(!is.na(column)) %>%
#   mutate(type = factor(type, levels = type_levels, ordered = TRUE)) %>%
#   group_by(column, row) %>%
#   summarise(
#     status = type[which.max(as.integer(type))], 
#     .groups = "drop"
#   ) %>%
#   count(column, status) %>%
#   pivot_wider(
#     names_from = status,
#     values_from = n,
#     values_fill = 0
#   ) %>%
#   mutate(
#     VALID = if (!"VALID" %in% names(.)) 0 else VALID,
#     WARNING = if (!"WARNING" %in% names(.)) 0 else WARNING,
#     ERROR = if (!"ERROR" %in% names(.)) 0 else ERROR,
#     TOTAL = VALID + WARNING + ERROR
#   ) %>%
#   select(column, VALID, WARNING, ERROR,TOTAL) %>%
#   arrange(column) %>%
#   as.data.frame()

validate_referential <- function(data, data_col, ref_table, ref_code_col, ref_id_col, table_name, normalize = FALSE,monitor) {
  
  if(!is.null(monitor))monitor(0.01,sprintf("checking referential %s for column :%s",data_col,table_name))
  
  get_norm <- function(x) normalize_name(x)
  
  data_vals <- data[[data_col]]
  ref_vals  <- ref_table[[ref_code_col]]
  
  if (normalize) {
    data_df <- tibble(value = data_vals, key = get_norm(data_vals)) %>% filter(!is.na(key))
    ref_df  <- tibble(key = get_norm(ref_vals)) %>% filter(!is.na(key))
    
    new_keys <- setdiff(unique(data_df$key), unique(ref_df$key))
    missing_values <- data_df %>% filter(key %in% new_keys) %>% distinct(value)
  } else {
    ref_values_clean <- unique(na.omit(ref_vals))
    missing_values <- tibble(value = setdiff(unique(na.omit(data_vals)), ref_values_clean))
  }
  
  df_missing <- missing_values %>%
    mutate(
      table = table_name,
      type = "missing",
      description = sprintf("Value '%s' not found in reference table '%s'", value, table_name)
    ) %>%
    select(table, value, type, description)
  
  if (normalize) {
    ref_norm <- ref_table %>%
      mutate(norm_key = get_norm(.data[[ref_code_col]])) %>%
      filter(norm_key %in% data_df$key)
    
    dup_ref <- ref_norm %>%
      group_by(norm_key) %>%
      summarise(
        n = n_distinct(.data[[ref_id_col]]),
        ID_list = paste(unique(.data[[ref_id_col]]), collapse = ", "),
        value = first(.data[[ref_code_col]]),
        .groups = "drop"
      ) %>%
      filter(n > 1)
  } else {
    dup_ref <- ref_table %>%
      filter(.data[[ref_code_col]] %in% na.omit(data_vals)) %>%
      group_by(code = .data[[ref_code_col]]) %>%
      summarise(
        n = n_distinct(.data[[ref_id_col]]),
        ID_list = paste(unique(.data[[ref_id_col]]), collapse = ", "),
        value = first(code),
        .groups = "drop"
      ) %>%
      filter(n > 1)
  }
  
  df_duplicate <- dup_ref %>%
    mutate(
      table = table_name,
      type = "duplicate",
      description = sprintf("Value '%s' returns multiple IDs in reference table '%s' (%s)", value, table_name, ID_list)
    ) %>%
    select(table, value, type, description)
  
  bind_rows(df_missing, df_duplicate)
}

referentials <- bind_rows(
  validate_referential(data, "vessel_registration_nr", calipseo_reg_vessels, "REGISTRATION_NUMBER", "ID", "reg_vessels",monitor = monitor),
  validate_referential(data, "survey_type", calipseo_cl_stat_effort_survey_types, "CODE", "ID", "cl_stat_effort_survey_types",monitor = monitor),
  validate_referential(data, "species_code", calipseo_cl_ref_species, "ASFIS_CODE", "ID", "cl_ref_species",monitor = monitor),
  validate_referential(data, "fish_product_code", calipseo_cl_ref_fishery_products, "CODE", "ID", "cl_ref_fishery_products",monitor = monitor),
  validate_referential(data, "departure_site_name", calipseo_cl_fish_landing_sites, "NAME", "ID", "cl_fish_landing_sites",TRUE,monitor = monitor),
  validate_referential(data, "landing_site_name", calipseo_cl_fish_landing_sites, "NAME", "ID", "cl_fish_landing_sites",TRUE,monitor = monitor),
  validate_referential(data, "fishing_unit_name", calipseo_cl_fish_fishing_units, "NAME", "ID", "cl_fish_fishing_units",monitor = monitor),
  validate_referential(data, "fishing_zone_name", calipseo_cl_fish_fishing_zones, "NAME", "ID", "cl_fish_fishing_zones",monitor = monitor),
  validate_referential(data, "recorder_name", calipseo_custom_reg_entity_individuals, "FULL_NAME", "REG_INDIVIDUAL_ID", "reg_entity_individuals",TRUE,monitor = monitor),
  validate_referential(data, "unit_time_spent_fishing", calipseo_cl_app_quantity_units, "NAME", "ID", "cl_app_quantity_units",TRUE,monitor = monitor),
  validate_referential(data, "currency_catch_price", calipseo_cl_ref_currencies, "CODE", "ID", "cl_ref_currencies",monitor = monitor),
  validate_referential(data, "currency_food_cost", calipseo_cl_ref_currencies, "CODE", "ID", "cl_ref_currencies",monitor = monitor),
  validate_referential(data, "currency_fuel_price", calipseo_cl_ref_currencies, "CODE", "ID", "cl_ref_currencies",monitor = monitor),
  validate_referential(data, "currency_ice_price", calipseo_cl_ref_currencies, "CODE", "ID", "cl_ref_currencies",monitor = monitor),
  validate_referential(data, "gear_name", calipseo_cl_ref_gears, "NAME", "ID", "cl_ref_gears",monitor = monitor)
)%>%
  distinct()


# summary_ref_table <- referentials %>%
#   filter(!is.na(table)) %>%
#   group_by(table, type) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   pivot_wider(
#     names_from = type,
#     values_from = n,
#     values_fill = list(n = 0)
#   ) %>%
#   mutate(
#     MISSING = if (!"missing" %in% names(.)) 0 else missing,
#     DUPLICATE = if (!"duplicate" %in% names(.)) 0 else duplicate,
#   ) %>%
#   select(table, MISSING,DUPLICATE) %>%
#   arrange(table) %>%
#   as.data.frame()
  

data_sql<-c()
message=NULL
if(nrow(subset(referentials,type=="ERROR"))>0|nrow(subset(errors,type=="ERROR"))>0){
  print("Not Valid")
  valid<-FALSE
  message<-"Your data file is not valid."
}else{
  print("Valid")
  valid<-TRUE
  message<-"Your data file is valid.\n\nThe SQL script will insert the following entries:\n\n"
}

if(valid){
#Generate SQL

  data_effort<-data%>%
    select(identifier,recorder_name,vessel_registration_nr,boat_activity_days,fishing_unit_name,recorder_name,departure_date,arrival_date,time_spent_fishing,unit_time_spent_fishing,departure_site_name,landing_site_name,fishing_zone_name,nr_fishers,food_cost,currency_food_cost,ice_bought_m3,ice_price,currency_ice_price,fuel_consumed_liter,fuel_price,currency_fuel_price,survey_type,notes)%>%
    distinct()%>%
    select(arrival_date,survey_type,boat_activity_days,landing_site_name,fishing_unit_name)%>%
    filter(!is.na(survey_type))%>%
    rowwise()%>%
    mutate(year=year(arrival_date),
           month=month(arrival_date),
           day=day(arrival_date),
           survey_type=mapping_table(survey_type,"cl_stat_effort_survey_types","CODE"),
           landing_site_name=mapping_table(landing_site_name,"cl_fish_landing_sites","NAME",exact = F),
           fishing_unit_name=mapping_table(fishing_unit_name,"cl_fish_fishing_units","NAME"),
           boat_activity_days=as.numeric(boat_activity_days)
    )%>%
    group_by(year,month,day,survey_type,landing_site_name,fishing_unit_name)%>%
    summarise(boat_activity_days=sum(boat_activity_days,na.rm=T))%>%
    ungroup()
  
for(i in 1:nrow(data_effort)){
  
  target_row<-data_effort[i,]
  
  new_dt_effort_survey$ADD_ENTRY(
    YEAR=target_row$year,  
    CL_APP_MONTH_ID=target_row$month,
    DAYS=target_row$day,                         
    CL_STAT_EFFORT_SURVEY_TYPE_ID=target_row$survey_type, 
    CL_FISH_LANDING_SITE_ID=target_row$landing_site_name,
    CL_FISH_FISHING_UNIT_ID=target_row$fishing_unit_name,       
    NB_DAYS_SAMPLED=target_row$boat_activity_days,               
    UPDATER_ID=updater,                   
    COMMENT=comment,                       
    CREATED_AT=now,                    
    UPDATED_AT=now                   
  )
}
  
for(trip in unique(data$identifier)){

print(trip)
data_target<-data%>%
  filter(identifier==trip)

print("data_trip")
data_trip<-data_target%>%
  select(identifier,recorder_name,vessel_registration_nr,boat_activity_days,fishing_unit_name,gear_name,recorder_name,departure_date,arrival_date,time_spent_fishing,unit_time_spent_fishing,departure_site_name,landing_site_name,fishing_zone_name,nr_fishers,food_cost,currency_food_cost,ice_bought_m3,ice_price,currency_ice_price,fuel_consumed_liter,fuel_price,currency_fuel_price,survey_type,notes)%>%
  distinct()%>%
  rowwise()%>%
  mutate(recorder_name=mapping_table(recorder_name,"custom_reg_entity_individuals","FULL_NAME","REG_ENTITY_ID",exact=F),
         unit_time_spent_fishing=mapping_table(unit_time_spent_fishing,"cl_app_quantity_units","NAME"),
         survey_type=mapping_table(survey_type,"cl_stat_effort_survey_types","CODE"),
         vessel_registration_nr=mapping_table(vessel_registration_nr,"reg_vessels","REGISTRATION_NUMBER"),
         fishing_unit_name=mapping_table(fishing_unit_name,"cl_fish_fishing_units","NAME"),
         gear_name=mapping_table(gear_name,"cl_ref_gears","NAME"),
         landing_site_name=mapping_table(landing_site_name,"cl_fish_landing_sites","NAME",exact = F),
         departure_site_name=mapping_table(departure_site_name,"cl_fish_landing_sites","NAME",exact = F),
         fishing_zone_name=mapping_table(fishing_zone_name,"cl_fish_fishing_zones","NAME",exact = F),
         currency_food_cost=mapping_table(currency_food_cost,"cl_ref_currencies","CODE"),
         currency_ice_price=mapping_table(currency_ice_price,"cl_ref_currencies","CODE"),
         currency_fuel_price=mapping_table(currency_fuel_price,"cl_ref_currencies","CODE")
         )%>%
  ungroup()

data_species<-data_target%>%
  select(species_code,landed_weight_kg,catch_price,fish_product_code,currency_catch_price)%>%
  rowwise()%>%
  mutate(species_code=mapping_table(species_code,"cl_ref_species","ASFIS_CODE"),
         fish_product_code=mapping_table(fish_product_code,"cl_ref_fishery_products","CODE"),
         quantity_live_weight=apply_product_factor(as.numeric(landed_weight_kg),fish_product_code),
         currency_catch_price=mapping_table(currency_catch_price,"cl_ref_currencies","CODE")
         )%>%
  ungroup()

new_dt_fishing_trip$ADD_ENTRY(
    REG_VESSEL_ID=data_trip$vessel_registration_nr ,                           
    CL_FISH_FISHING_UNIT_ID=data_trip$fishing_unit_name,                 
    REG_REPORTING_OFFICER_ID=data_trip$recorder_name,               
    CL_FISH_FISHING_TRIP_TYPE_ID=mapping_table("LANDFORM","cl_fish_fishing_trip_types","CODE"),            
    DATE_FROM=data_trip$departure_date,                                
    DATE_TO=data_trip$arrival_date,                                  
    CL_FROM_PORT_LOCATION_ID=data_trip$departure_site_name,                
    CL_TO_PORT_LOCATION_ID=data_trip$landing_site_name,                   
    CL_FROM_PORT_SITE_ID=data_trip$departure_site_name,                    
    CL_TO_PORT_SITE_ID=data_trip$landing_site_name,                      
    CL_FISH_FISHING_ZONE_ID=data_trip$fishing_zone_name,                  
    TIME_SPENT_FISHING_ZONE=data_trip$time_spent_fishing,                  
    CL_TIME_SPENT_FISHING_UNIT_ID=data_trip$unit_time_spent_fishing,           
    TIME_SPENT_FISHING_IN_FISHING_ZONE=data_trip$time_spent_fishing,       
    CL_TIME_SPENT_FISHING_ZONE_UNIT_ID=data_trip$unit_time_spent_fishing,       
    CREW_NUMBER=data_trip$nr_fishers,                            
    IDENTIFIER=data_trip$identifier,                               
    FOOD_COST=data_trip$food_cost,                                
    FOOD_COST_CURRENCY_ID=data_trip$currency_food_cost,                    
    ICE_AMOUNT=data_trip$ice_bought_m3,                               
    CL_APP_ICE_AMOUNT_UNIT_ID=mapping_table("Cube Meters","cl_app_quantity_units","NAME"),                
    ICE_COST=data_trip$ice_price,                                
    ICE_COST_CURRENCY_ID=data_trip$currency_ice_price,                       
    FUEL_AMOUNT=data_trip$fuel_consumed_liter,                             
    CL_APP_FUEL_AMOUNT_UNIT_ID=mapping_table("Liter","cl_app_quantity_units","NAME"),                
    FUEL_COST=data_trip$fuel_price,                                  
    FUEL_COST_CURRENCY_ID=data_trip$currency_fuel_price,                          
    COMMENT_TRIP=data_trip$notes,                             
    UPDATED_AT=now,                              
    UPDATER_ID=updater,                               
    COMMENT=comment
)

new_dt_fishing_activities$ADD_ENTRY(
DT_FISHING_TRIP_ID =new_dt_fishing_trip$MAX_INDEX(),
CL_FISH_FISHING_ACTIVITY_TYPE_ID=mapping_table("LANDING","cl_fish_fishing_activities_types","CODE"),
DATE_FROM=data_trip$departure_date,        
DATE_TO=data_trip$arrival_date,               
UPDATER_ID=updater,    
COMMENT=comment,            
CREATED_AT=now,                   
UPDATED_AT=now 
)

new_dt_fishing_activities_gear$ADD_ENTRY(
DT_FISHING_ACTIVITY_ID=new_dt_fishing_activities$MAX_INDEX(),
CL_REF_GEAR_ID   =data_trip$gear_name,
CL_APP_GEAR_ROLE_ID ="1" ,
UPDATER_ID=updater,
CREATED_AT=now,
UPDATED_AT=now
)

for (i in 1:nrow(data_species)){

  target_row<-data_species[i,]

new_dt_fishing_activities_species$ADD_ENTRY(
  DT_FISHING_ACTIVITY_ID =new_dt_fishing_activities$MAX_INDEX(),                           
  CL_REF_SPECIES_ID=target_row$species_code,                    
  DT_FISHING_ACTIVITY_GEAR_ID=new_dt_fishing_activities_gear$MAX_INDEX(),                     
  QUANTITY=target_row$landed_weight_kg,                  
  CL_APP_QUANTITY_UNIT_ID=mapping_table("Kilogram","cl_app_quantity_units"),                         
  PRICE_PER_UNIT_CATCH=target_row$catch_price,                            
  CL_REF_FISHERY_PRODUCT_ID=target_row$fish_product_code,                       
  CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT=target_row$quantity_live_weight,           
  TOTAL_VALUE_CURRENCY=target_row$currency_catch_price,                          
  UPDATER_ID=updater,
  COMMENT=comment,                                         
  CREATED_AT=now,
  UPDATED_AT=now
  )

}

}

execute_statement=FALSE
  
data_sql<-new_dt_fishing_trip$INSERT_STATEMENT(file=data_sql, execute=execute_statement)

data_sql<-new_dt_fishing_activities$INSERT_STATEMENT(file=data_sql, execute=execute_statement)

data_sql<-new_dt_fishing_activities_gear$INSERT_STATEMENT(file=data_sql, execute=execute_statement)

data_sql<-new_dt_fishing_activities_species$INSERT_STATEMENT(file=data_sql, execute=execute_statement)

data_sql<-new_dt_effort_survey$INSERT_STATEMENT(file=data_sql, execute=execute_statement)

message<-paste0(message, sprintf("- **\" %s\"** entries into table \"%s\"\n", nrow(new_dt_fishing_trip$VIEW_ENTRIES()),"dt_fishing_trip"))
message<-paste0(message, sprintf("- **\" %s\"** entries into table \"%s\"\n", nrow(new_dt_fishing_activities$VIEW_ENTRIES()),"dt_fishing_actvities"))
message<-paste0(message, sprintf("- **\" %s\"** entries into table \"%s\"\n", nrow(new_dt_fishing_activities_gear$VIEW_ENTRIES()),"dt_fishing_activities_gear"))
message<-paste0(message, sprintf("- **\" %s\"** entries into table \"%s\"\n", nrow(new_dt_fishing_activities_species$VIEW_ENTRIES()),"dt_fishing_activities_species"))
message<-paste0(message, sprintf("- **\" %s\"** entries into table \"%s\"\n", nrow(new_dt_effort_survey$VIEW_ENTRIES()),"dt_effort_survey"))


}

return(list(
  data = data,
  result = data_sql,
  referentials = referentials,
  errors = errors,
  valid = valid,
  message = message
))
}