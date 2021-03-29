#ICCAT TASK II script
#Author: Janette Daniel (FD)
compute_iccat_task_2 <- function(area_catch_year, area_effort_year, species_info){
  
  #harmonize column names
  colnames(area_catch_year) <- c("reference_number","time_period","square_type","square_lat","square_long","species_no","catch_lbs")
  colnames(area_effort_year) <- c("reference_number","time_period","no_sets","no_hooks","square_type","square_lat","square_long")
  
  #effort data
  #create new column in effort table for no sets * no hooks
  area_effort_year$total_hooks <- area_effort_year$no_sets * area_effort_year$no_hooks
  #keep and arrange columns of interest
  area_effort_year_1 <- area_effort_year[, c(2,3,8,5,6,7)]
  #aggregate data: no sets and no hooks by area
  area_effort_year_2 <-  aggregate(.~time_period+square_type+square_lat+square_long, area_effort_year_1, sum)
  #catch data
  #create column for catch kgs in catch table
  area_catch_year$catch_kgs <- area_catch_year[,"catch_lbs"]/2.20462
  #keep and arrange columns of interest
  area_catch_year_1 <- area_catch_year[, c(2,3,4,5,6,8)]
  #merge effort and catch data
  effort_catch_area_year <- merge(
    x = area_effort_year_2,
    y = area_catch_year_1,
    by.x = c("time_period", "square_type", "square_lat","square_long"),
    by.y = c("time_period", "square_type", "square_lat","square_long"),
    all.x = TRUE,
    all.y = TRUE
  )
  #merge species info with effort and catch data
  effort_catch_area_sp_year <- merge(
    x = effort_catch_area_year,
    y = species_info,
    by.x = "species_no",
    by.y = "species_no",
    all.x = TRUE,
    all.y = FALSE
  )
  #keep and arrange columns of interest
  ce_area_sp_year <- effort_catch_area_sp_year [,c(2,3,4,5,6,7,8,10)]
  #sum by species
  t2ce_year <- aggregate(.~time_period+square_type+square_lat+square_long+no_sets+total_hooks+scientific_name,
                         ce_area_sp_year, sum)
  #reshape data
  t2ce_year_wide <- reshape(
    data = t2ce_year, direction = "wide",
    idvar = c("time_period", "square_type", "square_lat", "square_long","no_sets", "total_hooks"),
    timevar = "scientific_name",
    v.names = "catch_kgs"
  )
  # remove the prefix of reshaped columns names  ;-)
  colnames(t2ce_year_wide) <- gsub("catch_kgs.", "", colnames(t2ce_year_wide)) 
  result <- t2ce_year_wide[order(t2ce_year_wide$time_period),]
  result
}