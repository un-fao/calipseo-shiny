#'@name compute_catch_repartition
#'@title Repartion of the catch
#'@description
#'The catch are divided according to a parameter based on vessels ratio. It can be by region, by fleet segment, by landing site etc,
#' to allocate catch according to parameter that are not included in the stratification plan.
#'
#'Note: Additional check are performed to remove data with NAs, and ensure data consistency
#'
#'@param year results data
#'@param month results data 
#'@param minor_strata minor_strata. Default is \code{NULL}
#'@param artfish_results cvs produce by artfish_compute_report
#'@param active_vessels from sql query with region (cl_ref_admin_level_1)
#'@param active_vessels_strategy latest (takes the previous frame survey) or closest (takes the closest frame survey)
#'@return a \link[tibble]{tibble} object giving catch by the choosen parameter
#'@export



compute_catch_repartition<-function(con, year=NULL, month=NULL, minor_strata=NULL, artfish_results, active_vessels, active_vessels_strategy){
  
  # select_active with ref_period from artfish results
  ref_period <- as.Date(paste0(unique(artfish_results$year),"-",unique(sprintf("%02d", artfish_results$month)),"-01"))
  active_vessels <- select_active_vessels(periods = ref_period, active_vessels = active_vessels, active_vessels_strategy = active_vessels_strategy)
  
  # select columns in artfish_results
  estimates <- artfish_results %>% select(year, month, fishing_unit, species, catch_nominal_landed)
  
  # Total number of vessel per fishing unit
  FU_tot <- active_vessels %>%
    group_by(fishing_unit) %>%
    summarize(FU_tot = sum(fleet_engagement_number), .groups = "drop")
  
  # Number of vessel per fishing unit per region (get data from frame survey with accessArtfishAregion)
  FU_repart_var <- active_vessels %>%
    group_by(fishing_unit, repart_var) %>%
    summarize(FU_repart_var = sum(fleet_engagement_number), .groups = "drop")
  
  # For each fishing unit the total number of vessel per region is retrieved
  FU_repart_var$FU_tot <- FU_tot$FU_tot[match(FU_repart_var$fishing_unit,FU_tot$fishing_unit)]
  # Ration of vessel per FU per Region
  FU_repart_var$ratio<-FU_repart_var$FU_repart_var/FU_repart_var$FU_tot
  
  # Count total number of regions
  n_repart_var <- length(unique(active_vessels$repart_var))
  
  # Create duplicate lines for each region
  estimates_expanded <- estimates %>%
    crossing(repart_var = unique(active_vessels$repart_var))
  
  # Calculate the catch by region
  estimates_expanded$ratio <- FU_repart_var$ratio[match(paste0(estimates_expanded$fishing_unit,"-",estimates_expanded$repart_var),paste0(FU_repart_var$fishing_unit,"-",FU_repart_var$repart_var))]
  
  estimates_expanded$catch_repart_var<-estimates_expanded$ratio*estimates_expanded$catch_nominal_landed
  
  
  return(estimates_expanded)
}

# when is the csv with results is produce?
