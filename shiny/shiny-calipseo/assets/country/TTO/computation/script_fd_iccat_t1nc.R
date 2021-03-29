#ICCAT TASK I script - Modified from t1nc.R to comply with ICCAT Task I template
#Authors: Janette Daniel (FD), Emmanuel Blondel (FAO-FIAS)
compute_iccat_task_1 <- function(t1nc, species_info){
  
  colnames(t1nc) <- c("reference_number","species_no","catch_lbs","yft_sc","bet_sc","alb_sc","skj_sc",
                      "swo_sc","bum_sc","whm_sc","sai_sc","spf_sc","oth_sp_sc","yft_sa","bet_sa","bil_sa",
                      "alb_sa","skj_sa", "YearC")
  
  #landings by area by species
  landings_area_sp <- merge (
    x = t1nc,
    y = species_info,
    by.x = "species_no",
    by.y = "species_no",
    all.x = TRUE,
    all.y= FALSE
  )
  ##merge with fishing zone
  #landings_area_sp <- merge(
  #  x = landings_area_sp,
  #  y = fishingzone_info,
  #  by.x = "reference_number",
  #  by.y = "reference_number",
  #  all.x = TRUE, all.y = FALSE
  #)
  landings_area_sp$FishZoneCd <- "COMB"
  
  #create quantity column
  landings_area_sp$qtyLkg <- landings_area_sp [, "catch_lbs"]/2.20462
  
  #create new column selecting species specific stock codes
  landings_area_sp$SpcStockCd <- sapply(1:nrow(landings_area_sp), function(i){
    x <- landings_area_sp[i,]
    switch(x$scientific_name,
           "Thunnus albacares" = x$yft_sc,
           "Thunnus obesus" = x$bet_sc,
           "Xiphias gladius" = x$swo_sc,
           "Thunnus alalunga" = x$alb_sc,
           "Istiophorus albicans" = x$sai_sc,
           "Makaira nigricans" = x$bum_sc,
           "Tetrapturus albidus" = x$whm_sc,
           "Kutsuwonus pelamis" = x$skj_sc,
           x$oth_sp_sc
    )
  })
  #select species specific sampling area
  #assign each sampling area column with expression?
  #create new column selecting species specific stock codes
  landings_area_sp$SaAreaCd <- sapply(1:nrow(landings_area_sp), function(i){
    x <- landings_area_sp[i,]
    switch(x$scientific_name,
           "Thunnus albacares" = x$yft_sa,
           "Thunnus obesus" = x$bet_sa,
           "Xiphias_gladius" = x$bil_sa,
           "Thunnus alalunga" = x$alb_sa,
           "Kutsuwonus pelamis" = x$skj_sa,
           x$bil_sa
    )
  })
  
  landings_area_sp$FlagVesCd <- "TTO"
  landings_area_sp$PortZone <- "Trinidad"
  landings_area_sp$SpeciesCd <- substr(landings_area_sp$species_code, 1,3)
  landings_area_sp$AreaT1Cd <- ""
  landings_area_sp$GearCd <- "LL"
  
  #rearrange column names
  landings_area_sp <- landings_area_sp[,c("FlagVesCd", "PortZone", "YearC", "SpeciesCd", "SpcStockCd", "SaAreaCd","AreaT1Cd", "GearCd", "FishZoneCd", "qtyLkg")]
  
  #sum by species
  result <- aggregate (.~FlagVesCd+PortZone+YearC+SpeciesCd+SpcStockCd+SaAreaCd+AreaT1Cd+GearCd+FishZoneCd, landings_area_sp, sum)
  result$qtyLkg <- round(result$qtyLkg, 0)
  
  #adding columns
  #qty
  result$qtDDkg <- ""
  result$qtDLkg <- ""
  result$qtFAkg <- ""
  #conversion factors (ICCAT 2020 update)
  result$CFL <- ""
  result$CFD <- ""
  #datasources
  result$DSourceLCd <- "OTH"
  result$corrL <- sapply(result$SpeciesCd, function(x){
    corrl <- ifelse(x %in% c("YFT", "BET", "SAI", "BUM", "WHT", "SPF", "SWO"),"b","a")
    return(corrl)
  })
  result$DSourceDCd <- "OTH"
  result$corrD <- ""
  
  result
}