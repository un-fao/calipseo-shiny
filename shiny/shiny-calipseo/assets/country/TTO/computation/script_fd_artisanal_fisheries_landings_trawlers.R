##
# Indicator for Artisanal fisheries 
# * Script name:
#     script_fd_artisanal_fisheries_landings_trawlers.R
# * Description:
#	  Computes artisanal fisheries landings indicator for Trinidad and Tobago fisheries
# * History:
# ------------- --------------- -----------------------
# Date			 Author			 Comment
# ------------- --------------- -----------------------
# 2018-12-20	eblondel    Creation.
# 2019-02-14	eblondel	subtotals / totals aggregations added
#

#environment
#--------------------
#set your wd

#options
#--------------------
#options(stringsAsFactors = FALSE)

#package dependencies
#--------------------
#require(readxl)
#require(writexl)

#functions
#---------

#round2 <- function(x, n=0) {scale<-10^n; trunc(x*scale+sign(x)*0.5)/scale}

#compute_1st_raised_trawl_landings 
#@param raw_data a standardized raw data set
#@param raised_1 a normalized dataset of 1st raising factors
#@param by aggregation group (species or species_group)
#@returns the landings statistical descriptors in normalized data.frame
#----------------------------------------------------------------------------------------------
compute_1st_raised_trawl_landings <- function(raw_data, raised_1, by = "species"){

  raw_data <- as.data.frame(raw_data)
  raised_1 <- as.data.frame(raised_1)
  
	if(!(by %in% c("species","species_group"))){
		stop("'by' parameter should be either 'species' or 'species_group'")
	}
	bySpecies <- by == "species"
	bySpeciesGroup <- by == "species_group"

	#we filter only on TRAWLING fishing method
	raw_data <- raw_data[raw_data$f_mthd == "TRAWLING",]

	#convert these date/time from character to datetime objects (POSIXct)
	#This assumes that date/time are stored and read in UTC
	raw_data$dep_datetime <- as.POSIXct(raw_data$dep_datetime); attr(raw_data$dep_datetime,"tzone") <- appConfig$country_profile$timezone
	raw_data$ret_datetime <- as.POSIXct(raw_data$ret_datetime); attr(raw_data$ret_datetime,"tzone") <- appConfig$country_profile$timezone
	#year/month
	raw_data$year <- as.integer(format(raw_data$ret_datetime, "%Y"))
	raw_data$month <- as.integer(format(raw_data$ret_datetime,"%m"))

	#try to join/merge raw_data with raised_1 table
	raw_data_01 <- merge(
		x = raw_data, #table 1
		y = raised_1, #table 2
		by.x = c("bch_id", "month"),  #character name of column
		by.y = c("bch_id", "month") , #character name of column
		all.x = TRUE, #TRUE/FALSE
		all.y = FALSE  #TRUE/FALSE
	)

	#raised weight
	UNIT_LBS_TO_KGS <- 0.453592
	raw_data_01$raised_lan_lbs <- as.numeric(raw_data_01$quantity) * as.numeric(raw_data_01$act) / as.numeric(raw_data_01$enum)
	raw_data_01$raised_lan_kgs <- raw_data_01$raised_lan_lbs * UNIT_LBS_TO_KGS
	#raised value
	raw_data_01$raised_val <- raw_data_01$raised_lan_lbs * (raw_data_01$value / raw_data_01$quantity)
	#trip numbers
	raw_data_01$raised_trp <- as.numeric(raw_data_01$act) / as.numeric(raw_data_01$enum)

	#harmonize field name whatever level is considered for aggregation (species/species_group)
	if(bySpecies){
		raw_data_01$species_item_id <- raw_data_01$species_id
		raw_data_01$species_item_desc <- raw_data_01$species_desc
	}
	if(bySpeciesGroup){
		raw_data_01$species_item_id <- 0
		raw_data_01$species_item_desc <- ""
		raw_data_01[regexpr("SHRIMP",raw_data_01$species_desc, ignore.case = TRUE)>0,]$species_item_id <- 1
		raw_data_01[regexpr("SHRIMP",raw_data_01$species_desc, ignore.case = TRUE)>0,]$species_item_desc <- "Shrimp"
		raw_data_01[regexpr("SHRIMP",raw_data_01$species_desc, ignore.case = TRUE)<0,]$species_item_id <- 2
		raw_data_01[regexpr("SHRIMP",raw_data_01$species_desc, ignore.case = TRUE)<0,]$species_item_desc <- "Bycatch"
	}	
	
	
	#out aggregations
	#===================================================================================================================
	
	#1st aggregation (by vesstype/species_item/area/month)
	#-------------------------------------------------------------------------------------------------------------------
	
	#out aggregations
	#LAN (Landings in Kg)
	out_result_LAN <- do.call(data.frame, aggregate(
		x = raw_data_01$raised_lan_kgs,
		by = list(
			"bch_id" = raw_data_01$"bch_id",
			"bch_name" = raw_data_01$"bch_name",
			"vesstype" = raw_data_01$"vesstype",
			"species_item_id" = raw_data_01$"species_item_id",
			"species_item_desc" = raw_data_01$"species_item_desc",
			"gr_f_area_id" = raw_data_01$"gr_f_area_id",
			"gr_f_area" = raw_data_01$"gr_f_area",
			"year" = raw_data_01$"year.x",
			"month" = raw_data_01$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result_LAN)[ncol(out_result_LAN)] <- "var.LAN"
	#VAL (Value in TT $)
	out_result_VAL <- do.call(data.frame, aggregate(
		x = raw_data_01$raised_val,
		by = list(
			"bch_id" = raw_data_01$"bch_id",
			"bch_name" = raw_data_01$"bch_name",
			"vesstype" = raw_data_01$"vesstype",
			"species_item_id" = raw_data_01$"species_item_id",
			"species_item_desc" = raw_data_01$"species_item_desc",
			"gr_f_area_id" = raw_data_01$"gr_f_area_id",
			"gr_f_area" = raw_data_01$"gr_f_area",
			"year" = raw_data_01$"year.x",
			"month" = raw_data_01$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result_VAL)[ncol(out_result_VAL)] <- "var.VAL"
	#TRP (Number of trips)
	#we need to calculate the number of trips by beach/vesseltype/species_item_id/month
	raw_data_01_species_item <- unique(raw_data_01[,c("landing_id","bch_id","bch_name","vesstype","species_item_id","species_item_desc","gr_f_area_id","gr_f_area","year.x","month","raised_trp")])
	raw_data_01_species_item <- raw_data_01_species_item[order(raw_data_01_species_item$landing_id),]
	out_result_TRP <- do.call(data.frame, aggregate(
		x = raw_data_01_species_item$raised_trp,
		by = list(
			"bch_id" = raw_data_01_species_item$"bch_id",
			"bch_name" = raw_data_01_species_item$"bch_name",
			"vesstype" = raw_data_01_species_item$"vesstype",
			"species_item_id" = raw_data_01_species_item$"species_item_id",
			"species_item_desc" = raw_data_01_species_item$"species_item_desc",
			"gr_f_area_id" = raw_data_01_species_item$"gr_f_area_id",
			"gr_f_area" = raw_data_01_species_item$"gr_f_area",
			"year" = raw_data_01_species_item$"year.x",
			"month" = raw_data_01_species_item$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	))
	colnames(out_result_TRP)[ncol(out_result_TRP)] <- "var.TRP"

	#we bind these 3 first statistical descriptors
	out_result <- cbind(out_result_LAN, "var.VAL" = out_result_VAL$"var.VAL", "var.TRP" = out_result_TRP$"var.TRP")

	#we need to calculate the number of trips by beach/vesstype/gr_f_area/month for next statistical descriptors
	raw_data_01_trips <- unique(raw_data_01[,c("landing_id","bch_id","bch_name","vesstype","gr_f_area_id","gr_f_area","year.x","month","raised_trp")])
	out_result_TRP_trips <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_trp,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = raw_data_01_trips$"vesstype",
			"gr_f_area_id" = raw_data_01_trips$"gr_f_area_id",
			"gr_f_area" = raw_data_01_trips$"gr_f_area",
			"year" = raw_data_01_trips$"year.x",
			"month" = raw_data_01_trips$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	))
	colnames(out_result_TRP_trips)[ncol(out_result_TRP_trips)] <- "TRP_gear"
		
	#L/T
	out_result$"var.L/T" <- sapply(1:nrow(out_result), function(x){
		rec <- out_result[x,]
		trips <- out_result_TRP_trips[
			out_result_TRP_trips$bch_name == rec$bch_name &
			out_result_TRP_trips$vesstype == rec$vesstype &
			out_result_TRP_trips$gr_f_area == rec$gr_f_area &
			out_result_TRP_trips$year == rec$year &
			out_result_TRP_trips$month == rec$month,"TRP_gear"]
		out <- rec$"var.LAN" / trips
		return(out)
	})

	#V/T
	out_result$"var.V/T" <- sapply(1:nrow(out_result), function(x){
		rec <- out_result[x,]
		trips <- out_result_TRP_trips[
			out_result_TRP_trips$bch_name == rec$bch_name &
			out_result_TRP_trips$vesstype == rec$vesstype &
			out_result_TRP_trips$gr_f_area == rec$gr_f_area &
			out_result_TRP_trips$year == rec$year &
			out_result_TRP_trips$month == rec$month,"TRP_gear"]
		out <- rec$"var.VAL" / trips
		return(out)
	})

	#P/K
	out_result$"var.P/K" <- round2(out_result$"var.VAL" / out_result$"var.LAN",2)

	
	#2nd aggregation (by vesstype/species_item/area) total on year
	#-------------------------------------------------------------------------------------------------------------------
		
	IGNORE_DIMENSION <- rep("all", nrow(out_result))	
		
	#out aggregations
	#LAN (Landings in Kg)
	out_result2_LAN <- do.call(data.frame, aggregate(
		x = out_result$"var.LAN",
		by = list(
			"bch_id" = out_result$"bch_id",
			"bch_name" = out_result$"bch_name",
			"vesstype" = out_result$"vesstype",
			"species_item_id" = out_result$"species_item_id",
			"species_item_desc" = out_result$"species_item_desc",
			"gr_f_area_id" = out_result$"gr_f_area_id",
			"gr_f_area" = out_result$"gr_f_area",
			"year" = out_result$"year",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result2_LAN)[ncol(out_result2_LAN)] <- "var.LAN"
	#VAL (Value in TT $)
	out_result2_VAL <- do.call(data.frame, aggregate(
		x = out_result$"var.VAL",
		by = list(
			"bch_id" = out_result$"bch_id",
			"bch_name" = out_result$"bch_name",
			"vesstype" = out_result$"vesstype",
			"species_item_id" = out_result$"species_item_id",
			"species_item_desc" = out_result$"species_item_desc",
			"gr_f_area_id" = out_result$"gr_f_area_id",
			"gr_f_area" = out_result$"gr_f_area",
			"year" = out_result$"year",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result2_VAL)[ncol(out_result2_VAL)] <- "var.VAL"
	#TRP (Number of trips)
	out_result2_TRP <- do.call(data.frame, aggregate(
		x = out_result$"var.TRP",
		by = list(
			"bch_id" = out_result$"bch_id",
			"bch_name" = out_result$"bch_name",
			"vesstype" = out_result$"vesstype",
			"species_item_id" = out_result$"species_item_id",
			"species_item_desc" = out_result$"species_item_desc",
			"gr_f_area_id" = out_result$"gr_f_area_id",
			"gr_f_area" = out_result$"gr_f_area",
			"year" = out_result$"year",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result2_TRP)[ncol(out_result2_TRP)] <- "var.TRP"

	#we bind these 3 first statistical descriptors
	out_result2 <- cbind(out_result2_LAN, "var.VAL" = out_result2_VAL$"var.VAL", "var.TRP" = out_result2_TRP$"var.TRP")
	
	out_result_TRP_trips_tot <- do.call(data.frame, aggregate(
		x = out_result_TRP_trips$TRP_gear,
		by = list(
			"bch_id" = out_result_TRP_trips$"bch_id",
			"bch_name" = out_result_TRP_trips$"bch_name",
			"vesstype" = out_result_TRP_trips$"vesstype",
			"gr_f_area_id" = out_result_TRP_trips$"gr_f_area_id",
			"gr_f_area" = out_result_TRP_trips$"gr_f_area",
			"year" = out_result_TRP_trips$"year",
			"month" = rep("", nrow(out_result_TRP_trips))
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	))
	colnames(out_result_TRP_trips_tot)[ncol(out_result_TRP_trips_tot)] <- "TRP_gear"
	
		
	#L/T
	out_result2$"var.L/T" <- sapply(1:nrow(out_result2), function(x){
		rec <- out_result2[x,]
		trips <- out_result_TRP_trips_tot[
			out_result_TRP_trips_tot$bch_name == rec$bch_name &
			out_result_TRP_trips_tot$vesstype == rec$vesstype &
			out_result_TRP_trips_tot$gr_f_area == rec$gr_f_area &
			out_result_TRP_trips_tot$year == rec$year,"TRP_gear"]
		out <- rec$"var.LAN" / trips
		return(out)
	})
	#V/T
	out_result2$"var.V/T" <- sapply(1:nrow(out_result2), function(x){
		rec <- out_result2[x,]
		trips <- out_result_TRP_trips_tot[
			out_result_TRP_trips_tot$bch_name == rec$bch_name &
			out_result_TRP_trips_tot$vesstype == rec$vesstype &
			out_result_TRP_trips_tot$gr_f_area == rec$gr_f_area &
			out_result_TRP_trips_tot$year == rec$year,"TRP_gear"]
		out <- rec$"var.VAL" / trips
		return(out)
	})
	#P/K
	out_result2$"var.P/K" <- out_result2$"var.VAL" / out_result2$"var.LAN"
	
	
	#3rd aggregation (grand total by month)
	#----------------------------------------------------------------------------------------------------------------------------------
	#out aggregations
	#LAN (Landings in Kg)
	out_result3_LAN <- do.call(data.frame, aggregate(
		x = out_result$"var.LAN",
		by = list(
			"bch_id" = out_result$"bch_id",
			"bch_name" = out_result$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"species_item_id" = IGNORE_DIMENSION,
			"species_item_desc" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = out_result$"year",
			"month" = out_result$"month"
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result3_LAN)[ncol(out_result3_LAN)] <- "var.LAN"
	#VAL (Value in TT $)
	out_result3_VAL <- do.call(data.frame, aggregate(
		x = out_result$"var.VAL",
		by = list(
			"bch_id" = out_result$"bch_id",
			"bch_name" = out_result$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"species_item_id" = IGNORE_DIMENSION,
			"species_item_desc" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = out_result$"year",
			"month" = out_result$"month"
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result3_VAL)[ncol(out_result3_VAL)] <- "var.VAL"
	#TRP (Number of trips)
	raw_data_01_tot <- unique(raw_data_01[,c("landing_id","bch_id","bch_name","year.x","month","raised_trp")])
	IGNORE_DIMENSION_ON_TOT <- rep("", nrow(raw_data_01_tot))
	out_result3_TRP <- do.call(data.frame, aggregate(
		x = raw_data_01_tot$raised_trp,
		by = list(
			"bch_id" = raw_data_01_tot$"bch_id",
			"bch_name" = raw_data_01_tot$"bch_name",
			"vesstype" = IGNORE_DIMENSION_ON_TOT,
			"species_item_id" = IGNORE_DIMENSION_ON_TOT,
			"species_item_desc" = IGNORE_DIMENSION_ON_TOT,
			"gr_f_area_id" = IGNORE_DIMENSION_ON_TOT,
			"gr_f_area" = IGNORE_DIMENSION_ON_TOT,
			"year" = raw_data_01_tot$"year.x",
			"month" = raw_data_01_tot$"month"
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	))
	colnames(out_result3_TRP)[ncol(out_result3_TRP)] <- "var.TRP"

	#we bind these 3 first statistical descriptors
	out_result3 <- cbind(out_result3_LAN, "var.VAL" = out_result3_VAL$"var.VAL", "var.TRP" = out_result3_TRP$"var.TRP")
		
	#L/T
	out_result3$"var.L/T" <- out_result3$"var.LAN" / out_result3$"var.TRP"
	#V/T
	out_result3$"var.V/T" <- out_result3$"var.VAL" / out_result3$"var.TRP"
	#P/K
	out_result3$"var.P/K" <- out_result3$"var.VAL" / out_result3$"var.LAN"
	
	#4th aggregation (total)
	#----------------------------------------------------------------------------------------------------------------------------------
	#out aggregations
	#LAN (Landings in Kg)
	out_result4_LAN <- do.call(data.frame, aggregate(
		x = out_result$"var.LAN",
		by = list(
			"bch_id" = out_result$"bch_id",
			"bch_name" = out_result$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"species_item_id" = IGNORE_DIMENSION,
			"species_item_desc" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = out_result$"year",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result4_LAN)[ncol(out_result4_LAN)] <- "var.LAN"
	#VAL (Value in TT $)
	out_result4_VAL <- do.call(data.frame, aggregate(
		x = out_result$"var.VAL",
		by = list(
			"bch_id" = out_result$"bch_id",
			"bch_name" = out_result$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"species_item_id" = IGNORE_DIMENSION,
			"species_item_desc" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = out_result$"year",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result4_VAL)[ncol(out_result4_VAL)] <- "var.VAL"
	#TRP (Number of trips)
	IGNORE_DIMENSION <- rep("", nrow(out_result3_TRP))
	out_result4_TRP <- do.call(data.frame, aggregate(
		x = out_result3_TRP$"var.TRP",
		by = list(
			"bch_id" = out_result3_TRP$"bch_id",
			"bch_name" = out_result3_TRP$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"species_item_id" = IGNORE_DIMENSION,
			"species_item_desc" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = out_result3_TRP$"year",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	))
	colnames(out_result4_TRP)[ncol(out_result4_TRP)] <- "var.TRP"

	#we bind these 3 first statistical descriptors
	out_result4 <- cbind(out_result4_LAN, "var.VAL" = out_result4_VAL$"var.VAL", "var.TRP" = out_result4_TRP$"var.TRP")
		
	#L/T
	out_result4$"var.L/T" <- out_result4$"var.LAN" / out_result4$"var.TRP"
	#V/T
	out_result4$"var.V/T" <- out_result4$"var.VAL" / out_result4$"var.TRP"
	#P/K
	out_result4$"var.P/K" <- out_result4$"var.VAL" / out_result4$"var.LAN"
	
	
	#==================================================================================================================================		
	#final structuring of output
	out_result <- rbind(out_result, out_result2, out_result3, out_result4)
	
	#order by beach #
	out_result <- out_result[order(out_result$"bch_id"),]

	#compute denormalize table
	out_result <- reshape(out_result, v.names = "var", idvar = c("bch_id", "bch_name", "vesstype", "species_item_id", "species_item_desc", "gr_f_area_id","gr_f_area", "year", "month"), timevar = "descriptor", times = sapply(names(out_result)[(ncol(out_result)-5):ncol(out_result)], function(x){unlist(strsplit(x,"var."))[2]}), varying = list(names(out_result)[(ncol(out_result)-5):ncol(out_result)]), direction = "long")
	colnames(out_result)[ncol(out_result)] <- "value"

	#renaming all values
	out_result[out_result$vesstype == "all", "vesstype"] <- NA
	out_result[out_result$species_item_id == "all", "species_item_id"] <- NA
	out_result[out_result$species_item_desc == "all", "species_item_desc"] <- NA
	out_result[out_result$gr_f_area_id == "all", "gr_f_area_id"] <- NA
	out_result[out_result$gr_f_area == "all", "gr_f_area"] <- NA
	out_result[out_result$month == "all", "month"] <- NA
	
	#rounding
	out_result$value <- round2(out_result$value, 2)

	return(out_result)
}
