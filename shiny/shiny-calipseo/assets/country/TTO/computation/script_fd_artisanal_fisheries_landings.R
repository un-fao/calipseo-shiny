##
# Indicator for Artisanal fisheries 
# * Script name:
#     script_fd_artisanal_fisheries.R
# * Description:
#	  Computes artisanal fisheries landings indicator for Trinidad and Tobago fisheries
# * History:
# ------------- --------------- -----------------------
# Date			 Author			 Comment
# ------------- --------------- -----------------------
# 2018-11-11	eblondel    Creation.
# 2018-12-04	eblondel	Consolidation based on 2012/2013 datasets
# 2018-12-12	eblondel	Wrap logics into functions
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

#compute_1st_raised_landings 
#@param raw_data a standardized raw data set
#@param raised_1 a normalized dataset of 1st raising factors
#@returns the landings statistical descriptors in normalized data.frame
#----------------------------------------------------------------------------------------------
compute_1st_raised_landings <- function(raw_data, raised_1){

  raw_data <- as.data.frame(raw_data)
  raised_1 <- as.data.frame(raised_1)
  
	#raise data
	raw_data_01 <- raise_raw_data(raw_data, raised_1)

	#unique by gear
	raw_data_01_gear <- unique(raw_data_01[,c("landing_id","bch_id","bch_name","f_mthd_id","f_mthd","year.x","month","raised_trp")])
	
	#aggregate
	IGNORE_DIMENSION <- rep("all", nrow(raw_data_01))
	IGNORE_DIMENSION_ON_GEAR <- rep("all", nrow(raw_data_01_gear))
	
	#out aggregations
	#===================================================================================================================
	
	#1st aggregation (by gear/species/month)
	#-------------------------------------------------------------------------------------------------------------------
	
	#out aggregations
	#LAN (Landings in Kg)
	out_result_LAN <- do.call(data.frame, aggregate(
		x = raw_data_01$raised_lan_kgs,
		by = list(
			"bch_id" = raw_data_01$"bch_id",
			"bch_name" = raw_data_01$"bch_name",
			"gear_id" = raw_data_01$"f_mthd_id",
			"gear_name" = raw_data_01$"f_mthd",
			"species_id" = raw_data_01$"species_id",
			"species_name" = raw_data_01$species_des,
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
			"gear_id" = raw_data_01$"f_mthd_id",
			"gear_name" = raw_data_01$"f_mthd",
			"species_id" = raw_data_01$"species_id",
			"species_name" = raw_data_01$species_des,
			"year" = raw_data_01$"year.x",
			"month" = raw_data_01$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result_VAL)[ncol(out_result_VAL)] <- "var.VAL"
	#TRP (Number of trips)
	out_result_TRP <- do.call(data.frame, aggregate(
		x = raw_data_01$raised_trp,
		by = list(
			"bch_id" = raw_data_01$"bch_id",
			"bch_name" = raw_data_01$"bch_name",
			"gear_id" = raw_data_01$"f_mthd_id",
			"gear_name" = raw_data_01$"f_mthd",
			"species_id" = raw_data_01$"species_id",
			"species_name" = raw_data_01$species_des,
			"year" = raw_data_01$"year.x",
			"month" = raw_data_01$month
		),
		FUN = function(x){round2(sum(x))}
	))
	colnames(out_result_TRP)[ncol(out_result_TRP)] <- "var.TRP"

	#we bind these 3 first statistical descriptors
	out_result <- cbind(out_result_LAN, "var.VAL" = out_result_VAL$"var.VAL", "var.TRP" = out_result_TRP$"var.TRP")
	
	#we add subtotals by gear here (these are not subtotals but specific aggregations!)
	#LAN (Landings in Kg) by GEAR
	out_result_LAN_gear <- do.call(data.frame, aggregate(
		x = raw_data_01$raised_lan_kgs,
		by = list(
			"bch_id" = raw_data_01$"bch_id",
			"bch_name" = raw_data_01$"bch_name",
			"gear_id" = raw_data_01$"f_mthd_id",
			"gear_name" = raw_data_01$"f_mthd",
			"species_id" = IGNORE_DIMENSION,
			"species_name" = IGNORE_DIMENSION,
			"year" = raw_data_01$"year.x",
			"month" = raw_data_01$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result_LAN_gear)[ncol(out_result_LAN)] <- "var.LAN"
	#VAL (Value in TT $) by GEAR
	out_result_VAL_gear <- do.call(data.frame, aggregate(
		x = raw_data_01$raised_val,
		by = list(
			"bch_id" = raw_data_01$"bch_id",
			"bch_name" = raw_data_01$"bch_name",
			"gear_id" = raw_data_01$"f_mthd_id",
			"gear_name" = raw_data_01$"f_mthd",
			"species_id" = IGNORE_DIMENSION,
			"species_name" = IGNORE_DIMENSION,
			"year" = raw_data_01$"year.x",
			"month" = raw_data_01$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result_VAL_gear)[ncol(out_result_VAL_gear)] <- "var.VAL"

	#we need to calculate the number of trips by beach/gear/month for next statistical descriptors
	#this is also appended for gear "sub-totals"
	out_result_TRP_gear <- do.call(data.frame, aggregate(
		x = raw_data_01_gear$raised_trp,
		by = list(
			"bch_id" = raw_data_01_gear$"bch_id",
			"bch_name" = raw_data_01_gear$"bch_name",
			"gear_id" = raw_data_01_gear$"f_mthd_id",			
			"gear_name" = raw_data_01_gear$"f_mthd",
			"species_id" = IGNORE_DIMENSION_ON_GEAR,
			"species_name" = IGNORE_DIMENSION_ON_GEAR,
			"year" = raw_data_01_gear$"year.x",
			"month" = raw_data_01_gear$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result_TRP_gear)[ncol(out_result_TRP_gear)] <- "var.TRP"
	out_result_TRP_gear$"var.TRP" <- round2(out_result_TRP_gear$"var.TRP", 0)
		
	out_result_gear <- cbind(out_result_LAN_gear, "var.VAL" = out_result_VAL_gear$"var.VAL", "var.TRP" = out_result_TRP_gear$"var.TRP")
	out_result <- rbind(out_result, out_result_gear)
	
	#L/T
	out_result$"var.L/T" <- sapply(1:nrow(out_result), function(x){
		rec <- out_result[x,]
		trips_by_gear <- out_result_TRP_gear[
			out_result_TRP_gear$bch_name == rec$bch_name &
			out_result_TRP_gear$gear_name == rec$gear_name &
			out_result_TRP_gear$year == rec$year &
			out_result_TRP_gear$month == rec$month,"var.TRP"]
		out <- rec$"var.LAN" / trips_by_gear
		return(out)
	})

	#V/T
	out_result$"var.V/T" <- sapply(1:nrow(out_result), function(x){
		rec <- out_result[x,]
		trips_by_gear <- out_result_TRP_gear[
			out_result_TRP_gear$bch_name == rec$bch_name &
			out_result_TRP_gear$gear_name == rec$gear_name &
			out_result_TRP_gear$year == rec$year &
			out_result_TRP_gear$month == rec$month,"var.TRP"]
		out <- rec$"var.VAL" / trips_by_gear
		return(out)
	})

	#P/K
	out_result$"var.P/K" <- out_result$"var.VAL" / out_result$"var.LAN"
	
	
	#2d aggregation (by gear/species)
	#-------------------------------------------------------------------------------------------------------------------
	
	IGNORE2_DIMENSION <- rep("all", nrow(out_result))
	
	#out aggregations
	#LAN (Landings in Kg)
	out_result2_LAN <- do.call(data.frame, aggregate(
		x = out_result$"var.LAN",
		by = list(
			"bch_id" = out_result$"bch_id",
			"bch_name" = out_result$"bch_name",
			"gear_id" = out_result$"gear_id",
			"gear_name" = out_result$"gear_name",
			"species_id" = out_result$"species_id",
			"species_name" = out_result$"species_name",
			"year" = out_result$"year",
			"month" = IGNORE2_DIMENSION
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
			"gear_id" = out_result$"gear_id",
			"gear_name" = out_result$"gear_name",
			"species_id" = out_result$"species_id",
			"species_name" = out_result$"species_name",
			"year" = out_result$"year",
			"month" = IGNORE2_DIMENSION
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
			"gear_id" = out_result$"gear_id",
			"gear_name" = out_result$"gear_name",
			"species_id" = out_result$"species_id",
			"species_name" = out_result$"species_name",
			"year" = out_result$"year",
			"month" = IGNORE2_DIMENSION
		),
		FUN = function(x){round2(sum(x,na.rm=TRUE))}
	))
	colnames(out_result2_TRP)[ncol(out_result2_TRP)] <- "var.TRP"

	#we bind these 3 first statistical descriptors
	out_result2 <- cbind(out_result2_LAN, "var.VAL" = out_result2_VAL$"var.VAL", "var.TRP" = out_result2_TRP$"var.TRP")
	
	#we need to calculate the number of trips by beach/gear/month for next statistical descriptors
	#this is also appended for gear "sub-totals"
	out_result2_TRP_gear <- do.call(data.frame, aggregate(
		x = raw_data_01_gear$raised_trp,
		by = list(
			"bch_id" = raw_data_01_gear$"bch_id",
			"bch_name" = raw_data_01_gear$"bch_name",
			"gear_id" = raw_data_01_gear$"f_mthd_id",			
			"gear_name" = raw_data_01_gear$"f_mthd",
			"species_id" = IGNORE_DIMENSION_ON_GEAR,
			"species_name" = IGNORE_DIMENSION_ON_GEAR,
			"year" = raw_data_01_gear$"year.x",
			"month" = IGNORE_DIMENSION_ON_GEAR
		),
		function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result2_TRP_gear)[ncol(out_result2_TRP_gear)] <- "var.TRP"
	out_result2_TRP_gear$"var.TRP" <- round2(out_result2_TRP_gear$"var.TRP", 0)
		
	#L/T
	out_result2$"var.L/T" <- sapply(1:nrow(out_result2), function(x){
		rec <- out_result2[x,]
		trips_by_gear <- out_result2_TRP_gear[
			out_result2_TRP_gear$bch_name == rec$bch_name &
			out_result2_TRP_gear$gear_name == rec$gear_name &
			out_result2_TRP_gear$year == rec$year &
			out_result2_TRP_gear$month == rec$month,"var.TRP"]
		out <- rec$"var.LAN" / trips_by_gear
		return(out)
	})

	#V/T
	out_result2$"var.V/T" <- sapply(1:nrow(out_result2), function(x){
		rec <- out_result2[x,]
		trips_by_gear <- out_result2_TRP_gear[
			out_result2_TRP_gear$bch_name == rec$bch_name &
			out_result2_TRP_gear$gear_name == rec$gear_name &
			out_result2_TRP_gear$year == rec$year &
			out_result2_TRP_gear$month == rec$month,"var.TRP"]
		out <- rec$"var.VAL" / trips_by_gear
		return(out)
	})

	#P/K
	out_result2$"var.P/K" <- out_result2$"var.VAL" / out_result2$"var.LAN"
	
	#3rd aggregation (grand total by month)
	#-------------------------------------------------------------------------------------------------------------------
	
	IGNORE3_DIMENSION <- rep("all", nrow(out_result[out_result$species_id == "all",]))
	
	#out aggregations
	#LAN (Landings in Kg)
	out_result3_LAN <- do.call(data.frame, aggregate(
		x = out_result[out_result$species_id == "all",]$"var.LAN",
		by = list(
			"bch_id" = out_result[out_result$species_id == "all",]$"bch_id",
			"bch_name" = out_result[out_result$species_id == "all",]$"bch_name",
			"gear_id" = IGNORE3_DIMENSION,
			"gear_name" = IGNORE3_DIMENSION,
			"species_id" = IGNORE3_DIMENSION,
			"species_name" = IGNORE3_DIMENSION,
			"year" = out_result[out_result$species_id == "all",]$"year",
			"month" = out_result[out_result$species_id == "all",]$"month"
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result3_LAN)[ncol(out_result3_LAN)] <- "var.LAN"
	#VAL (Value in TT $)
	out_result3_VAL <- do.call(data.frame, aggregate(
		x = out_result[out_result$species_id == "all",]$"var.VAL",
		by = list(
			"bch_id" = out_result[out_result$species_id == "all",]$"bch_id",
			"bch_name" = out_result[out_result$species_id == "all",]$"bch_name",
			"gear_id" = IGNORE3_DIMENSION,
			"gear_name" = IGNORE3_DIMENSION,
			"species_id" = IGNORE3_DIMENSION,
			"species_name" = IGNORE3_DIMENSION,
			"year" = out_result[out_result$species_id == "all",]$"year",
			"month" = out_result[out_result$species_id == "all",]$"month"
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result3_VAL)[ncol(out_result3_VAL)] <- "var.VAL"
	#TRP (Number of trips)
	out_result3_TRP <- do.call(data.frame, aggregate(
		x = out_result[out_result$species_id == "all",]$"var.TRP",
		by = list(
			"bch_id" = out_result[out_result$species_id == "all",]$"bch_id",
			"bch_name" = out_result[out_result$species_id == "all",]$"bch_name",
			"gear_id" = IGNORE3_DIMENSION,
			"gear_name" = IGNORE3_DIMENSION,
			"species_id" = IGNORE3_DIMENSION,
			"species_name" = IGNORE3_DIMENSION,
			"year" = out_result[out_result$species_id == "all",]$"year",
			"month" = out_result[out_result$species_id == "all",]$"month"
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
	
	#4th aggregation (grand total by month)
	#-------------------------------------------------------------------------------------------------------------------
	
	#out aggregations
	#LAN (Landings in Kg)
	out_result4_LAN <- do.call(data.frame, aggregate(
		x = out_result[out_result$species_id == "all",]$"var.LAN",
		by = list(
			"bch_id" = out_result[out_result$species_id == "all",]$"bch_id",
			"bch_name" = out_result[out_result$species_id == "all",]$"bch_name",
			"gear_id" = IGNORE3_DIMENSION,
			"gear_name" = IGNORE3_DIMENSION,
			"species_id" = IGNORE3_DIMENSION,
			"species_name" = IGNORE3_DIMENSION,
			"year" = out_result[out_result$species_id == "all",]$"year",
			"month" = IGNORE3_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result4_LAN)[ncol(out_result4_LAN)] <- "var.LAN"
	#VAL (Value in TT $)
	out_result4_VAL <- do.call(data.frame, aggregate(
		x = out_result[out_result$species_id == "all",]$"var.VAL",
		by = list(
			"bch_id" = out_result[out_result$species_id == "all",]$"bch_id",
			"bch_name" = out_result[out_result$species_id == "all",]$"bch_name",
			"gear_id" = IGNORE3_DIMENSION,
			"gear_name" = IGNORE3_DIMENSION,
			"species_id" = IGNORE3_DIMENSION,
			"species_name" = IGNORE3_DIMENSION,
			"year" = out_result[out_result$species_id == "all",]$"year",
			"month" = IGNORE3_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result4_VAL)[ncol(out_result4_VAL)] <- "var.VAL"
	#TRP (Number of trips)
	out_result4_TRP <- do.call(data.frame, aggregate(
		x = out_result[out_result$species_id == "all",]$"var.TRP",
		by = list(
			"bch_id" = out_result[out_result$species_id == "all",]$"bch_id",
			"bch_name" = out_result[out_result$species_id == "all",]$"bch_name",
			"gear_id" = IGNORE3_DIMENSION,
			"gear_name" = IGNORE3_DIMENSION,
			"species_id" = IGNORE3_DIMENSION,
			"species_name" = IGNORE3_DIMENSION,
			"year" = out_result[out_result$species_id == "all",]$"year",
			"month" = IGNORE3_DIMENSION
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
	out_result <- reshape(out_result, v.names = "var", idvar = c("bch_id", "bch_name", "gear_id", "gear_name", "species_id", "species_name", "year", "month"), timevar = "descriptor", times = sapply(names(out_result)[(ncol(out_result)-5):ncol(out_result)], function(x){unlist(strsplit(x,"var."))[2]}), varying = list(names(out_result)[(ncol(out_result)-5):ncol(out_result)]), direction = "long")
	colnames(out_result)[ncol(out_result)] <- "value"
	
	#renaming all values
	out_result[out_result$gear_id == "all", "gear_id"] <- NA
	out_result[out_result$gear_name == "all", "gear_name"] <- NA
	out_result[out_result$species_id == "all", "species_id"] <- NA
	out_result[out_result$species_name == "all", "species_name"] <- NA
	out_result[out_result$month == "all", "month"] <- NA

	#rounding
	out_result$value <- round2(out_result$value, 2)

	return(out_result)
}
