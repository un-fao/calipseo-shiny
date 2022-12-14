##
# Indicator for Artisanal fisheries 
# * Script name:
#     script_fd_artisanal_fisheries.R
# * Description:
#	  Computes artisanal fisheries effort indicator for Trinidad and Tobago fisheries
# * History:
# ------------- --------------- -----------------------
# Date			 Author			 Comment
# ------------- --------------- -----------------------
# 2018-11-11	eblondel    Creation.
# 2018-12-04	eblondel	Consolidation based on 2012/2013 datasets
# 2018-12-12	eblondel	Wrap logics into functions
# 2019-02-13	eblondel	handle multiple aggregations
#

#environment
#--------------------
#set your wd

#options
#--------------------
options(stringsAsFactors = FALSE)

#package dependencies
#--------------------
require(readxl)
require(writexl)

#functions
#---------

round2 <- function(x, n=0) {scale<-10^n; trunc(x*scale+sign(x)*0.5)/scale}


#compute_1st_raised_effort 
#@param raw_data a standardized raw data set
#@param raised_1 a normalized dataset of 1st raising factors
#@returns the effort statistical descriptors in normalized data.frame
#----------------------------------------------------------------------------------------------
compute_1st_raised_trawl_effort <- function(raw_data, raised_1){

  raw_data <- as.data.frame(raw_data)
  raised_1 <- as.data.frame(raised_1)
  
	#filter on TRAWLING
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

	#raised hours
	raw_data_01$raised_hours <- as.numeric(difftime(raw_data_01$ret_datetime, raw_data_01$dep_datetime, units = "hours")) * as.numeric(raw_data_01$act) / as.numeric(raw_data_01$enum)
	#trip numbers
	raw_data_01$raised_trips <- as.numeric(raw_data_01$act) / as.numeric(raw_data_01$enum)
	if(any(raw_data_01$crew == "NULL")) stop("They are NULL crew values. Please check this dataset with TT Fisheries Division")
	raw_data_01$raised_mantrips <- raw_data_01$raised_trips * as.numeric(raw_data_01$crew)

	#unique trips
	raw_data_01_trips <- unique(raw_data_01[,c("landing_id","regnum","crew","bch_id","bch_name","vesstype","gr_f_area_id","gr_f_area","year.x","month","raised_hours","raised_trips", "raised_mantrips")])
	
	#aggregate
	IGNORE_DIMENSION <- rep("all", nrow(raw_data_01_trips))
	
	#out aggregations
	#===================================================================================================================
	
	#1st aggregation
	#-------------------------------------------------------------------------------------------------------------------
	out_result_HOURS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_hours,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = raw_data_01_trips$"vesstype",
			"gr_f_area_id" = raw_data_01_trips$"gr_f_area_id",
			"gr_f_area" = raw_data_01_trips$"gr_f_area",
			"year" = raw_data_01_trips$"year.x",
			"month" = raw_data_01_trips$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result_HOURS)[ncol(out_result_HOURS)] <- "var.HOURS"

	#BOATS (Boats refers to those operating on enumerated days and is NOT raised to account for non-enumerated days.)
	out_result_BOATS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$"regnum",
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = raw_data_01_trips$"vesstype",
			"gr_f_area_id" = raw_data_01_trips$"gr_f_area_id",
			"gr_f_area" = raw_data_01_trips$"gr_f_area",
			"year" = raw_data_01_trips$"year.x",
			"month" = raw_data_01_trips$month
		),
		FUN = function(x){length(unique(x))}
	))
	colnames(out_result_BOATS)[ncol(out_result_BOATS)] <- "var.BOATS"	

	#TRIPS
	out_result_TRIPS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_trips,
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
	colnames(out_result_TRIPS)[ncol(out_result_TRIPS)] <- "var.TRIPS"

	#MAN TRIPS
	out_result_MANTRIPS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_mantrips,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = raw_data_01_trips$"vesstype",
			"gr_f_area_id" = raw_data_01_trips$"gr_f_area_id",
			"gr_f_area" = raw_data_01_trips$"gr_f_area",
			"year" = raw_data_01_trips$"year.x",
			"month" = raw_data_01_trips$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result_MANTRIPS)[ncol(out_result_MANTRIPS)] <- "var.MAN TRIPS"

	#binding descriptors
	out_result <- cbind(out_result_HOURS, "var.MAN TRIPS" = out_result_MANTRIPS$"var.MAN TRIPS", "var.BOATS" = out_result_BOATS$"var.BOATS", "var.TRIPS" = out_result_TRIPS$"var.TRIPS")

	#HRS/TRP
	out_result$"var.HRS/TRP" <- out_result$"var.HOURS" / out_result$"var.TRIPS"
	out_result$"var.DYS/TRP" <- out_result$"var.HRS/TRP" / 24
	
	
	#2nd aggregation (ignore month) --> yearly total per strata
	#-------------------------------------------------------------------------------------------------------------------
	out_result2_HOURS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_hours,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = raw_data_01_trips$"vesstype",
			"gr_f_area_id" = raw_data_01_trips$"gr_f_area_id",
			"gr_f_area" = raw_data_01_trips$"gr_f_area",
			"year" = raw_data_01_trips$"year.x",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result2_HOURS)[ncol(out_result2_HOURS)] <- "var.HOURS"

	#BOATS (Boats refers to those operating on enumerated days and is NOT raised to account for non-enumerated days.)
	out_result2_BOATS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$"regnum",
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = raw_data_01_trips$"vesstype",
			"gr_f_area_id" = raw_data_01_trips$"gr_f_area_id",
			"gr_f_area" = raw_data_01_trips$"gr_f_area",
			"year" = raw_data_01_trips$"year.x",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){length(unique(x))}
	))
	colnames(out_result2_BOATS)[ncol(out_result2_BOATS)] <- "var.BOATS"	

	#TRIPS
	out_result2_TRIPS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_trips,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = raw_data_01_trips$"vesstype",
			"gr_f_area_id" = raw_data_01_trips$"gr_f_area_id",
			"gr_f_area" = raw_data_01_trips$"gr_f_area",
			"year" = raw_data_01_trips$"year.x",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	))
	colnames(out_result2_TRIPS)[ncol(out_result2_TRIPS)] <- "var.TRIPS"

	#MAN TRIPS
	out_result2_MANTRIPS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_mantrips,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = raw_data_01_trips$"vesstype",
			"gr_f_area_id" = raw_data_01_trips$"gr_f_area_id",
			"gr_f_area" = raw_data_01_trips$"gr_f_area",
			"year" = raw_data_01_trips$"year.x",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result2_MANTRIPS)[ncol(out_result2_MANTRIPS)] <- "var.MAN TRIPS"

	#binding descriptors
	out_result2 <- cbind(out_result2_HOURS, "var.MAN TRIPS" = out_result2_MANTRIPS$"var.MAN TRIPS", "var.BOATS" = out_result2_BOATS$"var.BOATS", "var.TRIPS" = out_result2_TRIPS$"var.TRIPS")

	#HRS/TRP
	out_result2$"var.HRS/TRP" <- out_result2$"var.HOURS" / out_result2$"var.TRIPS"
	out_result2$"var.DYS/TRP" <- out_result2$"var.HRS/TRP" / 24
	
	
	#3rd aggregation (ignore strata) --> total per month
	#-------------------------------------------------------------------------------------------------------------------
	out_result3_HOURS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_hours,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = raw_data_01_trips$"year.x",
			"month" = raw_data_01_trips$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result3_HOURS)[ncol(out_result3_HOURS)] <- "var.HOURS"

	#BOATS (Boats refers to those operating on enumerated days and is NOT raised to account for non-enumerated days.)
	out_result3_BOATS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$"regnum",
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = raw_data_01_trips$"year.x",
			"month" = raw_data_01_trips$month
		),
		FUN = function(x){length(unique(x))}
	))
	colnames(out_result3_BOATS)[ncol(out_result3_BOATS)] <- "var.BOATS"	

	#TRIPS
	out_result3_TRIPS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_trips,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = raw_data_01_trips$"year.x",
			"month" = raw_data_01_trips$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	))
	colnames(out_result3_TRIPS)[ncol(out_result3_TRIPS)] <- "var.TRIPS"

	#MAN TRIPS
	out_result3_MANTRIPS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_mantrips,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = raw_data_01_trips$"year.x",
			"month" = raw_data_01_trips$month
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result3_MANTRIPS)[ncol(out_result3_MANTRIPS)] <- "var.MAN TRIPS"

	#binding descriptors
	out_result3 <- cbind(out_result3_HOURS, "var.MAN TRIPS" = out_result3_MANTRIPS$"var.MAN TRIPS", "var.BOATS" = out_result3_BOATS$"var.BOATS", "var.TRIPS" = out_result3_TRIPS$"var.TRIPS")

	#HRS/TRP
	out_result3$"var.HRS/TRP" <- out_result3$"var.HOURS" / out_result3$"var.TRIPS"
	out_result3$"var.DYS/TRP" <- out_result3$"var.HRS/TRP" / 24
	
	#4th aggregation total
	#-------------------------------------------------------------------------------------------------------------------
	out_result4_HOURS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_hours,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = raw_data_01_trips$"year.x",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result4_HOURS)[ncol(out_result4_HOURS)] <- "var.HOURS"

	#BOATS (Boats refers to those operating on enumerated days and is NOT raised to account for non-enumerated days.)
	out_result4_BOATS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$"regnum",
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = raw_data_01_trips$"year.x",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){length(unique(x))}
	))
	colnames(out_result4_BOATS)[ncol(out_result4_BOATS)] <- "var.BOATS"	

	#TRIPS
	out_result4_TRIPS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_trips,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = raw_data_01_trips$"year.x",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	))
	colnames(out_result4_TRIPS)[ncol(out_result4_TRIPS)] <- "var.TRIPS"

	#MAN TRIPS
	out_result4_MANTRIPS <- do.call(data.frame, aggregate(
		x = raw_data_01_trips$raised_mantrips,
		by = list(
			"bch_id" = raw_data_01_trips$"bch_id",
			"bch_name" = raw_data_01_trips$"bch_name",
			"vesstype" = IGNORE_DIMENSION,
			"gr_f_area_id" = IGNORE_DIMENSION,
			"gr_f_area" = IGNORE_DIMENSION,
			"year" = raw_data_01_trips$"year.x",
			"month" = IGNORE_DIMENSION
		),
		FUN = function(x){sum(x, na.rm = TRUE)}
	))
	colnames(out_result4_MANTRIPS)[ncol(out_result4_MANTRIPS)] <- "var.MAN TRIPS"

	#binding descriptors
	out_result4 <- cbind(out_result4_HOURS, "var.MAN TRIPS" = out_result4_MANTRIPS$"var.MAN TRIPS", "var.BOATS" = out_result4_BOATS$"var.BOATS", "var.TRIPS" = out_result4_TRIPS$"var.TRIPS")

	#HRS/TRP
	out_result4$"var.HRS/TRP" <- out_result4$"var.HOURS" / out_result4$"var.TRIPS"
	out_result4$"var.DYS/TRP" <- out_result4$"var.HRS/TRP" / 24
	
	#==================================================================================================================================		
	#final structuring of output
	out_result <- rbind(out_result, out_result2, out_result3, out_result4)
	
	#order by beach #
	out_result <- out_result[order(out_result$"bch_id"),]

	#compute denormalize table
	out_result <- reshape(out_result, v.names = "var", idvar = c("bch_id", "bch_name", "vesstype", "gr_f_area_id", "gr_f_area", "year", "month"), timevar = "descriptor", times = sapply(names(out_result)[(ncol(out_result)-5):ncol(out_result)], function(x){unlist(strsplit(x,"var."))[2]}), varying = list(names(out_result)[(ncol(out_result)-5):ncol(out_result)]), direction = "long")
	colnames(out_result)[ncol(out_result)] <- "value"

	#renaming all values
	out_result[out_result$vesstype == "all", "vesstype"] <- NA
	out_result[out_result$gr_f_area_id == "all", "gr_f_area_id"] <- NA
	out_result[out_result$gr_f_area == "all", "gr_f_area"] <- NA
	out_result[out_result$month == "all", "month"] <- NA
	
	#rounding
	out_result$value <- round2(out_result$value, 2)
	
	return(out_result)
}


