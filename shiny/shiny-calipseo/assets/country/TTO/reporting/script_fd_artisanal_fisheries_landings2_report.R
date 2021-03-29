 ##
# Indicator for Artisanal fisheries 
# * Script name:
#     script_fd_artisanal_fisheries_landings2_report.R
# * Description:
#	  Prepares an Excel report artisanal fisheries 2d raised landings indicator,, by species/subtotal for Trinidad and Tobago fisheries
# * History:
# ------------- --------------- -----------------------
# Date			 Author			 Comment
# ------------- --------------- -----------------------
# 2019-02-26	eblondel    Creation.
#

#environment
#--------------------
#set your wd

#options
#--------------------
options(stringsAsFactors = FALSE)

#package dependencies
#--------------------

#functions
#---------

#report_2nd_raised_landings_by_SPECIES
#@param landings_2 a normalized dataset with 2nd landings statistical descriptors
#@returns a data.frame object ready for writing to Excel files
report_2nd_raised_landings_by_SPECIES <- function(landings_2){

	year <- unique(landings_2$year)[1]

	report <- landings_2[is.na(landings_2$group_id_iccat) & is.na(landings_2$group_id_fao),]

	#replace NA by NA strings
	report[is.na(report$month),]$month <- "NA"
	class(report$group_id_subt) <- "numeric"
	report[is.na(report$group_id_subt),]$group_id_subt <- 9999	
	report[is.na(report$group_name_subt),]$group_name_subt <- "NA"
	class(report$species_id) <- "numeric"
	report[is.na(report$species_id),]$species_id <- 9999
	report[is.na(report$species_desc),]$species_desc <- "NA"
	#remove useless columns for this particular report
	report$group_id <- NULL
	report$group_name <- NULL
	report$group_id_iccat <- NULL
	report$group_name_iccat <- NULL
	report$group_id_fao <- NULL
	report$group_name_fao <- NULL

	#process month
	report <- report[order(report$month),]
	report$month <- sapply(report$month, function(x){
		return(switch(x, "1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY","6"="JUN","7"="JUL","8"="AUG","9"="SEP","10"="OCT","11"="NOV","12"="DEC","NA"="TOTAL"))
	})
	
	#reshape to wide
	report_resh <- reshape(report, v.names = "value", idvar = c("species_id", "species_desc", "group_id_subt", "group_name_subt", "descriptor"), timevar = "month", direction = "wide")
	start_values_idx <- which(regexpr("value.",colnames(report_resh))>0)[1]
	colnames(report_resh)[start_values_idx:ncol(report_resh)] <- sapply(colnames(report_resh)[start_values_idx:ncol(report_resh)], function(x){unlist(strsplit(x,"value."))[2]})
	colnames(report_resh)[start_values_idx-1] <- "descriptor"
	
	#reorder descriptors
	report_resh$descriptor <- factor(report_resh$descriptor, levels = c("LAN", "VAL", "TRP", "L/T", "V/T", "P/K"))
	#sorting (requires to be done here!)
	report_resh <- report_resh[with(report_resh, order(group_id_subt, species_id, descriptor)),]
	report_resh$descriptor <- as.character(report_resh$descriptor)
	#merge columns
	report_resh$group_name_subt <- paste("SUBTOTAL: GROUP",paste(report_resh$"group_id_subt",report_resh$"group_name_subt",sep="-"), sep=" ")
	report_resh$species_desc <-  paste(report_resh$"species_id", report_resh$"species_desc", sep="-")
	report_resh[report_resh$species_desc=="9999-NA", "species_desc"] <- report_resh[report_resh$species_desc=="9999-NA", "group_name_subt"]
	report_resh[report_resh$species_id==9999 & report_resh$group_id_subt==9999, "species_desc"] <- "GRAND TOTAL"

	#check all month columns are there, and set 0 where NA
	month_names <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","TOTAL")
	for(month_name in month_names){
		if(!(month_name %in% colnames(report_resh))) report_resh[,month_name] <- NA
		report_resh[is.na(report_resh[,month_name]),month_name] <- 0
	}
	
	report_resh$year <- NULL
	report_resh <- report_resh[,c("species_id","species_desc","group_id_subt","group_name_subt","descriptor",month_names)]

	#add mean,var,sd
	report_resh_mean <- round2(rowMeans(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)
	report_resh_var <- round2(rowVar(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)		
	report_resh$"MEAN" <- report_resh_mean	
	report_resh$"VAR" <- report_resh_var		
	report_resh$"STD DEV" <- round2(sqrt(report_resh$VAR),2)
	
	#update MEAN
	items <- unique(report_resh$species_desc)
	for(variable in c("MEAN")){
		invisible(lapply(items, function(item){
			group_id_subt <- unique(report_resh[report_resh$species_desc == item,"group_id_subt"])
			#update L/T
			report_resh[report_resh$species_desc == item & report_resh$descriptor == "L/T",variable] <<- round2(report_resh[report_resh$species_desc == item & report_resh$descriptor == "LAN", variable] / report_resh[report_resh$species_id==9999 & report_resh$species_des == "GRAND TOTAL" & report_resh$descriptor == "TRP", variable],2)
			#update V/T
			report_resh[report_resh$species_desc == item & report_resh$descriptor == "V/T",variable] <<- round2(report_resh[report_resh$species_desc == item & report_resh$descriptor == "VAL", variable] / report_resh[report_resh$species_id==9999 & report_resh$species_des == "GRAND TOTAL" & report_resh$descriptor == "TRP", variable],2)
			#update P/K
			report_resh[report_resh$species_desc == item & report_resh$descriptor == "P/K",variable] <<- round2(report_resh[report_resh$species_desc == item & report_resh$descriptor == "VAL", variable] / report_resh[report_resh$species_desc == item & report_resh$descriptor == "LAN", variable],2)
		}))
	}
	
	
	#remove columns
	report_resh$"species_id" <- NULL
	report_resh$"group_id_subt" <- NULL
	report_resh$"group_name_subt" <- NULL
	#remove extra gear/species text by on subsequent descriptors other than LAN
	report_resh[report_resh$descriptor != "LAN", "species_desc"] <- ""
	#rename columns
	colnames(report_resh)[colnames(report_resh)=="species_desc"] <- "SPECIES"
	colnames(report_resh)[colnames(report_resh)=="descriptor"] <- "DESCRIPTOR"
	
	#add headers
	headers <- c(
		sprintf("SPECIES/MONTH SUMMARY OF TOTAL ESTIMATED LANDINGS FOR ARTISINAL FISHERIES OF TRINIDAD (INCLUDING TRAWLING) FOR %s", year),
		"Report ID: rf2_rep2"
	)
	attr(report_resh, "headers") <- headers
	#add footers
	footers <- c(
		"Trips for a species refers to the trips that caught the species.",
		"Landings/trip and Value/trip for a species is calculated using the total trips done by all gears and NOT the number of trips that caught the species.",
		"Trips are not standardized across gears."
	)
	attr(report_resh, "footers") <- footers
	
	return(report_resh)
	
}

#report_2nd_raised_landings_by_FAOGROUP
#@param landings_2 a normalized dataset with 2nd landings statistical descriptors
#@returns a data.frame object ready for writing to Excel files
report_2nd_raised_landings_by_FAOGROUP <- function(landings_2){

	year <- unique(landings_2$year)[1]

	report <- landings_2[is.na(landings_2$group_id_subt) & is.na(landings_2$group_id_iccat),]

	#replace NA by NA strings
	report[is.na(report$month),]$month <- "NA"
	class(report$group_id_fao) <- "numeric"
	report[is.na(report$group_id_fao),]$group_id_fao <- 9999	
	report[is.na(report$group_name_fao),]$group_name_fao <- "NA"
	class(report$species_id) <- "numeric"
	report[is.na(report$species_id),]$species_id <- 9999
	report[is.na(report$species_desc),]$species_desc <- "NA"
	#remove useless columns for this particular report
	report$group_id <- NULL
	report$group_name <- NULL
	report$group_id_subt <- NULL
	report$group_name_subt <- NULL
	report$group_id_iccat <- NULL
	report$group_name_iccat <- NULL

	#process month
	report <- report[order(report$month),]
	report$month <- sapply(report$month, function(x){
		return(switch(x, "1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY","6"="JUN","7"="JUL","8"="AUG","9"="SEP","10"="OCT","11"="NOV","12"="DEC","NA"="TOTAL"))
	})
	
	#reshape to wide
	report_resh <- reshape(report, v.names = "value", idvar = c("species_id", "species_desc", "group_id_fao", "group_name_fao", "descriptor"), timevar = "month", direction = "wide")
	start_values_idx <- which(regexpr("value.",colnames(report_resh))>0)[1]
	colnames(report_resh)[start_values_idx:ncol(report_resh)] <- sapply(colnames(report_resh)[start_values_idx:ncol(report_resh)], function(x){unlist(strsplit(x,"value."))[2]})
	colnames(report_resh)[start_values_idx-1] <- "descriptor"
	
	#reorder descriptors
	report_resh$descriptor <- factor(report_resh$descriptor, levels = c("LAN", "VAL", "TRP", "L/T", "V/T", "P/K"))
	#sorting (requires to be done here!)
	report_resh <- report_resh[with(report_resh, order(group_id_fao, species_id, descriptor)),]
	report_resh$descriptor <- as.character(report_resh$descriptor)
	#merge columns
	report_resh$group_name_fao <- paste(report_resh$"group_id_fao",report_resh$"group_name_fao",sep="-")
	report_resh$species_desc <-  paste(report_resh$"species_id", report_resh$"species_desc", sep="-")
	report_resh[report_resh$species_desc=="9999-NA", "species_desc"] <- report_resh[report_resh$species_desc=="9999-NA", "group_name_fao"]
	report_resh[report_resh$species_id==9999 & report_resh$group_id_fao==9999, "species_desc"] <- "GRAND TOTAL"

	#check all month columns are there, and set 0 where NA
	month_names <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","TOTAL")
	for(month_name in month_names){
		if(!(month_name %in% colnames(report_resh))) report_resh[,month_name] <- NA
		report_resh[is.na(report_resh[,month_name]),month_name] <- 0
	}
	
	report_resh$year <- NULL
	report_resh <- report_resh[,c("species_id","species_desc","group_id_fao","group_name_fao","descriptor",month_names)]

	#add mean,var,sd
	report_resh_mean <- round2(rowMeans(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)
	report_resh_var <- round2(rowVar(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)		
	report_resh$"MEAN" <- report_resh_mean	
	report_resh$"VAR" <- report_resh_var		
	report_resh$"STD DEV" <- round2(sqrt(report_resh$VAR),2)
	
	#update MEAN
	items <- unique(report_resh$species_desc)
	for(variable in c("MEAN")){
		invisible(lapply(items, function(item){
			group_id_fao <- unique(report_resh[report_resh$species_desc == item,"group_id_fao"])
			#update L/T
			report_resh[report_resh$species_desc == item & report_resh$descriptor == "L/T",variable] <<- round2(report_resh[report_resh$species_desc == item & report_resh$descriptor == "LAN", variable] / report_resh[report_resh$species_id==9999 & report_resh$species_des == "GRAND TOTAL" & report_resh$descriptor == "TRP", variable],2)
			#update V/T
			report_resh[report_resh$species_desc == item & report_resh$descriptor == "V/T",variable] <<- round2(report_resh[report_resh$species_desc == item & report_resh$descriptor == "VAL", variable] / report_resh[report_resh$species_id==9999 & report_resh$species_des == "GRAND TOTAL" & report_resh$descriptor == "TRP", variable],2)
			#update P/K
			report_resh[report_resh$species_desc == item & report_resh$descriptor == "P/K",variable] <<- round2(report_resh[report_resh$species_desc == item & report_resh$descriptor == "VAL", variable] / report_resh[report_resh$species_desc == item & report_resh$descriptor == "LAN", variable],2)
		}))
	}
	
	
	#remove columns
	report_resh$"species_id" <- NULL
	report_resh$"group_id_fao" <- NULL
	report_resh$"group_name_fao" <- NULL
	#remove extra gear/species text by on subsequent descriptors other than LAN
	report_resh[report_resh$descriptor != "LAN", "species_desc"] <- ""
	#rename columns
	colnames(report_resh)[colnames(report_resh)=="species_desc"] <- "SPECIES"
	colnames(report_resh)[colnames(report_resh)=="descriptor"] <- "DESCRIPTOR"
	
	#add headers
	headers <- c(
		sprintf("SPECIES/MONTH SUMMARY OF TOTAL ESTIMATED LANDINGS FOR ARTISINAL FISHERIES OF TRINIDAD (INCLUDING TRAWLING) FOR %s", year),
		"Report ID: rf2_rep2"
	)
	attr(report_resh, "headers") <- headers
	#add footers
	footers <- c(
		"Trips for a species refers to the trips that caught the species.",
		"Landings/trip and Value/trip for a species is calculated using the total trips done by all gears and NOT the number of trips that caught the species.",
		"Trips are not standardized across gears."
	)
	attr(report_resh, "footers") <- footers
	
	return(report_resh)
	
}

#report_2nd_raised_landings_by_ICCATGROUP
#@param landings_2 a normalized dataset with 2nd landings statistical descriptors
#@returns a data.frame object ready for writing to Excel files
report_2nd_raised_landings_by_ICCATGROUP <- function(landings_2){

	year <- unique(landings_2$year)[1]

	report <- landings_2[is.na(landings_2$group_id_subt) & is.na(landings_2$group_id_fao),]
	
	#replace NA by NA strings
	report[is.na(report$month),]$month <- "NA"
	class(report$group_id_iccat) <- "numeric"
	report[is.na(report$group_id_iccat),]$group_id_iccat <- 9999	
	report[is.na(report$group_name_iccat),]$group_name_iccat <- "NA"
	class(report$species_id) <- "numeric"
	report[is.na(report$species_id),]$species_id <- 9999
	report[is.na(report$species_desc),]$species_desc <- "NA"
	
	#remove useless columns for this particular report
	report$group_id <- NULL
	report$group_name <- NULL
	report$group_id_subt <- NULL
	report$group_name_subt <- NULL
	report$group_id_fao <- NULL
	report$group_name_fao <- NULL

	
	
	#process month
	report <- report[order(report$month),]
	report$month <- sapply(report$month, function(x){
		return(switch(x, "1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY","6"="JUN","7"="JUL","8"="AUG","9"="SEP","10"="OCT","11"="NOV","12"="DEC","NA"="TOTAL"))
	})
	
	#reshape to wide
	report_resh <- reshape(report, v.names = "value", idvar = c("species_id", "species_desc", "group_id_iccat", "group_name_iccat", "descriptor"), timevar = "month", direction = "wide")
	start_values_idx <- which(regexpr("value.",colnames(report_resh))>0)[1]
	colnames(report_resh)[start_values_idx:ncol(report_resh)] <- sapply(colnames(report_resh)[start_values_idx:ncol(report_resh)], function(x){unlist(strsplit(x,"value."))[2]})
	colnames(report_resh)[start_values_idx-1] <- "descriptor"
	
	#reorder descriptors
	report_resh$descriptor <- factor(report_resh$descriptor, levels = c("LAN", "VAL", "TRP", "L/T", "V/T", "P/K"))
	#sorting (requires to be done here!)
	report_resh <- report_resh[with(report_resh, order(group_id_iccat, species_id, descriptor)),]
	report_resh$descriptor <- as.character(report_resh$descriptor)
	#merge columns
	report_resh$group_name_iccat <- paste(report_resh$"group_id_iccat",report_resh$"group_name_iccat",sep="-")
	report_resh$species_desc <-  paste(report_resh$"species_id", report_resh$"species_desc", sep="-")
	report_resh[report_resh$species_desc=="9999-NA", "species_desc"] <- report_resh[report_resh$species_desc=="9999-NA", "group_name_iccat"]
	report_resh[report_resh$species_id==9999 & report_resh$group_id_iccat==9999, "species_desc"] <- "GRAND TOTAL"
	
	#check all month columns are there, and set 0 where NA
	month_names <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","TOTAL")
	for(month_name in month_names){
		if(!(month_name %in% colnames(report_resh))) report_resh[,month_name] <- NA
		report_resh[is.na(report_resh[,month_name]),month_name] <- 0
	}
	
	report_resh$year <- NULL
	report_resh <- report_resh[,c("species_id","species_desc","group_id_iccat","group_name_iccat","descriptor",month_names)]

	#add mean,var,sd
	report_resh_mean <- round2(rowMeans(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)
	report_resh_var <- round2(rowVar(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)		
	report_resh$"MEAN" <- report_resh_mean	
	report_resh$"VAR" <- report_resh_var		
	report_resh$"STD DEV" <- round2(sqrt(report_resh$VAR),2)
	
	#update MEAN
	items <- unique(report_resh$species_desc)
	for(variable in c("MEAN")){
		invisible(lapply(items, function(item){
			group_id_iccat <- unique(report_resh[report_resh$species_desc == item,"group_id_iccat"])
			#update L/T
			report_resh[report_resh$species_desc == item & report_resh$descriptor == "L/T",variable] <<- round2(report_resh[report_resh$species_desc == item & report_resh$descriptor == "LAN", variable] / report_resh[report_resh$species_id==9999 & report_resh$species_des == "GRAND TOTAL" & report_resh$descriptor == "TRP", variable],2)
			#update V/T
			report_resh[report_resh$species_desc == item & report_resh$descriptor == "V/T",variable] <<- round2(report_resh[report_resh$species_desc == item & report_resh$descriptor == "VAL", variable] / report_resh[report_resh$species_id==9999 & report_resh$species_des == "GRAND TOTAL" & report_resh$descriptor == "TRP", variable],2)
			#update P/K
			report_resh[report_resh$species_desc == item & report_resh$descriptor == "P/K",variable] <<- round2(report_resh[report_resh$species_desc == item & report_resh$descriptor == "VAL", variable] / report_resh[report_resh$species_desc == item & report_resh$descriptor == "LAN", variable],2)
		}))
	}
	
	
	#remove columns
	report_resh$"species_id" <- NULL
	report_resh$"group_id_iccat" <- NULL
	report_resh$"group_name_iccat" <- NULL
	#remove extra gear/species text by on subsequent descriptors other than LAN
	report_resh[report_resh$descriptor != "LAN", "species_desc"] <- ""
	#rename columns
	colnames(report_resh)[colnames(report_resh)=="species_desc"] <- "SPECIES"
	colnames(report_resh)[colnames(report_resh)=="descriptor"] <- "DESCRIPTOR"
	
	#add headers
	headers <- c(
		sprintf("SPECIES/MONTH SUMMARY OF TOTAL ESTIMATED LANDINGS FOR ARTISINAL FISHERIES OF TRINIDAD (INCLUDING TRAWLING) FOR %s", year),
		"Report ID: rf2_rep2"
	)
	attr(report_resh, "headers") <- headers
	#add footers
	footers <- c(
		"Trips for a species refers to the trips that caught the species.",
		"Landings/trip and Value/trip for a species is calculated using the total trips done by all gears and NOT the number of trips that caught the species.",
		"Trips are not standardized across gears."
	)
	attr(report_resh, "footers") <- footers
	
	return(report_resh)
	
}