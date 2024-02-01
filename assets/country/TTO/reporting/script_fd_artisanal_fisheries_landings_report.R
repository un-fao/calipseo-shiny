##
# Indicator for Artisanal fisheries 
# * Script name:
#     script_fd_artisanal_fisheries_reporting.R
# * Description:
#	  Prepares an Excel report artisanal fisheries landings indicator for Trinidad and Tobago fisheries
# * History:
# ------------- --------------- -----------------------
# Date			 Author			 Comment
# ------------- --------------- -----------------------
# 2018-11-12	eblondel    Creation.
# 2018-12-04	eblondel	Consolidation based on 2012/2013 datasets
# 2018-12-12	eblondel	Wrap logics into functions
# 2019-02-13	eblondel	generic report writer
# 2019-02-14	eblondel	subtotals / totals aggregations moved to computation script
# 2022-12-14  eblondel  coerce to data.frame to adapt to core shiny-calipseo computation model

#environment
#--------------------
#set your wd

#options
#--------------------
#options(stringsAsFactors = FALSE)

#package dependencies
#--------------------
#require(readxl)
#require(openxlsx) #required for report fine-tuning

#functions
#---------

#report_1st_raised_landings
#@param landings a normalized dataset with landings statistical descriptors
#@returns a list of data.frame object ready for writing to Excel files
report_1st_raised_landings <- function(landings){

  landings <- as.data.frame(landings)
  
	#replace NA by NA strings
	landings[is.na(landings$month),]$month <- "NA"
	landings[is.na(landings$gear_id),]$gear_id <- 9999
	class(landings$gear_id) <- "numeric"
	landings[is.na(landings$gear_name),]$gear_name <- "NA"
	landings[is.na(landings$species_id),]$species_id <- 9999
	class(landings$species_id) <- "numeric"
	landings[is.na(landings$species_name),]$species_name <- "NA"

	beaches <- unique(landings$"bch_name")
	beaches <- beaches[order(beaches)]
	year <- unique(landings$year)[1]
	out_report_list <- lapply(beaches, function(bch_name){
		report <- landings[landings$bch_name == bch_name,]
		report <- report[order(report$month),]
		report$month <- sapply(report$month, function(x){
			return(switch(x, "1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY","6"="JUN","7"="JUL","8"="AUG","9"="SEP","10"="OCT","11"="NOV","12"="DEC","NA"="TOTAL"))
		})
		
		#reshape to wide
		report_resh <- reshape(report, v.names = "value", idvar = c("bch_id", "bch_name", "gear_id", "gear_name", "species_id", "species_name", "descriptor"), timevar = "month", direction = "wide")
		start_values_idx <- which(regexpr("value.",colnames(report_resh))>0)[1]
		colnames(report_resh)[start_values_idx:ncol(report_resh)] <- sapply(colnames(report_resh)[start_values_idx:ncol(report_resh)], function(x){unlist(strsplit(x,"value."))[2]})
		colnames(report_resh)[start_values_idx-1] <- "descriptor"
		#sorting (requires to be done here!)
		report_resh <- report_resh[with(report_resh, order(gear_id, species_id)),]
		#merge columns
		report_resh$gear_name <- paste(report_resh$"gear_id",report_resh$"gear_name",sep="-")
		report_resh$species_name <- paste(report_resh$"species_id", report_resh$"species_name", sep="-")
		report_resh[report_resh$species_name=="9999-NA", "species_name"] <- "GEAR TOTAL"

		#check all month columns are there, and set 0 where NA
		month_names <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","TOTAL")
		for(month_name in month_names){
			if(!(month_name %in% colnames(report_resh))) report_resh[,month_name] <- NA
			report_resh[is.na(report_resh[,month_name]),month_name] <- 0
		}
		
		#remove bch columns from reporting
		report_resh$"bch_id" <- NULL
		report_resh$"bch_name" <- NULL
		report_resh$year <- NULL
		report_resh <- report_resh[,c("gear_id","gear_name","species_id","species_name","descriptor",month_names)]

		
		report_resh[nrow(report_resh)-5,"species_name"] <- "GRAND TOTAL"
		report_resh[(nrow(report_resh)-5):nrow(report_resh),"gear_id"] <- "gear_all"
		report_resh[(nrow(report_resh)-5):nrow(report_resh),"species_id"] <- 9999
		report_resh[(nrow(report_resh)-4):nrow(report_resh),"species_name"] <- ""
		report_resh[(nrow(report_resh)-5):nrow(report_resh),"gear_name"] <- ""
		report_resh[(nrow(report_resh)-5):nrow(report_resh), "descriptor"] <- c("LAN","VAL","TRP","L/T","V/T","P/K")
		
		#add mean,var,sd
		report_resh_mean <- round2(rowMeans(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)
		report_resh_var <- round2(rowVar(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)		
		report_resh$"MEAN" <- report_resh_mean	
		report_resh$"VAR" <- report_resh_var		
		report_resh$"STD DEV" <- round2(sqrt(report_resh$VAR),2)
		
		#update MEAN
		gears <- unique(report_resh$gear_id)
		gears <- gears[!sapply(gears, is.na)]
		for(variable in c("MEAN")){
			invisible(lapply(gears, function(gear_id){
				species <- unique(report_resh[report_resh$gear_id == gear_id,"species_id"])
				lapply(species, function(species_id){6
					#update L/T
					report_resh[report_resh$gear_id == gear_id & report_resh$species_id == species_id & report_resh$descriptor == "L/T",variable] <<- round2(report_resh[report_resh$gear_id == gear_id & report_resh$species_id == species_id & report_resh$descriptor == "LAN", variable] / report_resh[report_resh$gear_id == gear_id & report_resh$species_id == 9999 & report_resh$descriptor == "TRP", variable],2)
					#update V/T
					report_resh[report_resh$gear_id == gear_id & report_resh$species_id == species_id & report_resh$descriptor == "V/T", variable] <<- round2(report_resh[report_resh$gear_id == gear_id & report_resh$species_id == species_id & report_resh$descriptor == "VAL", variable] / report_resh[report_resh$gear_id == gear_id & report_resh$species_id == 9999 & report_resh$descriptor == "TRP", variable],2)
					#update P/K
					report_resh[report_resh$gear_id == gear_id & report_resh$species_id == species_id & report_resh$descriptor == "P/K", variable] <<- round2(report_resh[report_resh$gear_id == gear_id & report_resh$species_id == species_id & report_resh$descriptor == "VAL", variable] / report_resh[report_resh$gear_id == gear_id & report_resh$species_id == species_id & report_resh$descriptor == "LAN", variable],2)	
				})
			}))
		}
		
		
		#remove gear/species id
		report_resh$"gear_id" <- NULL
		report_resh$"species_id" <- NULL
		#remove extra gear/species text by on subsequent descriptors other than LAN
		report_resh[duplicated(report_resh$gear_name), "gear_name"] <- ""
		report_resh[report_resh$descriptor != "LAN", "species_name"] <- ""
		#rename columns
		colnames(report_resh)[colnames(report_resh)=="gear_name"] <- "GEAR"
		colnames(report_resh)[colnames(report_resh)=="species_name"] <- "SPECIES"
		colnames(report_resh)[colnames(report_resh)=="descriptor"] <- "DESCRIPTOR"
		
		#add headers
		headers <- c(
			sprintf("GEAR/SPECIES/MONTH RAISED LANDING (KG) REPORT FOR NON ENUMERATED FISHING DAYS FOR %s FOR ENUMERATED BEACH %s", year, bch_name),
			"rf1.rep2"
		)
		attr(report_resh, "headers") <- headers
		#add footers
		footers <- c(
			"Landings/trip and Value/Trip for a particular species for a particular gear is calculated using the total trips done by the gear and NOT the number of trips that caught the species_id",
			"Trips are not standardized across gears."
		)
		attr(report_resh, "footers") <- footers
		
		return(report_resh)
	})
	names(out_report_list) <- beaches
	return(out_report_list)
}

