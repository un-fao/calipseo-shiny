##
# Indicator for Artisanal fisheries 
# * Script name:
#     script_fd_artisanal_fisheries_reporting.R
# * Description:
#	  Prepares an Excel report artisanal fisheries effort indicator for Trinidad and Tobago fisheries
# * History:
# ------------- --------------- -----------------------
# Date			 Author			 Comment
# ------------- --------------- -----------------------
# 2018-11-12	eblondel    Creation.
# 2018-12-04	eblondel	Consolidation based on 2012/2013 datasets
# 2018-12-12	eblondel	Wrap logics into functions
# 2019-02-13	eblondel	generic report writer + handle multiple aggregations
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
require(openxlsx) #required for report fine-tuning

#functions
#---------

#report_1st_raised_effort
#@param effort a normalized dataset with effort statistical descriptors
#@returns a list of data.frame object ready for writing to Excel files
report_1st_raised_effort <- function(effort){

	#replace NA by NA strings
	effort[is.na(effort$month),]$month <- "NA"
	effort[is.na(effort$gear_id),]$gear_id <- 9999
	effort[is.na(effort$gear_name),]$gear_name <- "NA"

	beaches <- unique(effort$"bch_name")
	beaches <- beaches[order(beaches)]
	year <- unique(effort$year)[1]
	out_report_list <- lapply(beaches, function(bch_name){
		report <- effort[effort$bch_name == bch_name,]
		report <- report[order(report$month),]
		report$month <- sapply(report$month, function(x){
			return(switch(x, "1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY","6"="JUN","7"="JUL","8"="AUG","9"="SEP","10"="OCT","11"="NOV","12"="DEC", "NA"="TOTAL"))
		})
		
		#reshape to wide
		report_resh <- reshape(report, v.names = "value", idvar = c("bch_id", "bch_name", "gear_id", "gear_name", "descriptor"), timevar = "month", direction = "wide")
		start_values_idx <- which(regexpr("value.",colnames(report_resh))>0)[1]
		colnames(report_resh)[start_values_idx:ncol(report_resh)] <- sapply(colnames(report_resh)[start_values_idx:ncol(report_resh)], function(x){unlist(strsplit(x,"value."))[2]})
		colnames(report_resh)[start_values_idx-1] <- "descriptor"
		#sorting (requires to be done here!)
		report_resh <- report_resh[with(report_resh, order(gear_id)),]
		#merge columns
		report_resh$gear_name <- paste(report_resh$"gear_id",report_resh$"gear_name",sep="-")
		report_resh[report_resh$gear_name=="9999-NA", "gear_name"] <- "GRAND TOTAL"
		
		#remove extra gear/species text by on subsequent descriptors other than LAN
		report_resh[duplicated(report_resh$gear_name), "gear_name"] <- ""
		#check all month columns are there, and set 0 where NA
		month_names <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC", "TOTAL")
		for(month_name in month_names){
			if(!(month_name %in% colnames(report_resh))) report_resh[,month_name] <- NA
			report_resh[is.na(report_resh[,month_name]),month_name] <- 0
		}
		report_resh <- report_resh[,c("gear_id","gear_name","descriptor",month_names)]
		
		#remove columns from reporting
		report_resh$"bch_id" <- NULL
		report_resh$"bch_name" <- NULL
		
		#add mean,var,sd
		report_resh_mean <- round2(rowMeans(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)
		report_resh_var <- round2(rowVar(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)		
		report_resh$"MEAN" <- report_resh_mean	
		report_resh$"VAR" <- report_resh_var		
		report_resh$"STD DEV" <- round2(sqrt(report_resh$VAR),2)
		
		gears <- unique(report_resh$gear_id)
		gears <- gears[!sapply(gears, is.na)]
		for(variable in c("MEAN")){
			invisible(lapply(gears, function(gear_id){
				#update HRS/TRP
				report_resh[report_resh$gear_id == gear_id & report_resh$descriptor == "HRS/TRP",variable] <<- round2(report_resh[report_resh$gear_id == gear_id & report_resh$descriptor == "HOURS", variable] / report_resh[report_resh$gear_id == gear_id & report_resh$descriptor == "TRIPS", variable],2)
			}))
		}
		
		#update mean
		
		#remove columns from reporting
		report_resh$"gear_id" <- NULL
		report_resh$year <- NULL
		#rename columns
		colnames(report_resh)[colnames(report_resh)=="gear_name"] <- "GEAR"
		colnames(report_resh)[colnames(report_resh)=="descriptor"] <- "DESCRIPTOR"
		
		#headers
		headers <- c(
			sprintf("FISHING EFFORT BY GEAR FOR %s FOR %s", bch_name, year),
			"rf1_rep4"
		)
		attr(report_resh, "headers") <- headers
		footers <- c(
			"Boats refers to those operating on enumerated days and is NOT raised to account for non-enumerated days."
		)
		attr(report_resh, "footers") <- footers
		
		return(report_resh)
	})
	names(out_report_list) <- beaches

	return(out_report_list)
}

