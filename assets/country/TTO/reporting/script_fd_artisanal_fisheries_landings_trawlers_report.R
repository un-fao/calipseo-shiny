##
# Indicator for Artisanal fisheries 
# * Script name:
#     script_fd_artisanal_fisheries_landings_trawlers_report.R
# * Description:
#	  Prepares an Excel report artisanal fisheries trawler landings indicator for Trinidad and Tobago fisheries
# * History:
# ------------- --------------- -----------------------
# Date			 Author			 Comment
# ------------- --------------- -----------------------
# 2018-12-20	eblondel    Creation.
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

#report_1st_raised_trawl_landings
#@param landings a normalized dataset with landings statistical descriptors
#@param by aggregation group (species or species_group)
#@returns a list of data.frame object ready for writing to Excel files
report_1st_raised_trawl_landings <- function(landings, by = "species"){

  landings <- as.data.frame(landings)

	#replace NA by NA strings
	landings[is.na(landings$month),]$month <- "NA"
	landings[is.na(landings$vesstype),]$vesstype <- "NA"
	landings[is.na(landings$gr_f_area_id),]$gr_f_area_id <- 9999
	class(landings$gr_f_area_id) <- "numeric"
	landings[is.na(landings$gr_f_area),]$gr_f_area <- "NA"
	landings[is.na(landings$species_item_id),]$species_item_id <- 9999
	class(landings$species_item_id) <- "numeric"
	landings[is.na(landings$species_item_desc),]$species_item_desc <- "NA"

	if(!(by %in% c("species","species_group"))){
		stop("'by' parameter should be either 'species' or 'species_group'")
	}
	bySpecies <- by == "species"
	bySpeciesGroup <- by == "species_group"

	month_names <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","TOTAL")
	
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
		report_resh <- reshape(report, v.names = "value", idvar = c("bch_id", "bch_name", "vesstype","species_item_id","species_item_desc","gr_f_area_id","gr_f_area", "descriptor"), timevar = "month", direction = "wide")
		start_values_idx <- which(regexpr("value.",colnames(report_resh))>0)[1]
		colnames(report_resh)[start_values_idx:ncol(report_resh)] <- sapply(colnames(report_resh)[start_values_idx:ncol(report_resh)], function(x){unlist(strsplit(x,"value."))[2]})
		colnames(report_resh)[start_values_idx-1] <- "descriptor"
		#sorting (requires to be done here!)
		report_resh <- report_resh[with(report_resh, order(vesstype,species_item_id,gr_f_area_id)),]
		#merge columns
		report_resh$vesstype <- paste("V:", report_resh$"vesstype", sep=" ")
		report_resh$species_item_desc <- paste(report_resh$"species_item_id", report_resh$"species_item_desc", sep=" ")
		if(bySpeciesGroup) report_resh$species_item_desc <- paste("G:", report_resh$species_item_desc)
		report_resh$gr_f_area <- paste("F:", paste(report_resh$"gr_f_area_id", report_resh$"gr_f_area",sep="-"), sep=" ")
		
		#add strata on single column
		strata <- t(unique(report_resh[,c("vesstype","species_item_desc","gr_f_area")]))
		
		report_resh <- cbind(
			strata_id = do.call(c,lapply(colnames(strata), function(col){rep(paste(strata[,col],collapse="_"),6)})),
			strata = do.call(c,lapply(colnames(strata),function(col){c(strata[,col],rep("",3))})),
			report_resh[,9:ncol(report_resh)]
		)
		#check all month columns are there, and set 0 where NA
		for(month_name in month_names){
			#on report_resh
			if(!(month_name %in% colnames(report_resh))) report_resh[,month_name] <- NA
			report_resh[is.na(report_resh[,month_name]),month_name] <- 0
		}
		report_resh <- report_resh[,c("strata_id", "strata","descriptor",month_names)]
		
		report_resh_mean <- round2(rowMeans(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)
		report_resh_var <- round2(rowVar(report_resh[,colnames(report_resh)[(ncol(report_resh)-12):(ncol(report_resh)-1)]], na.rm = TRUE),2)		
		report_resh$"MEAN" <- report_resh_mean	
		report_resh$"VAR" <- report_resh_var		
		report_resh$"STD DEV" <- round2(sqrt(report_resh$VAR),2)
		
		#update MEAN
		stratas <- unique(report_resh$strata_id)
		stratas <- stratas[!sapply(stratas, is.na)]
		variable = "MEAN"
		invisible(lapply(stratas, function(strata_id){
			#update L/T
			report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "L/T",variable] <<- round2(report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "LAN", variable] / (report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "LAN", "TOTAL"]/report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "L/T", "TOTAL"]/12),2)
			#update V/T
			report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "V/T",variable] <<- round2(report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "VAL", variable] / (report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "LAN", "TOTAL"]/report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "L/T", "TOTAL"]/12),2)		
			#update P/K
			report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "P/K",variable] <<- round2(report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "VAL", variable] / report_resh[report_resh$strata_id == strata_id & report_resh$descriptor == "LAN", variable],2)
		}))
		
		
		#renaming
		report_resh[report_resh$strata %in% c("G: 9999 NA","F: 9999-NA", "9999 NA"), "strata"] <- ""
		report_resh[report_resh$strata == "V: NA","strata"] <- "GRAND TOTAL"
		colnames(report_resh)[colnames(report_resh)=="strata"] <- "STRATA"
		colnames(report_resh)[colnames(report_resh)=="descriptor"] <- "DESCRIPTOR"
		report_resh$strata_id <- NULL
		
		#header level
		level <- switch(by, "species" = "SPECIES", "species_group" = "SPECIES GROUP")
		
		#headers
		headers <- c(
			sprintf("TRAWLING 1st RAISED LANDINGS BY VESSEL TYPE/%s/FISHING AREA AT ENUMERATED BEACH: %s FOR YEAR %s", level, bch_name, year),
			"Report ID:rf4_rep1a"
		)
		attr(report_resh, "headers") <- headers
		footers <- c(
			"Trips for a species refers to the trips that caught the species.",
			"Landings/Trip and Value/trip for a species is calculated using the total trips done by the particular trawl type in the particular area and NOT the number of trips that caught the species"
		)
		attr(report_resh, "footers") <- footers
		return(report_resh)
	})
	names(out_report_list) <- beaches
	return(out_report_list)
}
