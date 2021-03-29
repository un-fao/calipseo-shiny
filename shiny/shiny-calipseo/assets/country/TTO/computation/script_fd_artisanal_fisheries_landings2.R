##
# Indicator for Artisanal fisheries 
# * Script name:
#     script_fd_artisanal_fisheries_landings2.R
# * Description:
#	  Computes artisanal fisheries 2d raised landings indicator, by species/subtotals for Trinidad and Tobago fisheries
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
#options(stringsAsFactors = FALSE)

#package dependencies
#--------------------

#functions
#---------

#compute_2nd_raised_landings_by_SPECIES
#@param raw_data the raw data, required for computing number of trips by month/species_group
#@param landings_1 a standardized raw data set
#@param raised_1 a normalized dataset of 1st raising factors
#@param raised_2 a normalized dataset of 2nd raising factors
#@param zones an association dataset between beaches and zones
#@param species_groups an association dataset between species and species groups
#@returns the landings statistical descriptors in normalized data.frame
#----------------------------------------------------------------------------------------------
compute_2nd_raised_landings_by_SPECIES <- function(raw_data, landings_1, raised_1, raised_2, zones, species_groups){

	#TRIP NUMBERS
	#-----------------------------------------------
	#For TRIP numbers, this is not a simple addition
	#for that, we need to take raw data, get nb of trips by species group
	raw_data_01 <- raise_raw_data(raw_data, raised_1)
	
	raw_data_01 <- raw_data_01[!(raw_data_01$bch_name %in%  c("CACANDEE", "TYPE IV")),]
	
	raw_data_01$act <- NULL
	raw_data_01$enum <- NULL
	raw_data_01_zones <- merge(
		x = raw_data_01,
		y = zones,
		by.x = "bch_name",
		by.y = "beach_name",
		all.x = TRUE,
		all.y = FALSE
	)
	raw_data_01_zones[is.na(raw_data_01_zones $zone), "zone"] <- -99
	raw_data_01_raised_2 <- merge(
		x = raw_data_01_zones,
		y = raised_2,
		by.x = c("zone","month"),
		by.y = c("zone#","month"),
		all.x = TRUE,
		all.y = FALSE
	)
	raw_data_01_sp <- merge(
		x = raw_data_01_raised_2,
		y = species_groups,
		by.x = "species_id",
		by.y = "species_id",
		all.x = TRUE,
		all.y = FALSE
	)
	raw_data_01_sp[is.na(raw_data_01_sp$act),"act"] <- 1
	raw_data_01_sp[is.na(raw_data_01_sp$enum),"enum"] <- 1
	raw_data_01_sp$raised_trp <- raw_data_01_sp$raised_trp* as.numeric(raw_data_01_sp$act) / as.numeric(raw_data_01_sp$enum)
	colnames(raw_data_01_sp)[which(colnames(raw_data_01_sp)== "month.x")] <- "month"
	colnames(raw_data_01_sp)[which(colnames(raw_data_01_sp)== "year.x")] <- "year"
	colnames(raw_data_01_sp)[which(colnames(raw_data_01_sp)=="species_desc.x")] <- "species_desc"
	raw_data_01_sp$"month.y" <- NULL
	raw_data_01_sp$"year.y" <- NULL
	
	#number of trips by month/group
	raw_data_01_trp_species <- unique(raw_data_01_sp[,c("landing_id","year","month", "species_id", "species_desc", "group_id_subt", "group_name_subt", "raised_trp")])
	IGNORE_DIMENSION <- rep("",nrow(raw_data_01_trp_species))
	trp_by_species <- aggregate(
		x = raw_data_01_trp_species$raised_trp,
		by = list(
			species_id = raw_data_01_trp_species$species_id,
			species_desc = raw_data_01_trp_species$species_desc,
			group_id_subt = raw_data_01_trp_species$group_id_subt,
			group_name_subt = raw_data_01_trp_species$group_name_subt,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = rep("TRP", nrow(raw_data_01_trp_species)),
			year = raw_data_01_trp_species$year,
			month = raw_data_01_trp_species$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	)
	colnames(trp_by_species)[length(colnames(trp_by_species))] <- "value"
	
	#number of trips by month/group
	raw_data_01_trp_speciesgroup <- unique(raw_data_01_sp[,c("landing_id","year","month", "group_id_subt", "group_name_subt", "raised_trp")])
	IGNORE_DIMENSION <- rep("",nrow(raw_data_01_trp_speciesgroup))
	trp_by_speciesgroup <- aggregate(
		x = raw_data_01_trp_speciesgroup$raised_trp,
		by = list(
			species_id = IGNORE_DIMENSION,
			species_desc = IGNORE_DIMENSION,
			group_id_subt = raw_data_01_trp_speciesgroup$group_id_subt,
			group_name_subt = raw_data_01_trp_speciesgroup$group_name_subt,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = rep("TRP", nrow(raw_data_01_trp_speciesgroup)),
			year = raw_data_01_trp_speciesgroup$year,
			month = raw_data_01_trp_speciesgroup$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	)
	colnames(trp_by_speciesgroup)[length(colnames(trp_by_speciesgroup))] <- "value"
	
	#number of trips by month/group
	raw_data_01_trp_total <- unique(raw_data_01_sp[,c("landing_id","year","month", "raised_trp")])
	IGNORE_DIMENSION <- rep("",nrow(raw_data_01_trp_total))
	trp_by_total <- aggregate(
		x = raw_data_01_trp_total$raised_trp,
		by = list(
			species_id = rep("all",nrow(raw_data_01_trp_total)),
			species_desc = rep("all",nrow(raw_data_01_trp_total)),
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = rep("TRP", nrow(raw_data_01_trp_total)),
			year = raw_data_01_trp_total$year,
			month = raw_data_01_trp_total$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	)
	colnames(trp_by_total)[length(colnames(trp_by_total))] <- "value"

	#compute statistics other TRP number, from 1st raised landings
	#---------------------------------------------------------------------------
	df_with_zones <- merge(
		x = landings_1,
		y = zones,
		by.x = "bch_name",
		by.y = "beach_name",
		all.x = TRUE,
		all.y = FALSE
	)
	df_with_zones[is.na(df_with_zones$zone), "zone"] <- -99
	
	df_with_raised_2 <- merge(
		x = df_with_zones,
		y = raised_2,
		by.x = c("zone","month"),
		by.y = c("zone#","month"),
		all.x = TRUE,
		all.y = FALSE
	)
	
	df_with_raised_2[is.na(df_with_raised_2$act),"act"] <- 1
	df_with_raised_2[is.na(df_with_raised_2$enum),"enum"] <- 1
	df_with_raised_2$value <- df_with_raised_2$value* as.numeric(df_with_raised_2$act) / as.numeric(df_with_raised_2$enum)
	colnames(df_with_raised_2)[which(colnames(df_with_raised_2)== "month.x")] <- "month"
	colnames(df_with_raised_2)[which(colnames(df_with_raised_2)== "year.x")] <- "year"
	df_with_raised_2$"month.y" <- NULL
	df_with_raised_2$"year.y" <- NULL
	
	#we remove the trips
	df_with_raised_2 <- df_with_raised_2[df_with_raised_2$descriptor != "TRP",]
	df_with_raised_2 <- df_with_raised_2[!(df_with_raised_2$bch_name %in%  c("CACANDEE", "TYPE IV")),]
	
	#filtering out aggregated values
	without_totals <- !is.na(df_with_raised_2$gear_id) & !is.na(df_with_raised_2$species_id) & !is.na(df_with_raised_2$month)
	df1 <- df_with_raised_2[without_totals,]
	df1_totals <- df_with_raised_2[!without_totals,]
	df1_totals_by_month <- df1_totals[is.na(df1_totals$gear_id) & is.na(df1_totals$species_id) & !is.na(df1_totals$month),]
		
	#we first compute the aggregate by month of all species we need that for ratios L/T, V/T, P/K
	IGNORE_DIMENSION <- rep("",nrow(df1_totals_by_month))
	totals_by_month <- aggregate(
		x = df1_totals_by_month$value,
		by = list(
			species_id = rep("all",nrow(df1_totals_by_month)),
			species_desc = rep("all",nrow(df1_totals_by_month)),
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = df1_totals_by_month$descriptor,
			year = df1_totals_by_month$year,
			month = df1_totals_by_month$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE),2)}
	)
	colnames(totals_by_month)[ncol(totals_by_month)] <- "value"
	
	#merge with groups
	df2 <- merge(
		x = df1,
		y = species_groups,
		by.x = "species_id",
		by.y = "species_id",
		all.x = TRUE,
		all.y = FALSE
	)
	for(coln in colnames(df2)){
		df2[!is.na(df2[,coln]) & df2[,coln] == "NULL", coln] <- NA
	}
	
	
	#PROCEED WITH AGGREGATIONS
	#============================================================================================
	IGNORE_DIMENSION <- rep("",nrow(df2))
	
	#AGGREGATION BY SPECIES & SPECIES SUBGROUP
	#--------------------------------------------------------------------------------------------
	
	if(any(is.na(df2$group_id_subt))) df2[is.na(df2$group_id_subt),]$group_id_subt <- -1
	if(any(is.na(df2$group_name_subt))) df2[is.na(df2$group_name_subt),]$group_name_subt <- "NONE"
	
	#aggregate by species
	df_species <- aggregate(
		x = df2$value,
		by = list(
			species_id = df2$species_id,
			species_desc = df2$species_desc,
			group_id_subt = df2$group_id_subt,
			group_name_subt = df2$group_name_subt,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = df2$descriptor,
			year = df2$year,
			month = df2$month
		),
		FUN = function(x){round2(sum(x, na.rm=TRUE),2)}
	)
	colnames(df_species)[ncol(df_species)] <- "value"
	
	#aggregate by species group subt
	df_subt <- aggregate(
		x = df2$value,
		by = list(
			species_id = IGNORE_DIMENSION,
			species_desc = IGNORE_DIMENSION,
			group_id_subt = df2$group_id_subt,
			group_name_subt = df2$group_name_subt,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = df2$descriptor,
			year = df2$year,
			month = df2$month
		),
		FUN = function(x){round2(sum(x, na.rm=T),2)}
	)
	colnames(df_subt)[ncol(df_subt)] <- "value"
	
	#we do rbind to add TRP descriptor
	df_species <- rbind(df_species, trp_by_species)
	df_subt <- rbind(df_subt, trp_by_speciesgroup)
	totals_by_month <- rbind(totals_by_month, trp_by_total)
	
	#we add species + species subgroups + grand totals
	df_out <- do.call("rbind", list(df_species, df_subt, totals_by_month))	

	#aggregate on yearly total
	#-------------------------
	IGNORE_DIMENSION <- rep("",nrow(totals_by_month))
	#df_out$pk <- paste(df_out$descriptor, df_out$species_id, df_out$group_id, df_out$group_id_subt, df_out$group_id_iccat, df_out$group_id_fao, sep="_")
	df_tot <- aggregate(
		x = totals_by_month$value,
		by = list(
			#pk = totals_by_month$pk,
			species_id = totals_by_month$species_id,
			species_desc = totals_by_month$species_desc,
			group_id_subt = totals_by_month$group_id_subt,
			group_name_subt = totals_by_month$group_name_subt,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = totals_by_month$descriptor,
			year = totals_by_month$year,
			month = rep("all", nrow(totals_by_month))
		),
		FUN = function(x){round2(sum(x, na.rm=TRUE),2)}
	)
	#df_tot$pk <- NULL
	#df_out$pk <- NULL
	colnames(df_tot)[ncol(df_tot)] <- "value"

	#aggregate on yearly total by species
	#-------------------------
	IGNORE_DIMENSION <- rep("",nrow(df_out))
	#df_out$pk <- paste(df_out$descriptor, df_out$species_id, df_out$group_id, df_out$group_id_subt, df_out$group_id_iccat, df_out$group_id_fao, sep="_")
	df_tot_sp <- aggregate(
		x = df_out$value,
		by = list(
			species_id = df_out$species_id,
			species_desc = df_out$species_desc,
			group_id_subt = df_out$group_id_subt,
			group_name_subt = df_out$group_name_subt,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = df_out$descriptor,
			year = df_out$year,
			month = rep("all", nrow(df_out))
		),
		FUN = function(x){round2(sum(x, na.rm=TRUE),2)}
	)
	#df_tot$pk <- NULL
	#df_out$pk <- NULL
	colnames(df_tot_sp)[ncol(df_tot_sp)] <- "value"	
	
	#aggregate on yearly total by species group
	#-------------------------
	IGNORE_DIMENSION <- rep("",nrow(df_out))
	#df_out$pk <- paste(df_out$descriptor, df_out$species_id, df_out$group_id, df_out$group_id_subt, df_out$group_id_iccat, df_out$group_id_fao, sep="_")
	df_tot_spgroup <- aggregate(
		x = df_out$value,
		by = list(
			species_id = IGNORE_DIMENSION,
			species_desc = IGNORE_DIMENSION,
			group_id_subt = df_out$group_id_subt,
			group_name_subt = df_out$group_name_subt,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = df_out$descriptor,
			year = df_out$year,
			month = rep("all", nrow(df_out))
		),
		FUN = function(x){round2(sum(x, na.rm=TRUE),2)}
	)
	#df_tot$pk <- NULL
	#df_out$pk <- NULL
	colnames(df_tot_spgroup)[ncol(df_tot_spgroup)] <- "value"	
	
	#merging
	df_out <- rbind(df_out, df_tot, df_tot_sp, df_tot_spgroup)
	
	#we recalculate L/T, V/T, P/K ratios
	#here the same totals are used for species & subgroups
	invisible(lapply(c(1:12,"all"), function(month){
		if(month %in% 1:12){
			df_out[df_out$month == month & df_out$descriptor=="L/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="LAN", "value"] / trp_by_total[trp_by_total$month==month & trp_by_total$descriptor=="TRP","value"],2)
			df_out[df_out$month == month & df_out$descriptor=="V/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="VAL", "value"] / trp_by_total[trp_by_total$month==month & trp_by_total$descriptor=="TRP","value"],2)
		}else{
			df_out[df_out$month == month & df_out$descriptor=="L/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="LAN", "value"] / df_out[df_out$species_id == "all" & df_out$month == month & df_out$descriptor=="TRP", "value"],2)
			df_out[df_out$month == month & df_out$descriptor=="V/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="VAL", "value"] / df_out[df_out$species_id == "all" & df_out$month == month & df_out$descriptor=="TRP", "value"],2)
		}
		df_out[df_out$month == month & df_out$descriptor=="P/K", "value"] <<- round2(df_out[df_out$month == month & df_out$descriptor=="VAL", "value"]/df_out[df_out$month == month  & df_out$descriptor=="LAN", "value"],2)
	}))
	
	#round no decimals for TRP
	df_out[df_out$descriptor == "TRP", "value"] <- round2(df_out[df_out$descriptor == "TRP", "value"])
	
	#we remove the false group
	df_out <- df_out[df_out$group_id_subt != -1,]
	df_out[df_out$group_id_subt == -1, "group_id_subt"] <- NA
	df_out[df_out$group_name_subt == "NONE", "group_name_subt"] <- NA
	
	#renaming all values
	df_out[df_out$species_id == "all", "species_id"] <- NA
	df_out[df_out$species_desc == "all", "species_desc"] <- ""
	df_out[df_out$species_desc == "", "species_desc"] <- NA
	df_out[df_out$month == "all", "month"] <- NA
	df_out[df_out$group_id == "", "group_id"] <- NA
	df_out[df_out$group_name == "", "group_name"] <- NA
	df_out[!is.na(df_out$group_id_subt) & df_out$group_id_subt == "", "group_id_subt"] <- NA
	df_out[!is.na(df_out$group_name_subt) & df_out$group_name_subt == "", "group_name_subt"] <- NA
	df_out[df_out$group_id_iccat == "", "group_id_iccat"] <- NA
	df_out[df_out$group_name_iccat == "", "group_name_iccat"] <- NA
	df_out[df_out$group_id_fao == "", "group_id_fao"] <- NA
	df_out[df_out$group_name_fao == "", "group_name_fao"] <- NA	
	#rounding
	df_out$value <- round2(df_out$value, 2)
	
	return(df_out)
}


#compute_2nd_raised_landings_by_FAOGROUP
#@param raw_data a standardized raw data set
#@param landings_1 a standardized landings data set
#@param raised_1 a normalized dataset of 1st raising factors
#@param raised_2 a normalized dataset of 2nd raising factors
#@param zones an association dataset between beaches and zones
#@param species_groups an association dataset between species and species groups
#@returns the landings statistical descriptors in normalized data.frame
#----------------------------------------------------------------------------------------------
compute_2nd_raised_landings_by_FAOGROUP <- function(raw_data, landings_1, raised_1, raised_2, zones, species_groups){

	#TRIP NUMBERS
	#-----------------------------------------------
	#For TRIP numbers, this is not a simple addition
	#for that, we need to take raw data, get nb of trips by species group
	raw_data_01 <- raise_raw_data(raw_data, raised_1)
	
	raw_data_01 <- raw_data_01[!(raw_data_01$bch_name %in%  c("CACANDEE", "TYPE IV")),]
	
	raw_data_01$act <- NULL
	raw_data_01$enum <- NULL
	raw_data_01_zones <- merge(
		x = raw_data_01,
		y = zones,
		by.x = "bch_name",
		by.y = "beach_name",
		all.x = TRUE,
		all.y = FALSE
	)
	raw_data_01_zones[is.na(raw_data_01_zones $zone), "zone"] <- -99
	raw_data_01_raised_2 <- merge(
		x = raw_data_01_zones,
		y = raised_2,
		by.x = c("zone","month"),
		by.y = c("zone#","month"),
		all.x = TRUE,
		all.y = FALSE
	)
	raw_data_01_sp <- merge(
		x = raw_data_01_raised_2,
		y = species_groups,
		by.x = "species_id",
		by.y = "species_id",
		all.x = TRUE,
		all.y = FALSE
	)
	raw_data_01_sp[is.na(raw_data_01_sp$act),"act"] <- 1
	raw_data_01_sp[is.na(raw_data_01_sp$enum),"enum"] <- 1
	raw_data_01_sp$raised_trp <- raw_data_01_sp$raised_trp* as.numeric(raw_data_01_sp$act) / as.numeric(raw_data_01_sp$enum)
	colnames(raw_data_01_sp)[which(colnames(raw_data_01_sp)== "month.x")] <- "month"
	colnames(raw_data_01_sp)[which(colnames(raw_data_01_sp)== "year.x")] <- "year"
	colnames(raw_data_01_sp)[which(colnames(raw_data_01_sp)=="species_desc.x")] <- "species_desc"
	raw_data_01_sp$"month.y" <- NULL
	raw_data_01_sp$"year.y" <- NULL
	
	#number of trips by month/group
	raw_data_01_trp_species <- unique(raw_data_01_sp[,c("landing_id","year","month", "species_id", "species_desc", "group_id_fao", "group_name_fao", "raised_trp")])
	IGNORE_DIMENSION <- rep("",nrow(raw_data_01_trp_species))
	trp_by_species <- aggregate(
		x = raw_data_01_trp_species$raised_trp,
		by = list(
			species_id = raw_data_01_trp_species$species_id,
			species_desc = raw_data_01_trp_species$species_desc,
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = raw_data_01_trp_species$group_id_fao,
			group_name_fao = raw_data_01_trp_species$group_name_fao,
			descriptor = rep("TRP", nrow(raw_data_01_trp_species)),
			year = raw_data_01_trp_species$year,
			month = raw_data_01_trp_species$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	)
	colnames(trp_by_species)[length(colnames(trp_by_species))] <- "value"
	
	#number of trips by month/group
	raw_data_01_trp_fao <- unique(raw_data_01_sp[,c("landing_id","year","month", "group_id_fao", "group_name_fao", "raised_trp")])
	IGNORE_DIMENSION <- rep("",nrow(raw_data_01_trp_fao))
	trp_by_faogroup <- aggregate(
		x = raw_data_01_trp_fao$raised_trp,
		by = list(
			species_id = IGNORE_DIMENSION,
			species_desc = IGNORE_DIMENSION,
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = raw_data_01_trp_fao$group_id_fao,
			group_name_fao = raw_data_01_trp_fao$group_name_fao,
			descriptor = rep("TRP", nrow(raw_data_01_trp_fao)),
			year = raw_data_01_trp_fao$year,
			month = raw_data_01_trp_fao$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	)
	colnames(trp_by_faogroup)[length(colnames(trp_by_faogroup))] <- "value"
	
	#number of trips by month/group
	raw_data_01_trp_total <- unique(raw_data_01_sp[,c("landing_id","year","month", "raised_trp")])
	IGNORE_DIMENSION <- rep("",nrow(raw_data_01_trp_total))
	trp_by_total <- aggregate(
		x = raw_data_01_trp_total$raised_trp,
		by = list(
			species_id = rep("all",nrow(raw_data_01_trp_total)),
			species_desc = rep("all",nrow(raw_data_01_trp_total)),
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = rep("TRP", nrow(raw_data_01_trp_total)),
			year = raw_data_01_trp_total$year,
			month = raw_data_01_trp_total$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	)
	colnames(trp_by_total)[length(colnames(trp_by_total))] <- "value"

	#compute statistics other TRP number, from 1st raised landings
	#---------------------------------------------------------------------------
	df_with_zones <- merge(
		x = landings_1,
		y = zones,
		by.x = "bch_name",
		by.y = "beach_name",
		all.x = TRUE,
		all.y = FALSE
	)
	df_with_zones[is.na(df_with_zones$zone), "zone"] <- -99
	
	df_with_raised_2 <- merge(
		x = df_with_zones,
		y = raised_2,
		by.x = c("zone","month"),
		by.y = c("zone#","month"),
		all.x = TRUE,
		all.y = FALSE
	)
	
	df_with_raised_2[is.na(df_with_raised_2$act),"act"] <- 1
	df_with_raised_2[is.na(df_with_raised_2$enum),"enum"] <- 1
	df_with_raised_2$value <- df_with_raised_2$value* as.numeric(df_with_raised_2$act) / as.numeric(df_with_raised_2$enum)
	colnames(df_with_raised_2)[which(colnames(df_with_raised_2)== "month.x")] <- "month"
	colnames(df_with_raised_2)[which(colnames(df_with_raised_2)== "year.x")] <- "year"
	df_with_raised_2$"month.y" <- NULL
	df_with_raised_2$"year.y" <- NULL
	
	#we remove the trips
	df_with_raised_2 <- df_with_raised_2[df_with_raised_2$descriptor != "TRP",]
	df_with_raised_2 <- df_with_raised_2[!(df_with_raised_2$bch_name %in%  c("CACANDEE", "TYPE IV")),]
	
	#filtering out aggregated values
	without_totals <- !is.na(df_with_raised_2$gear_id) & !is.na(df_with_raised_2$species_id) & !is.na(df_with_raised_2$month)
	df1 <- df_with_raised_2[without_totals,]
	df1_totals <- df_with_raised_2[!without_totals,]
	df1_totals_by_month <- df1_totals[is.na(df1_totals$gear_id) & is.na(df1_totals$species_id) & !is.na(df1_totals$month),]
		
	#we first compute the aggregate by month of all species we need that for ratios L/T, V/T, P/K
	IGNORE_DIMENSION <- rep("",nrow(df1_totals_by_month))
	totals_by_month <- aggregate(
		x = df1_totals_by_month$value,
		by = list(
			species_id = rep("all",nrow(df1_totals_by_month)),
			species_desc = rep("all",nrow(df1_totals_by_month)),
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = df1_totals_by_month$descriptor,
			year = df1_totals_by_month$year,
			month = df1_totals_by_month$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE),2)}
	)
	colnames(totals_by_month)[ncol(totals_by_month)] <- "value"
	
	#merge with groups
	df2 <- merge(
		x = df1,
		y = species_groups,
		by.x = "species_id",
		by.y = "species_id",
		all.x = TRUE,
		all.y = FALSE
	)
	for(coln in colnames(df2)){
		df2[!is.na(df2[,coln]) & df2[,coln] == "NULL", coln] <- NA
	}
	
	
	#PROCEED WITH AGGREGATIONS
	#============================================================================================
	IGNORE_DIMENSION <- rep("",nrow(df2))
	
	#AGGREGATION BY SPECIES & SPECIES SUBGROUP
	#--------------------------------------------------------------------------------------------
	
	if(any(is.na(df2$group_id_fao))) df2[is.na(df2$group_id_fao),]$group_id_fao <- -1
	if(any(is.na(df2$group_name_fao)))df2[is.na(df2$group_name_fao),]$group_name_fao <- "NONE"
	
	#aggregate by species
	df_species <- aggregate(
		x = df2$value,
		by = list(
			species_id = df2$species_id,
			species_desc = df2$species_desc,
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = df2$group_id_fao,
			group_name_fao = df2$group_name_fao,
			descriptor = df2$descriptor,
			year = df2$year,
			month = df2$month
		),
		FUN = function(x){round2(sum(x, na.rm=TRUE),2)}
	)
	colnames(df_species)[ncol(df_species)] <- "value"
	
	#aggregate by species group subt
	df_fao <- aggregate(
		x = df2$value,
		by = list(
			species_id = IGNORE_DIMENSION,
			species_desc = IGNORE_DIMENSION,
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = df2$group_id_fao,
			group_name_fao = df2$group_name_fao,
			descriptor = df2$descriptor,
			year = df2$year,
			month = df2$month
		),
		FUN = function(x){round2(sum(x, na.rm=T),2)}
	)
	colnames(df_fao)[ncol(df_fao)] <- "value"
	
	#we do rbind to add TRP value
	df_fao <- rbind(df_fao, trp_by_faogroup)
	totals_by_month <- rbind(totals_by_month, trp_by_total)
	#we add species + species subgroups + grand totals
	df_out <- do.call("rbind", list(df_fao, totals_by_month))	

	#aggregate on yearly total
	#-------------------------
	IGNORE_DIMENSION <- rep("",nrow(totals_by_month))
	#totals_by_month$pk <- paste(df_out$descriptor, df_out$species_id, df_out$group_id, df_out$group_id_subt, df_out$group_id_iccat, df_out$group_id_fao, sep="_")
	df_tot <- aggregate(
		x = totals_by_month$value,
		by = list(
			#pk = totals_by_month$pk,
			species_id = totals_by_month$species_id,
			species_desc = totals_by_month$species_desc,
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = totals_by_month$group_id_fao,
			group_name_fao = totals_by_month$group_name_fao,
			descriptor = totals_by_month$descriptor,
			year = totals_by_month$year,
			month = rep("all", nrow(totals_by_month))
		),
		FUN = function(x){round2(sum(x, na.rm=TRUE),2)}
	)
	#df_tot$pk <- NULL
	#totals_by_month$pk <- NULL
	colnames(df_tot)[ncol(df_tot)] <- "value"
	
	#aggregate on yearly total by FAO species group
	#-------------------------
	IGNORE_DIMENSION <- rep("",nrow(df_out))
	#df_out$pk <- paste(df_out$descriptor, df_out$species_id, df_out$group_id, df_out$group_id_subt, df_out$group_id_iccat, df_out$group_id_fao, sep="_")
	df_tot_faogroup <- aggregate(
		x = df_out$value,
		by = list(
			species_id = IGNORE_DIMENSION,
			species_desc = IGNORE_DIMENSION,
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = df_out$group_id_fao,
			group_name_fao = df_out$group_name_fao,
			descriptor = df_out$descriptor,
			year = df_out$year,
			month = rep("all", nrow(df_out))
		),
		FUN = function(x){round2(sum(x, na.rm=TRUE),2)}
	)
	#df_tot$pk <- NULL
	#df_out$pk <- NULL
	colnames(df_tot_faogroup)[ncol(df_tot_faogroup)] <- "value"	

	#merge
	df_out <- rbind(df_out, df_tot, df_tot_faogroup)
	
	#we recalculate L/T, V/T, P/K ratios
	#here the same totals are used for species & subgroups
	invisible(lapply(c(1:12,"all"), function(month){
		if(month %in% 1:12){
			df_out[df_out$month == month & df_out$descriptor=="L/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="LAN", "value"] / trp_by_total[trp_by_total$month==month & trp_by_total$descriptor=="TRP","value"],2)
			df_out[df_out$month == month & df_out$descriptor=="V/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="VAL", "value"] / trp_by_total[trp_by_total$month==month & trp_by_total$descriptor=="TRP","value"],2)
		}else{
			df_out[df_out$month == month & df_out$descriptor=="L/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="LAN", "value"] / df_out[df_out$species_id == "all" & df_out$month == month & df_out$descriptor=="TRP", "value"],2)
			df_out[df_out$month == month & df_out$descriptor=="V/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="VAL", "value"] / df_out[df_out$species_id == "all" & df_out$month == month & df_out$descriptor=="TRP", "value"],2)
		}
		df_out[df_out$month == month & df_out$descriptor=="P/K", "value"] <<- round2(df_out[df_out$month == month & df_out$descriptor=="VAL", "value"]/df_out[df_out$month == month  & df_out$descriptor=="LAN", "value"],2)
	}))
	
	#round no decimals for TRP
	df_out[df_out$descriptor == "TRP", "value"] <- round2(df_out[df_out$descriptor == "TRP", "value"])
	
	#we remove the false group
	df_out <- df_out[df_out$group_id_fao != -1,]
	df_out[df_out$group_id_fao == -1, "group_id_fao"] <- NA
	df_out[df_out$group_name_fao == "NONE", "group_name_fao"] <- NA
	
	#renaming all values
	df_out[df_out$species_id == "all", "species_id"] <- NA
	df_out[df_out$species_desc == "all", "species_desc"] <- ""
	df_out[df_out$species_desc == "", "species_desc"] <- NA
	df_out[df_out$month == "all", "month"] <- NA
	df_out[df_out$group_id == "", "group_id"] <- NA
	df_out[df_out$group_name == "", "group_name"] <- NA
	df_out[!is.na(df_out$group_id_subt) & df_out$group_id_subt == "", "group_id_subt"] <- NA
	df_out[!is.na(df_out$group_name_subt) & df_out$group_name_subt == "", "group_name_subt"] <- NA
	df_out[!is.na(df_out$group_id_iccat) & df_out$group_id_iccat == "", "group_id_iccat"] <- NA
	df_out[!is.na(df_out$group_name_iccat) & df_out$group_name_iccat == "", "group_name_iccat"] <- NA
	df_out[!is.na(df_out$group_id_fao) & df_out$group_id_fao == "", "group_id_fao"] <- NA
	df_out[!is.na(df_out$group_name_fao) & df_out$group_name_fao == "", "group_name_fao"] <- NA	
	#rounding
	df_out$value <- round2(df_out$value, 2)
	
	return(df_out)
}


#compute_2nd_raised_landings_by_ICCATGROUP
#@param raw_data a standardized raw data set
#@param landings_1 a standardized landings data set
#@param raised_1 a normalized dataset of 1st raising factors
#@param raised_2 a normalized dataset of 2nd raising factors
#@param zones an association dataset between beaches and zones
#@param species_groups an association dataset between species and species groups
#@returns the landings statistical descriptors in normalized data.frame
#----------------------------------------------------------------------------------------------
compute_2nd_raised_landings_by_ICCATGROUP <- function(raw_data, landings_1, raised_1, raised_2, zones, species_groups){

	#TRIP NUMBERS
	#-----------------------------------------------
	#For TRIP numbers, this is not a simple addition
	#for that, we need to take raw data, get nb of trips by species group
	raw_data_01 <- raise_raw_data(raw_data, raised_1)
	
	raw_data_01 <- raw_data_01[!(raw_data_01$bch_name %in%  c("CACANDEE", "TYPE IV")),]
	
	raw_data_01$act <- NULL
	raw_data_01$enum <- NULL
	raw_data_01_zones <- merge(
		x = raw_data_01,
		y = zones,
		by.x = "bch_name",
		by.y = "beach_name",
		all.x = TRUE,
		all.y = FALSE
	)
	raw_data_01_zones[is.na(raw_data_01_zones $zone), "zone"] <- -99
	raw_data_01_raised_2 <- merge(
		x = raw_data_01_zones,
		y = raised_2,
		by.x = c("zone","month"),
		by.y = c("zone#","month"),
		all.x = TRUE,
		all.y = FALSE
	)
	raw_data_01_sp <- merge(
		x = raw_data_01_raised_2,
		y = species_groups,
		by.x = "species_id",
		by.y = "species_id",
		all.x = TRUE,
		all.y = FALSE
	)
	raw_data_01_sp[is.na(raw_data_01_sp$act),"act"] <- 1
	raw_data_01_sp[is.na(raw_data_01_sp$enum),"enum"] <- 1
	raw_data_01_sp$raised_trp <- raw_data_01_sp$raised_trp* as.numeric(raw_data_01_sp$act) / as.numeric(raw_data_01_sp$enum)
	colnames(raw_data_01_sp)[which(colnames(raw_data_01_sp)== "month.x")] <- "month"
	colnames(raw_data_01_sp)[which(colnames(raw_data_01_sp)== "year.x")] <- "year"
	colnames(raw_data_01_sp)[which(colnames(raw_data_01_sp)=="species_desc.x")] <- "species_desc"
	raw_data_01_sp$"month.y" <- NULL
	raw_data_01_sp$"year.y" <- NULL
	raw_data_01_sp <- raw_data_01_sp[raw_data_01_sp$group_id_iccat != "NULL",]

	#number of trips by month/group
	raw_data_01_trp_iccat <- unique(raw_data_01_sp[,c("landing_id","year","month", "group_id_iccat", "group_name_iccat", "raised_trp")])
	IGNORE_DIMENSION <- rep("",nrow(raw_data_01_trp_iccat))
	trp_by_iccatgroup <- aggregate(
		x = raw_data_01_trp_iccat$raised_trp,
		by = list(
			species_id = IGNORE_DIMENSION,
			species_desc = IGNORE_DIMENSION,
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = raw_data_01_trp_iccat$group_id_iccat,
			group_name_iccat = raw_data_01_trp_iccat$group_name_iccat,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = rep("TRP", nrow(raw_data_01_trp_iccat)),
			year = raw_data_01_trp_iccat$year,
			month = raw_data_01_trp_iccat$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	)
	colnames(trp_by_iccatgroup)[length(colnames(trp_by_iccatgroup))] <- "value"
	
	#number of trips by month/group
	raw_data_01_trp_total <- unique(raw_data_01_sp[,c("landing_id","year","month", "raised_trp")])
	IGNORE_DIMENSION <- rep("",nrow(raw_data_01_trp_total))
	trp_by_total <- aggregate(
		x = raw_data_01_trp_total$raised_trp,
		by = list(
			species_id = rep("all",nrow(raw_data_01_trp_total)),
			species_desc = rep("all",nrow(raw_data_01_trp_total)),
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = rep("TRP", nrow(raw_data_01_trp_total)),
			year = raw_data_01_trp_total$year,
			month = raw_data_01_trp_total$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE))}
	)
	colnames(trp_by_total)[length(colnames(trp_by_total))] <- "value"

	#compute statistics other TRP number, from 1st raised landings
	#---------------------------------------------------------------------------
	df_with_zones <- merge(
		x = landings_1,
		y = zones,
		by.x = "bch_name",
		by.y = "beach_name",
		all.x = TRUE,
		all.y = FALSE
	)
	df_with_zones[is.na(df_with_zones$zone), "zone"] <- -99
	
	df_with_raised_2 <- merge(
		x = df_with_zones,
		y = raised_2,
		by.x = c("zone","month"),
		by.y = c("zone#","month"),
		all.x = TRUE,
		all.y = FALSE
	)
	
	df_with_raised_2[is.na(df_with_raised_2$act),"act"] <- 1
	df_with_raised_2[is.na(df_with_raised_2$enum),"enum"] <- 1
	df_with_raised_2$value <- df_with_raised_2$value* as.numeric(df_with_raised_2$act) / as.numeric(df_with_raised_2$enum)
	colnames(df_with_raised_2)[which(colnames(df_with_raised_2)== "month.x")] <- "month"
	colnames(df_with_raised_2)[which(colnames(df_with_raised_2)== "year.x")] <- "year"
	df_with_raised_2$"month.y" <- NULL
	df_with_raised_2$"year.y" <- NULL
	
	#we remove the trips
	df_with_raised_2 <- df_with_raised_2[df_with_raised_2$descriptor != "TRP",]
	df_with_raised_2 <- df_with_raised_2[!(df_with_raised_2$bch_name %in%  c("CACANDEE", "TYPE IV")),]
	
	#filtering out aggregated values
	without_totals <- !is.na(df_with_raised_2$gear_id) & !is.na(df_with_raised_2$species_id) & !is.na(df_with_raised_2$month)
	df1 <- df_with_raised_2[without_totals,]
	
	#merge with groups
	df2 <- merge(
		x = df1,
		y = species_groups,
		by.x = "species_id",
		by.y = "species_id",
		all.x = TRUE,
		all.y = FALSE
	)
	for(coln in colnames(df2)){
		df2[!is.na(df2[,coln]) & df2[,coln] == "NULL", coln] <- NA
	}
	
	if(any(is.na(df2$group_id_iccat))) df2[is.na(df2$group_id_iccat),]$group_id_iccat <- -1
	if(any(is.na(df2$group_name_iccat)))df2[is.na(df2$group_name_iccat),]$group_name_iccat <- "NONE"	
	df2 <- df2[df2$group_id_iccat > -1,]
	
	#aggregate by species group subt
	IGNORE_DIMENSION <- rep("",nrow(df2))
	df_iccat <- aggregate(
		x = df2$value,
		by = list(
			species_id = IGNORE_DIMENSION,
			species_desc = IGNORE_DIMENSION,
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = df2$group_id_iccat,
			group_name_iccat = df2$group_name_iccat,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = df2$descriptor,
			year = df2$year,
			month = df2$month
		),
		FUN = function(x){round2(sum(x, na.rm=T),2)}
	)
	colnames(df_iccat)[ncol(df_iccat)] <- "value"
		
	#we first compute the aggregate by month of all species we need that for ratios L/T, V/T, P/K
	IGNORE_DIMENSION <- rep("",nrow(df_iccat))
	totals_by_month <- aggregate(
		x = df_iccat$value,
		by = list(
			species_id = rep("all",nrow(df_iccat)),
			species_desc = rep("all",nrow(df_iccat)),
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = IGNORE_DIMENSION,
			group_name_iccat = IGNORE_DIMENSION,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = df_iccat$descriptor,
			year = df_iccat$year,
			month = df_iccat$month
		),
		FUN = function(x){round2(sum(x, na.rm = TRUE),2)}
	)
	colnames(totals_by_month)[ncol(totals_by_month)] <- "value"

	
	#we do rbind to add TRP value
	df_iccat <- rbind(df_iccat, trp_by_iccatgroup)
	totals_by_month <- rbind(totals_by_month, trp_by_total)
	#we add species + species subgroups + grand totals
	df_out <- do.call("rbind", list(df_iccat, totals_by_month))	

	#aggregate on yearly total
	#-------------------------
	IGNORE_DIMENSION <- rep("",nrow(df_out))
	df_tot <- aggregate(
		x = df_out$value,
		by = list(
			#pk = totals_by_month$pk,
			species_id = df_out$species_id,
			species_desc = df_out$species_desc,
			group_id_subt = IGNORE_DIMENSION,
			group_name_subt = IGNORE_DIMENSION,
			group_id = IGNORE_DIMENSION,
			group_name = IGNORE_DIMENSION,
			group_id_iccat = df_out$group_id_iccat,
			group_name_iccat = df_out$group_name_iccat,
			group_id_fao = IGNORE_DIMENSION,
			group_name_fao = IGNORE_DIMENSION,
			descriptor = df_out$descriptor,
			year = df_out$year,
			month = rep("all", nrow(df_out))
		),
		FUN = function(x){round2(sum(x, na.rm=TRUE),2)}
	)
	#df_tot$pk <- NULL
	colnames(df_tot)[ncol(df_tot)] <- "value"	
	df_out <- rbind(df_out, df_tot)
	
	#we recalculate L/T, V/T, P/K ratios
	#here the same totals are used for species & subgroups
	invisible(lapply(c(1:12,"all"), function(month){
		if(month %in% 1:12){
			df_out[df_out$month == month & df_out$descriptor=="L/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="LAN", "value"] / trp_by_total[trp_by_total$month==month & trp_by_total$descriptor=="TRP","value"],2)
			df_out[df_out$month == month & df_out$descriptor=="V/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="VAL", "value"] / trp_by_total[trp_by_total$month==month & trp_by_total$descriptor=="TRP","value"],2)
		}else{
			df_out[df_out$month == month & df_out$descriptor=="L/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="LAN", "value"] / df_out[df_out$species_id == "all" & df_out$month == month & df_out$descriptor=="TRP", "value"],2)
			df_out[df_out$month == month & df_out$descriptor=="V/T", "value"] <<- round2( df_out[df_out$month == month & df_out$descriptor=="VAL", "value"] / df_out[df_out$species_id == "all" & df_out$month == month & df_out$descriptor=="TRP", "value"],2)
		}
		df_out[df_out$month == month & df_out$descriptor=="P/K", "value"] <<- round2(df_out[df_out$month == month & df_out$descriptor=="VAL", "value"]/df_out[df_out$month == month  & df_out$descriptor=="LAN", "value"],2)
	}))
	
	#round no decimals for TRP
	df_out[df_out$descriptor == "TRP", "value"] <- round2(df_out[df_out$descriptor == "TRP", "value"])
	
	#we remove the false group
	df_out <- df_out[df_out$group_id_iccat != -1,]
	df_out[df_out$group_id_iccat == -1, "group_id_iccat"] <- NA
	df_out[df_out$group_name_iccat == "NONE", "group_name_iccat"] <- NA
	
	#renaming all values
	df_out[df_out$species_id == "all", "species_id"] <- NA
	df_out[df_out$species_desc == "all", "species_desc"] <- ""
	df_out[df_out$species_desc == "", "species_desc"] <- NA
	df_out[df_out$month == "all", "month"] <- NA
	df_out[df_out$group_id == "", "group_id"] <- NA
	df_out[df_out$group_name == "", "group_name"] <- NA
	df_out[!is.na(df_out$group_id_subt) & df_out$group_id_subt == "", "group_id_subt"] <- NA
	df_out[!is.na(df_out$group_name_subt) & df_out$group_name_subt == "", "group_name_subt"] <- NA
	df_out[!is.na(df_out$group_id_iccat) & df_out$group_id_iccat == "", "group_id_iccat"] <- NA
	df_out[!is.na(df_out$group_name_iccat) & df_out$group_name_iccat == "", "group_name_iccat"] <- NA
	df_out[!is.na(df_out$group_id_fao) & df_out$group_id_fao == "", "group_id_fao"] <- NA
	df_out[!is.na(df_out$group_name_fao) & df_out$group_name_fao == "", "group_name_fao"] <- NA	
	#rounding
	df_out$value <- round2(df_out$value, 2)
	
	return(df_out)
}
