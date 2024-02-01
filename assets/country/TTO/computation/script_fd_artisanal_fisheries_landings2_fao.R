##
# Indicator for Artisanal fisheries for FAO reporting
# * Script name:
#     script_fd_artisanal_fisheries_landings2_fao.R
# * Description:
#	  Computes artisanal fisheries 2d raised landings indicator, by species, by year, for FAO
# * History:
# ------------- --------------- -----------------------
# Date			 Author			 Comment
# ------------- --------------- -----------------------
# 2021-02-11	eblondel    Creation.
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

#compute_2nd_raised_landings_by_FAO
#@param landings_2 a normalized dataset of 2nd raising factors
#@param ref_species
#@param ref_
#@returns the landings statistical descriptors in normalized data.frame
#----------------------------------------------------------------------------------------------
compute_2nd_raised_landings_FAO <- function(landings_2, ref_species){
  
  landings_2 <- as.data.frame(landings_2)
  ref_species <- as.data.frame(ref_species)
  
  data <- landings_2[landings_2$descriptor == "LAN" & !is.null(landings_2$species_id),]
  data <- merge(
    data,
    ref_species,
    by.x = "species_id",
    by.y = "ID",
    all.x = TRUE,
    all.y = TRUE
  )
  out <- aggregate(
    data$value,
    by = list(
      ASFIS_CODE = data$ASFIS_CODE,
      YEAR = data$year
    ),
    sum
  )
  colnames(out)[ncol(out)] <- "QUANTITY"
  return(out)
}