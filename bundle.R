#This scripts prepare the calipseo-shiny code to be deployed on Shiny servers
#It creates a shiny-calipseo.zip out of the source code
#The naming "shiny-calipseo" is conserved for legacy
#-------------------------------------------------------------------------------
wd = getwd()
unlink("../deployment/shiny-calipseo.zip", recursive = TRUE, force = TRUE)
dir.create("../deployment/shiny-calipseo")
for(file in list.files()){
  file.copy(
    from = file, 
    to = file.path("../deployment/shiny-calipseo"), 
    recursive = TRUE, 
    overwrite = TRUE
  )
}
setwd("../deployment")
zip::zip(
  "shiny-calipseo.zip", 
  list.files("shiny-calipseo", full.names = T, recursive = T)
)
unlink("shiny-calipseo", recursive = TRUE, force = TRUE)
setwd(wd)

#prepare local data for a specific country (including definition of statistical indicators)
#-------------------------------------------------------------------------------
COUNTRY = "<ISO3>"
country_data_dir = "../deployment/calipseo-data"
unlink(paste0(country_data_dir, "-", COUNTRY,".zip"), recursive = TRUE, force = TRUE)
dir.create(country_data_dir)
setwd(sprintf("../calipseo-data/country/%s", COUNTRY))
for(file in list.files()){
  file.copy(from = file, to = file.path("../..", country_data_dir), overwrite = T, recursive = T)
}
setwd(wd)
setwd("../deployment")
zip::zip(
  paste0(country_data_dir, "-", COUNTRY,".zip"), 
  list.files(basename(country_data_dir), full.names = T, recursive = T)
)
unlink(country_data_dir, recursive = TRUE, force = TRUE)
setwd(wd)