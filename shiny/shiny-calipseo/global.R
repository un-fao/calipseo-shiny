#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#config
#---------------------------------------------------------------------------------------
#config_file = "D:/Documents/DEV/Bitbucket/fao/fao-calipseo-stats/shinyconfigs/calipseo_shiny_config_SUR.yml"
config_file <- "/etc/shiny-calipseo/config.yml"
if(!nzchar(config_file)) stop("No configuration file at '/etc/shiny-calipseo/config.yml'")
appConfig <- suppressWarnings(yaml::read_yaml(config_file))

#packages
#---------------------------------------------------------------------------------------
#To be installed on server before (not at shiny runtime)
#CRAN packages
list_of_packages <- appConfig$r_packages
invisible(lapply(list_of_packages, function(x) {
  if(!require(x,character.only = TRUE, quietly = TRUE)){
    install.packages(x,repos = "https://cran.rstudio.com/")
    require(x,character.only = TRUE, quietly = TRUE)
  }
}))

#DB connections
#---------------------------------------------------------------------------------------
pool <- pool::dbPool(
  drv = DBI::dbDriver(appConfig$openfismis$dbi$drv),
  dbname = appConfig$openfismis$dbi$dbname,
  host = appConfig$openfismis$dbi$host,
  port = appConfig$openfismis$dbi$port,
  user = appConfig$openfismis$dbi$user,
  password = appConfig$openfismis$dbi$password
)

#global variables / environment
#---------------------------------------------------------------------------------------
CALIPSEO_SHINY_ENV <- new.env()

#utilities
#---------------------------------------------------------------------------------------
#core R script utils
source("assets/core/utils.R")
source("assets/core/module_utils.R")
source("assets/core/data_access_utils.R")
source("assets/core/ui_utils.R")
source("assets/core/vessel_utils.R")

#country R script utils
country_assets <- list.files(path = file.path("./assets/country", appConfig$country_profile$iso3), 
                             pattern = ".R", recursive = TRUE, full.names = TRUE)
for(country_asset in country_assets){ source(country_asset) }

#country profile
#---------------------------------------------------------------------------------------
appConfig <- loadCountryProfile(appConfig, pool)
print(appConfig$country_profile$data)

#local datasets
#---------------------------------------------------------------------------------------
loadLocalCountryDatasets(appConfig)

#modules
#---------------------------------------------------------------------------------------
loadModuleScripts(appConfig)

#main Shiny scripts
#---------------------------------------------------------------------------------------
source("ui.R")
source("server.R")

#onStop
#---------------------------------------------------------------------------------------
onStop(function(){
  cat("Closing DB pool connection\n")
  pool::poolClose(pool)
  cat("Removing CALIPSEO_SHINY_ENV environment\n")
  rm(CALIPSEO_SHINY_ENV)
})
