#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE)

#config
#---------------------------------------------------------------------------------------
#default config_file path for DEPLOYMENT
config_file <- "/etc/shiny-calipseo/config.yml"

#local configuration
#If you are an R developer, you need to create a .REnviron file (no file extension) in /shiny-calipseo dir
#The file should include the local path for your shiny config file in that way:
#CALIPSEO_SHINY_CONFIG=<your config path>
local_config_file <- Sys.getenv("CALIPSEO_SHINY_CONFIG")
if(nzchar(local_config_file)) config_file <- local_config_file
if(!file.exists(config_file)) stop("No configuration file at '/etc/shiny-calipseo/config.yml'")
appConfig <- suppressWarnings(yaml::read_yaml(config_file))

#language (in case not part of configuration)
if(is.null(appConfig$language)) appConfig$language <- "en"

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

#remote datasets
#---------------------------------------------------------------------------------------
loadRemoteReferenceDataset("asfis_enrished","https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_GROUPS.csv")

#language/i18n
#---------------------------------------------------------------------------------------
#we extend appConfig with i18n iterms
appConfig$i18n <- getModuleI18nTerms()

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
