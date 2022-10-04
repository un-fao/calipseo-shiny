#options
#---------------------------------------------------------------------------------------
options(
  encoding = "UTF-8",
  stringsAsFactors = FALSE, 
  dplyr.summarise.inform = FALSE
)


#packages
#---------------------------------------------------------------------------------------
source("assets/core/package_utils.R")
loadAppPackages()

#config
#---------------------------------------------------------------------------------------
#default config_file path for DEPLOYMENT
config_file <- "/etc/shiny-server/config.yml"

#local configuration
#If you are an R developer, you need to create a .REnviron file (no file extension) in /shiny-calipseo dir
#The file should include the local path for your shiny config file in that way:
#CALIPSEO_SHINY_CONFIG=<your config path>
local_config_file <- Sys.getenv("CALIPSEO_SHINY_CONFIG")
if(nzchar(local_config_file)) config_file <- local_config_file
appConfig <- suppressWarnings(yaml::read_yaml(config_file))

#language (in case not part of configuration)
if(is.null(appConfig$language)) appConfig$language <- "en"


#DB connections
#---------------------------------------------------------------------------------------
pool <- pool::dbPool(
  drv = DBI::dbDriver(appConfig$openfismis$dbi$drv),
  dbname = appConfig$openfismis$dbi$dbname,
  host = appConfig$openfismis$dbi$host,
  port = appConfig$openfismis$dbi$port,
  user = appConfig$openfismis$dbi$user,
  password = appConfig$openfismis$dbi$password,
  bigint = "numeric" #required otherwise RMariaDB coerces integers as integer64
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
source("assets/core/js_utils.R")
source("assets/core/vessel_utils.R")

#country R script utils
country_assets <- list.files(path = file.path("./assets/country", appConfig$country_profile$iso3), 
                             pattern = ".R", recursive = TRUE, full.names = TRUE)
for(country_asset in country_assets){ source(country_asset) }

#country profile
#---------------------------------------------------------------------------------------
appConfig <- loadCountryProfile(appConfig, pool)
print(appConfig$country_profile$data)

#country parameters
#---------------------------------------------------------------------------------------
COUNTRY_PARAMS <- accessCountryParam(pool)
HAS_REGMANGT <- ifelse(COUNTRY_PARAMS[COUNTRY_PARAMS$CODE=='REGMANGT',]$BOOLEAN == 1, TRUE, FALSE)
PREF_UNIT_WEIGHT<-accessCountryPrefUnitWeight(pool)[1,1]
PREF_CURRENCY<-accessCountryPrefCurrency(pool)[1,1]

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