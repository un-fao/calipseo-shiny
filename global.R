#options
#---------------------------------------------------------------------------------------
options(
  encoding = "UTF-8",
  stringsAsFactors = FALSE, 
  dplyr.summarise.inform = FALSE
)

#environment
#--------------------------------------------------------------------------------------
try(dotenv::load_dot_env(file = ".REnviron"), silent = TRUE)

#packages
#---------------------------------------------------------------------------------------
source("assets/core/package_utils.R")
#loadAppPackages() #not scanned by renv
require(dotenv)
require(htmltools)
require(shiny)
require(shinyWidgets)
require(shinycssloaders)
require(shinyjs)
require(shinyFiles)
require(openssl)
require(DBI)
require(RMariaDB)
require(pool)
require(yaml)
require(mime)
require(jsonlite)
require(readr)
require(readxl)
require(writexl)
require(openxlsx)
require(units)
require(measurements)
require(sf)
require(sp)
require(plyr)
require(dplyr)
require(tidyr)
require(reshape2)
require(uuid)
require(RColorBrewer)
require(data.table)
require(DT)
require(plotly)
require(leaflet)
require(leaflet.minicharts)
require(lubridate)
require(sortable)
require(data.tree)
require(DiagrammeR)
require(vrule)
require(artfishr)

#config
#---------------------------------------------------------------------------------------
#default config_file path for DEPLOYMENT
config_file <- "/etc/calipseo-shiny/config.yml"

#local configuration
#If you are an R developer, you need to create a .REnviron file (no file extension) in /shiny-calipseo dir
#The file should include the local path for your shiny config file in that way:
#CALIPSEO_SHINY_CONFIG=<your config path>

local <- FALSE
local_config_file <- Sys.getenv("CALIPSEO_SHINY_CONFIG")
if(nzchar(local_config_file)){
  config_file <- local_config_file
  local <- TRUE
}
appConfig <- suppressWarnings(yaml::read_yaml(config_file))
appConfig$local <- local

#debug
if(is.null(appConfig$debug)) appConfig$debug <- FALSE

#language (in case not part of configuration)
if(is.null(appConfig$language)) appConfig$language <- "en"

#shiny-storage check
default_store_dir <- "/srv/calipseo-shiny-store"
#shiny-storage in dev (assume Windows OS for now)
if(Sys.info()[["sysname"]] == "Windows") default_store_dir <- "out"
if(is.null(appConfig$store)) appConfig$store <- default_store_dir
if(!dir.exists(appConfig$store) && Sys.info()[["sysname"]] != "Windows") dir.create(appConfig$store)

#DB connections
#---------------------------------------------------------------------------------------
require(RMariaDB)
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
core_assets <- list.files("assets/core", pattern = ".R", full.names = T)
for(core_asset in core_assets) source(core_asset)

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
PREF_UNIT_WEIGHT<-accessCountryPrefUnitWeight(pool)
if(PREF_UNIT_WEIGHT$CODE=="lb") PREF_UNIT_WEIGHT$CODE <- "lbs" #patch to make sure unit conversion
PREF_CURRENCY<-accessCountryPrefCurrency(pool)[1,1]

#local datasets
#---------------------------------------------------------------------------------------
loadLocalCountryDatasets(appConfig)

#remote datasets
#---------------------------------------------------------------------------------------
loadRemoteReferenceDataset("cl_asfis_species","https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_asfis_species.csv")
loadRemoteReferenceDataset("cl_isscaap_group","https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscaap_group.csv")

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