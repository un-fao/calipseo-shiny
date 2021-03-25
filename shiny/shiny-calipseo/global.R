#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#config
#---------------------------------------------------------------------------------------
#config_file = "D:/Documents/DEV/Bitbucket/fao/fao-calipseo-stats/shinyconfigs/calipseo_shiny_config_SUR.yml"
config_file <- "/etc/shiny-calipseo/config.yml"
if(!nzchar(config_file)) stop("No configuration file at '/etc/shiny-calipseo/config.yml'")
appConfig <- yaml::read_yaml(config_file)

#packages
#---------------------------------------------------------------------------------------
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


#utilities
#---------------------------------------------------------------------------------------
source("assets/utils.R")
source("assets/data_access_utils.R")
source("assets/ui_utils.R")

#modules
#---------------------------------------------------------------------------------------
#HOME
source("modules/home/home_server.R")
source("modules/home/home_ui.R")
#VESSEL
source("modules/vessel/vessel_server.R")
source("modules/vessel/vessel_ui.R")


#main Shiny scripts
#---------------------------------------------------------------------------------------
source("ui.R")
source("server.R")

#onStop
#---------------------------------------------------------------------------------------
onStop(function(){
  cat("Closing DB pool connection\n")
  pool::poolClose(pool)
})
