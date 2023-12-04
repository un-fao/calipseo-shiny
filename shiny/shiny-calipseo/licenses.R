#analyse licenses
require(magrittr)
require(readr)

source("assets/core/package_utils.R")

package = getAppPackage()
pkgs = sapply(package$dependencies, function(x){x$package})
License = sapply(pkgs, function(x){packageDescription(x)$License})
readr::write_csv(as.data.frame(table(License)), "app_packages_licenses.csv")
gpl_licenses = License[sapply(pkgs, function(x){startsWith(packageDescription(x)$License, "GPL")})] %>% as.data.frame()
gpl_licenses = cbind(Package = row.names(gpl_licenses), License = gpl_licenses)
readr::write_csv(gpl_licenses, "app_packages_under_gpl.csv")
