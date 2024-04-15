package <- jsonlite::read_json('./srv/calipseo-shiny/package.json')
invisible(lapply(package$dependencies, function(pkg){
  from <- 'cran'
  pkg_installer <- remotes::install_version
  if(!is.null(pkg$from)){
    from <- pkg$from
    pkg_installer <- try(eval(parse(text=paste0("remotes::install_",from))))
  }
  if(class(pkg_installer)[1] == "try-error") return(NULL)
  version <- ""
  if(!is.null(pkg$version)) version <- paste0("[",pkg$version,"]")
  cat(sprintf("Install package '%s' %s from '%s'\n", pkg$package, version, from))
  pkg_args <- pkg[!sapply(names(pkg), function(x){x %in% c("from","dependencies")})]
  pkg_deps <- NA
  if(!is.null(pkg$dependencies)) pkg_deps <- pkg$dependencies
  do.call(pkg_installer, c(pkg_args, dependencies = pkg_deps))
}))