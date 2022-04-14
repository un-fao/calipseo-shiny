#js_generic_handler
js_generic_handler <- function(filename, ...){
  jscode <- paste(suppressWarnings(readLines(filename)), collapse="\n")
  jscode <- JS(sprintf(jscode, ...))
  return(jscode)
}

#js_select2_filter_provider (for enabling Select2.js in DT filter dropdown)
js_select2_filter_provider <- function(id){
 js_generic_handler(file.path(getwd(), "assets/core/js/js_select2_filter_provider.js"), id)
}