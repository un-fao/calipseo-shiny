#raise_raw_data
#@param raw_data
#@param raised_1
#@returns the raw_data raised
raise_raw_data <- function(raw_data, raised_1){
  #convert these date/time from character to datetime objects (POSIXct)
  #This assumes that date/time are stored and read in UTC
  raw_data$dep_datetime <- as.POSIXct(raw_data$dep_datetime, tz = "UTC"); attr(raw_data$dep_datetime,"tzone") <- appConfig$country_profile$timezone
  raw_data$ret_datetime <- as.POSIXct(raw_data$ret_datetime, tz = "UTC"); attr(raw_data$ret_datetime,"tzone") <- appConfig$country_profile$timezone
  #year/month
  raw_data$year <- as.integer(format(raw_data$ret_datetime, "%Y"))
  raw_data$month <- as.integer(format(raw_data$ret_datetime,"%m"))
  
  #try to join/merge raw_data with raised_1 table
  raw_data_01 <- merge(
    x = raw_data, #table 1
    y = raised_1, #table 2
    by.x = c("bch_id", "month"),  #character name of column
    by.y = c("bch_id", "month") , #character name of column
    all.x = TRUE, #TRUE/FALSE
    all.y = FALSE  #TRUE/FALSE
  )
  
  #raised weight
  UNIT_LBS_TO_KGS <- 0.453592
  raw_data_01$raised_lan_lbs <- as.numeric(raw_data_01$quantity) * as.numeric(raw_data_01$act) / as.numeric(raw_data_01$enum)
  raw_data_01$raised_lan_kgs <- raw_data_01$raised_lan_lbs * UNIT_LBS_TO_KGS
  #raised value
  raw_data_01$raised_val <- raw_data_01$raised_lan_lbs * (raw_data_01$value / raw_data_01$quantity)
  #trip numbers
  raw_data_01$raised_trp <- as.numeric(raw_data_01$act) / as.numeric(raw_data_01$enum)
  
  return(raw_data_01)
} 


#generateReport
generateReport <- function(session, indicator, year, data, con){
  
  report_msg <- sprintf("Generate %s for %s", indicator$label, year)
  cat(paste0(report_msg,"\n"))
  progress <- shiny::Progress$new(session, min = 0, max = 100)
  on.exit(progress$close())
  
  progress$set(value = 10, message = report_msg, detail = "Prepare R script...")
  cat(sprintf("Load R 'report' script '%s'\n", indicator$report_with$script))
  source(indicator$report_with$script)
  cat(sprintf("Report indicator '%s'\n", indicator$value))
  args <- names(formals(indicator$report_with$fun))
  fun_statement <- paste0("data_report <- ", indicator$report_with$fun, 
                          "(", paste0(sapply(args, function(arg){paste0(arg, " = ", indicator$report_with$fun_args[[arg]])}), collapse=","), ")"
  )
  cat(sprintf("Function statement: %s\n", fun_statement))
  progress$set(value = 30, message = report_msg, detail = "Prepare report...")
  eval(parse(text = fun_statement))
  if(is.null(data_report)){
    cat(sprintf("Error while building report for indicator '%s'\n", indicator$value))
  }else{
    cat(sprintf("Successfully built report for indicator '%s'\n", indicator$value))
    progress$set(value = 80, message = report_msg, detail = "Export report...")
    write_generic_report(data_report, con, 
                         firstActiveRow = indicator$report_with$export_options$firstActiveRow,
                         firstActiveCol = indicator$report_with$export_options$firstActiveCol)
    cat(sprintf("Successfully downloaded report for indicator '%s'\n", indicator$value))
    progress$set(value = 100, message = report_msg, detail = "Successfully downloaded report!")
  } 
}