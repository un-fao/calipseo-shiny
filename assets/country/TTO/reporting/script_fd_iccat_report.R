#ICCAT Reporting functions
#Author: E.Blondel

#functions
#------------------------------------------------------------------------------------------------------------------

#fill_iccat_report_statistical_correspondent
fill_iccat_report_statistical_correspondent <- function(template, 
                                                        fullname = "", email = "", phonenumber = "", 
                                                        institution = "", department = "", address = "", country = ""){
  sheet1 <- getSheets(template)[[1]]
  setCellValue(getCells(getRows(sheet1, 5), colIndex = 3)[[1]], value = fullname)
  setCellValue(getCells(getRows(sheet1, 6), colIndex = 3)[[1]], value = email)
  setCellValue(getCells(getRows(sheet1, 6), colIndex = 7)[[1]], value = phonenumber)
  setCellValue(getCells(getRows(sheet1, 7), colIndex = 3)[[1]], value = institution)
  setCellValue(getCells(getRows(sheet1, 8), colIndex = 3)[[1]], value = department)
  setCellValue(getCells(getRows(sheet1, 9), colIndex = 3)[[1]], value = address)
  setCellValue(getCells(getRows(sheet1, 9), colIndex = 7)[[1]], value = country)
}

#fill_iccat_report_dataset_characteristics
fill_iccat_report_dataset_characteristics <- function(template,
                                                      reporting_flag = "", 
                                                      from = NULL, to = NULL,
                                                      report_version,
                                                      report_type,
                                                      report_coverage,
                                                      notes = ""
){
  sheet1 <- getSheets(template)[[1]]
  setCellValue(getCells(getRows(sheet1, 12), colIndex = 3)[[1]], value = reporting_flag)
  setCellValue(getCells(getRows(sheet1, 13), colIndex = 3)[[1]], value = from)
  setCellValue(getCells(getRows(sheet1, 13), colIndex = 5)[[1]], value = to)
  
  allowed_report_versions <- c("Final", "Preliminary")
  if(!(report_version %in% allowed_report_versions)){
    stop(sprintf("The report version should be one value among [%s]", paste(allowed_report_versions, collapse=",")))
  }
  setCellValue(getCells(getRows(sheet1, 17), colIndex = 3)[[1]], value = report_version)
  
  allowed_report_types <- c("New", "Revision")
  if(!(report_type %in% allowed_report_types)){
    stop(sprintf("The report type should be one value among [%s]", paste(allowed_report_types, collapse=","))) 
  }
  allowed_report_coverages <- c("FULL", "PARTIAL")
  if(!(report_coverage %in% allowed_report_coverages)){
    stop(sprintf("The report coverage should be one value among [%s]", paste(allowed_report_coverages, collapse=","))) 
  }
  setCellValue(getCells(getRows(sheet1, 18), colIndex = 3)[[1]], value = paste0(report_type, " (", report_coverage,")"))
  
  setCellValue(getCells(getRows(sheet1, 12), colIndex = 11)[[1]], value = notes)
}

#fill_iccat_report_dataset
fill_iccat_report_dataset <- function(template, sheetIdx = 1, data, startRow = 1, startColumn = 1){
  sheet <- getSheets(template)[[sheetIdx]]
  xlsx::addDataFrame(
    data, sheet, col.names = FALSE, row.names = FALSE,
    startRow = startRow, startColumn = startColumn
  )
}

#------------------------------------------------------------------------------------------------------------------
#report_iccat_task_1
report_iccat_task_1 <- function(dataset, report_version = "Preliminary", report_type = "New", report_coverage = "PARTIAL"){
  template <- "forms/ST02-T1NC.xlsx"
  template_copy <- "ST02-T1NC_copy.xlsx"
  wb <- xlsx::loadWorkbook(template)
  forked <- file.copy(from = template, to = template_copy)
  if(!forked){
    stop(sprintf("Error while creating a copy of ICCAT template '%s'", template))
  }
  
  #statistical correspondent
  fill_iccat_report_statistical_correspondent(
    template = wb, 
    fullname = "Louanna Martin", email = "lmartin@fp.gov.tt", phonenumber = "+ (868) 634 4504; + (868) 634 4505", 
    institution = "Ministry of Agriculture, Land and Fisheries", department = "Fisheries Division", 
    address = "CFTDI Compound - Western Main Road, Chaguaramas", country = "Trinidad and Tobago"
  )
  
  #dataset characteristics
  fill_iccat_report_dataset_characteristics(
    template = wb,
    reporting_flag = "Trinidad and Tobago", 
    from = min(dataset$YearC), to = max(dataset$YearC),
    report_version = report_version, 
    report_type = report_type, 
    report_coverage = report_coverage,
    notes = ""
  )
  
  #data
  fill_iccat_report_dataset(
    template = wb,
    sheetIdx = 1,
    data = dataset,
    startRow = 26,
    startColumn = 1
  )
  
  #save workbook
  outfilename <- file.path("out", sprintf("%s_ST02-T1NC_TTO.xlsx", format(Sys.time(), "%Y%m%dT%H%M%S")))
  xlsx::saveWorkbook(wb, outfilename)
  unlink(template_copy)
  return(outfilename)
}
