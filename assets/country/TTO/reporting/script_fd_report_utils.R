rowVar <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}

colMax <- function(X, ...) apply(X, 2, max, ...)

#write_generic_report
#@param out_report_list a list of data.frame ready for writing to report Excel file
#@param filename the target filename
#@param firstActiveRow for pane freezing in Excel
#@param firstActiveCol for pane freezing in Excel
#----------------------------------------------------------------------------------------------
write_generic_report <- function(out_report_list, filename, firstActiveRow, firstActiveCol){

	wb <- openxlsx::createWorkbook()
	openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
	style_bold <- createStyle(textDecoration = "Bold")

	if(is.data.frame(out_report_list)) out_report_list <- list(Report = out_report_list)
	tabs <- names(out_report_list)
	for(tab in tabs){
		tab_data <- out_report_list[[tab]]
		
		#headers?
		headers <- attr(tab_data, "headers")
		attr(tab_data, "headers") <- NULL
		#footers
		footers <- attr(tab_data, "footers")
		attr(tab_data, "footers") <- NULL
		
		#add worksheet
		openxlsx::addWorksheet(wb, tab, gridLines = TRUE)
		#write headers
		rowIdx <- 1
		if(length(headers)>0){
			for(header in headers){
			  openxlsx::writeData(wb, tab, header, startCol = 1, startRow = rowIdx)
				rowIdx <- rowIdx+1
			}
		}
		openxlsx::addStyle(wb, tab, style = style_bold, rows = 1:(rowIdx-1), cols = 1:ncol(tab_data), gridExpand = TRUE)
		#write data table header
		openxlsx::writeData(wb, tab, t(data.frame(colnames(tab_data))), startCol = 1, startRow = rowIdx, colNames = FALSE)
		openxlsx::addStyle(wb, tab, style = style_bold, rows = rowIdx, cols = 1:ncol(tab_data), gridExpand = TRUE)
		rowIdx <- rowIdx+1
		#write data table
		rowIdx <- rowIdx+1 #add empty line
		openxlsx::writeData(wb, tab, tab_data, startCol = 1, startRow = rowIdx, colNames = FALSE)
		rowIdx <- rowIdx + nrow(tab_data) + 1 #add 1 for empty line
		#write footers
		if(length(footers)>0){
			for(footer in footers){
			  openxlsx::writeData(wb, tab, footer, startCol = 1, startRow = rowIdx)
				rowIdx <- rowIdx+1
			}
		}
		
		#column widths
		openxlsx::setColWidths(wb, tab, cols = 1:which(colnames(tab_data)=="DESCRIPTOR"), widths = 20)
		#freezing
		openxlsx::freezePane(wb, tab, firstActiveRow = firstActiveRow, firstActiveCol = firstActiveCol)
		
	}
	openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}
