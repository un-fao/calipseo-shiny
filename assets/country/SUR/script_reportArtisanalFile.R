reportArtisanalFile<-function(data,errors,referentials,path,add_cell_comments=F){

type_levels <- c("ERROR", "WARNING", "VALID")

summary_table <- errors %>%
  filter(!is.na(column)) %>%
  mutate(type = factor(type, levels = type_levels, ordered = TRUE)) %>%
  group_by(row, column) %>%
  slice_min(order_by = type, n = 1) %>%  
  ungroup() %>%
  group_by(column, type) %>%
  summarise(n = n_distinct(row), .groups = "drop") %>%
  complete(column,type = type_levels,fill = list(n = 0)) %>%
  tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
  mutate(across(any_of(c("VALID", "WARNING", "ERROR")), ~replace_na(.x, 0))) %>%   
  select(column, any_of(c("VALID", "WARNING", "ERROR"))) %>%                    
  mutate(TOTAL = rowSums(across(any_of(c("VALID", "WARNING", "ERROR"))))) %>%     
  arrange(column)

summary_ref <- NULL
if (!is.null(referentials) && nrow(referentials) > 0) {
  summary_ref <- referentials %>%
    group_by(table, type) %>%
    summarise(n = n_distinct(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
    arrange(table)
}

wb <- createWorkbook()
addWorksheet(wb, "Data")

style_error   <- createStyle(fgFill = "#F08080")
style_warning <- createStyle(fgFill = "#FFD700")
style_valid   <- createStyle(fgFill = "#90EE90")   

if (add_cell_comments) {
  writeData(wb, "Data", data)
  
  validation_agg <- errors %>%
    filter(!is.na(column)) %>%
    group_by(row, column) %>%
    summarise(
      type = factor(type, levels = type_levels, ordered = TRUE) %>% min() %>% as.character(),
      message = paste0(sprintf("[%s] %s: %s", type, category, message), collapse = "\n"),
      .groups = "drop"
    ) %>%
    filter(type != "VALID")
  
  if (nrow(validation_agg) > 0) {
    
    validation_agg <- validation_agg %>%
      mutate(col_num = sapply(column, function(col) {
        idx <- which(names(data) == col)
        if (length(idx) == 1) idx else NA_integer_
      })) %>%
      filter(!is.na(col_num))
    
    for (t in c("ERROR", "WARNING")) {
      rows <- validation_agg$row[validation_agg$type == t] + 1
      cols <- validation_agg$col_num[validation_agg$type == t]
      if (length(rows) > 0) {
        addStyle(wb, "Data",
                 style = if (t == "ERROR") style_error else style_warning,
                 rows = rows, cols = cols, gridExpand = FALSE, stack = TRUE)
      }
    }
    
    for (i in seq_len(nrow(validation_agg))) {
      com <- createComment(validation_agg$message[i], author = "Validation")
      writeComment(wb, "Data", col = validation_agg$col_num[i], row = validation_agg$row[i] + 1, comment = com)
    }
  }
} else {
  writeData(wb, "Data", data)
}

addWorksheet(wb, "Error report")
error_report <- errors %>% filter(type != "VALID")
writeData(wb, "Error report", error_report)

style_error   <- createStyle(bgFill = "#F08080")
style_warning <- createStyle(bgFill = "#FFD700")
style_valid   <- createStyle(bgFill = "#90EE90") 

# Summary
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_table)
conditionalFormatting(wb, "Summary", cols = 2, rows = 2:(nrow(summary_table)+1),
                      rule = ">=0", style = style_warning, type = "expression")
conditionalFormatting(wb, "Summary", cols = 2, rows = 2:(nrow(summary_table)+1),
                      rule = "=B2=E2", style = style_valid, type = "expression")
conditionalFormatting(wb, "Summary", cols = 3, rows = 2:(nrow(summary_table)+1),
                      rule = ">=1", style = style_warning, type = "expression")
conditionalFormatting(wb, "Summary", cols = 4, rows = 2:(nrow(summary_table)+1),
                      rule = ">=1", style = style_error, type = "expression")

# Referential issues
if (is.null(referentials) || nrow(referentials) == 0) {
  addWorksheet(wb, "Referentials")
  writeData(wb, "Referentials", "No issues found in referential mappings.")
} else {
  addWorksheet(wb, "Referentials")
  writeData(wb, "Referentials", referentials)
  
  addWorksheet(wb, "Referentials report")
  writeData(wb, "Referentials report", summary_ref)
  conditionalFormatting(wb, "Referentials report", cols = 2:3, rows = 2:(nrow(summary_ref)+1),
                        rule = ">=1", style = style_error, type = "expression")
  conditionalFormatting(wb, "Referentials report", cols = 2:3, rows = 2:(nrow(summary_ref)+1),
                        rule = "==0", style = style_valid, type = "expression")
}

saveWorkbook(wb, path, overwrite = TRUE)

}
