reportArtisanalFile<-function(data,errors,referentials,path){

type_levels <- c("ERROR", "WARNING", "VALID")


# validation_agg <- validation_results %>%
#   filter(!is.na(column)) %>%
#   group_by(row, column) %>%
#   summarise(
#     type = factor(type, levels = type_levels, ordered = TRUE) |> min() |> as.character(),
#     message = paste0(sprintf("[%s] %s: %s", type, category, message), collapse = "\n"),
#     .groups = "drop"
#   )
# 

# if (!highlight_valid) {
#   validation_agg <- validation_agg %>% filter(type != "VALID")
# }

summary_table <- errors %>%
  filter(!is.na(column)) %>%
  mutate(type = factor(type, levels = type_levels, ordered = TRUE)) %>%
  group_by(row, column) %>%
  slice_min(order_by = type, n = 1) %>%  # prendre type le plus sévère
  ungroup() %>%
  group_by(column, type) %>%
  summarise(n = n_distinct(row), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
  mutate(across(c(VALID, WARNING, ERROR), ~replace_na(.x, 0))) %>%
  select(column, VALID, WARNING, ERROR) %>%
  mutate(TOTAL = VALID + WARNING + ERROR)%>%
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

style_error <- createStyle(bgFill = "#F08080")     # rouge clair
style_warning <- createStyle(bgFill = "#FFD700")   # jaune
style_valid <- createStyle(bgFill = "#90EE90")     # vert clair

writeData(wb, "Data", data)

# # Appliquer les couleurs et commentaires
# for (i in seq_len(nrow(validation_agg))) {
#   print(i)
#   row_num <- validation_agg$row[i] + 1
#   col_num <- which(names(data) == validation_agg$column[i])
#   style <- switch(validation_agg$type[i],
#                   "ERROR" = style_error,
#                   "WARNING" = style_warning,
#                   "VALID" = style_valid)
#   
#   addStyle(wb, "Validated data", style, rows = row_num, cols = col_num, stack = TRUE)
#   
#   if (validation_agg$type[i] != "VALID") {
#     com <- createComment(validation_agg$message[i], author = "Validation")
#     writeComment(wb, "Validated data", col = col_num, row = row_num, comment = com)
#   }
# }

# Errors only
addWorksheet(wb, "Error report")
error_report <- errors %>% filter(type != "VALID")
writeData(wb, "Error report", error_report)

# Summary
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_table)
conditionalFormatting(wb, "Summary", cols = 2, rows = 2:(nrow(summary_table)+1),
                      rule = "B2>0",summary_table[1,5], style = style_warning, type = "expression")
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
