#@eblondel 2026-04-17
#migration script from legacy JSON to CSV files
languages = c("es","fr","en","ar")

#iterate over available languages
for(language in languages){
  
  #read JSON legacy i18n files
  i18n_files = list.files(pattern = paste0("_", language), path = "modules", recursive = T, full.names = T)
  
  #reconcile all i18n terms of one language into a single CSV files
  i18n_df = do.call("rbind", lapply(i18n_files, function(x){
    json = jsonlite::read_json(x)
    df = data.frame(
      key = names(json),
      lang = as.character(json)
    )
    names(df)[2] = language
    return(df)
  }))
  i18n_df = i18n_df[order(i18n_df$key),]
  readr::write_csv(i18n_df, file.path("i18n/translations", paste0("translation_", language, ".csv")))
  unlink(i18n_files)
}

