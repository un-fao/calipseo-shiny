artfish_estimates_test_report <- function(con, data, metadata, file){
  
  requireNamespace("magrittr")
  requireNamespace("dplyr")
  requireNamespace("openxlsx")
  requireNamespace("stringr")
  
  rep = data
  rep_names = colnames(rep)
  
  #enrich with codelists
  #-> enrich with fishing units
  ref_fishing_unit = accessRefFishingUnits(con = con) %>% dplyr::rename(fishing_unit = ID)
  rep = rep %>% 
    dplyr::left_join(ref_fishing_unit) %>% #join with fishing_unit ref to inherit names
    dplyr::mutate(fishing_unit = NAME) %>% #fill the fishing_unit with the fishing_unit names
    dplyr::select(rep_names) #keep only native column names, and get rid of other other fishing_unit joined columns
    
  #-> enrich with species
  ref_species = accessRefSpecies(con = con) %>% dplyr::rename(species = ID)
  rep = rep %>% 
    dplyr::left_join(ref_species) %>% #join with species ref to inherit names
    dplyr::mutate(species = NAME) %>% #fill the species with the species names
    dplyr::select(rep_names) #keep only native column names, and get rid of other other species joined columns
   
  #fdi terms?
  fdi_terms = artfishr::get_fdi_terms()
  fdi_terms = fdi_terms[match(colnames(rep), fdi_terms$code),]
  
  #prepare output
  outdata = list(
    Data = rep,
    Structure = fdi_terms,
    Metadata = if(!is.null(metadata)) metadata else data.frame(Warning = "No metadata information for this report")
  )
  #Excel export
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  openxlsx::addWorksheet(wb, "Structure")
  openxlsx::addWorksheet(wb, "Metadata")
  
  openxlsx::writeData(wb, "Data", outdata$Data)
  bs = createStyle(fontSize = 12, fontColour = "blue", textDecoration = c("BOLD"))
  for(i in 1:length(fdi_terms$code)){
    com = openxlsx::createComment(comment = as.character(fdi_terms[i,"label"]), style = bs)
    openxlsx::writeComment(wb, "Data", col = i, row = 1, comment = com)
  }
  openxlsx::writeData(wb, "Structure", outdata$Structure)
  openxlsx::writeData(wb, "Metadata", outdata$Metadata)

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}