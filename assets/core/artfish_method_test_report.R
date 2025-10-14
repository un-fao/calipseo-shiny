artfish_estimates_test_report <- function(con, data, metadata, file){
  
  requireNamespace("magrittr")
  requireNamespace("dplyr")
  requireNamespace("writexl")
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
   
  #example of formatting to feed an excel 
  colnames(rep) = stringr::str_to_title(gsub("_", " ", colnames(rep)))
  outdata = list(
    Data = rep,
    Metadata = if(!is.null(metadata)) metadata else data.frame(Warning = "No metadata information for this report")
  )
  #Excel export
  writexl::write_xlsx(x = outdata, path = file)
}