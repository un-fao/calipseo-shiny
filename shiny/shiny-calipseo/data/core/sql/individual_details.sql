SELECT salu.I18n_DEFAULT as Salutation, ind.FIRST_NAME, ind.MIDDLE_NAME, ind.SUFFIX_NAME, 
ent.NAME, gend.I18n_DEFAULT AS Gender, ind.DATE_OF_BIRTH AS DOB, ind.REG_ENTITY_ID AS individualNumber,
fr.CODE AS FSH_CODE,fr.I18n_DEFAULT AS FSH_ROLE FROM reg_entity_individuals AS ind 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 
LEFT JOIN reg_entity_individual_role_fishery AS entfr ON ind.ID = entfr.REG_ENTITY_ID 
LEFT JOIN cl_fish_role_in_fishery AS fr ON entfr.CL_FISH_ROLE_IN_FISHERY_ID = fr.ID 
WHERE ent.ENABLED = 1 AND ind.ID IS NOT NULL