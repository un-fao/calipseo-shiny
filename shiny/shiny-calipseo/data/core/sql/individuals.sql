SELECT salu.I18n_DEFAULT as Salutation, ind.FIRST_NAME, ind.MIDDLE_NAME, ind.SUFFIX_NAME,
ent.NAME AS LAST_NAME, gend.I18n_DEFAULT AS Gender, ind.DATE_OF_BIRTH AS DOB,edulevel.I18n_DEFAULT AS EDULEVEL, ind.REG_ENTITY_ID AS individualNumber FROM reg_entities AS ent 
LEFT JOIN reg_entity_individuals AS ind ON ent.ID = ind.REG_ENTITY_ID 
LEFT JOIN reg_entity_individual_education AS edu ON ent.ID = edu.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 