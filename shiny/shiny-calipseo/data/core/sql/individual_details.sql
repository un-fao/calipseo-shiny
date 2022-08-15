SELECT salu.I18n_DEFAULT as Salutation, ind.FIRST_NAME, ind.MIDDLE_NAME, ind.SUFFIX_NAME, 
ent.NAME, gend.I18n_DEFAULT AS Gender, ind.DATE_OF_BIRTH AS DOB, ind.REG_ENTITY_ID AS individualNumber FROM reg_entities AS ent 
LEFT JOIN reg_entity_individuals AS ind ON ent.ID = ind.REG_ENTITY_ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 
WHERE ent.ENABLED = 1