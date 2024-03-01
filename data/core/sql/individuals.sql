SELECT ind.REG_ENTITY_ID as ID, ind.FISHER_ID as FisherID, salu.I18n_DEFAULT as Salutations, ind.FIRST_NAME as First_name, ind.MIDDLE_NAME as Middle_name, ind.SUFFIX_NAME as Suffix_name,
ent.NAME AS Last_name, gend.CODE as GenderCode, gend.I18n_DEFAULT AS Gender 
 FROM reg_entity_individuals AS ind 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN reg_entity_individual_education AS edu ON ent.ID = edu.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 
WHERE ent.ENABLED = 1