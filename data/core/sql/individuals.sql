SELECT salu.I18n_DEFAULT as Salutation, ind.FIRST_NAME, ind.MIDDLE_NAME, ind.SUFFIX_NAME,
ent.NAME AS LAST_NAME, gend.I18n_DEFAULT AS Gender, ind.DATE_OF_BIRTH AS DOB,edulevel.I18n_DEFAULT AS EDULEVEL, 
ind.REG_ENTITY_ID AS individualNumber,fr.CODE AS FSH_CODE,fr.I18n_DEFAULT AS FSH_ROLE,
enttyp.I18n_DEFAULT AS ENT_DOC_NAME, ind.ENTITY_DOCUMENT_NUMBER AS ENT_DOC_NUMBER FROM reg_entity_individuals AS ind 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN reg_entity_individual_education AS edu ON ent.ID = edu.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 
LEFT JOIN reg_entity_individual_role_fishery AS entfr ON ind.ID = entfr.REG_ENTITY_ID 
LEFT JOIN cl_fish_role_in_fishery AS fr ON entfr.CL_FISH_ROLE_IN_FISHERY_ID = fr.ID 
LEFT JOIN cl_app_entity_document_types AS enttyp ON ind.CL_APP_ENTITY_DOCUMENT_TYPE_ID = enttyp.ID