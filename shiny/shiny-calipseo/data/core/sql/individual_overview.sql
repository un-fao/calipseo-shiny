SELECT 'fisher' as Category, gend.I18n_DEFAULT AS Gender,ind.DATE_OF_BIRTH AS DOB,edulevel.I18n_DEFAULT AS Edulevel, 
ind.REG_ENTITY_ID AS individualNumber,enttyp.CODE AS DOC_CODE,fr.CODE AS FSH_CODE,
fr.I18n_DEFAULT AS FSH_ROLE,ls.PERMIT_NUMBER,ind.FISHER_ID FROM reg_entity_individuals AS ind 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN reg_entity_individual_education AS edu ON ent.ID = edu.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 
LEFT JOIN reg_entity_individual_role_fishery AS entfr ON ind.ID = entfr.REG_ENTITY_ID 
LEFT JOIN cl_fish_role_in_fishery AS fr ON entfr.CL_FISH_ROLE_IN_FISHERY_ID = fr.ID 
LEFT JOIN cl_app_entity_document_types AS enttyp ON ind.CL_APP_ENTITY_DOCUMENT_TYPE_ID = enttyp.ID 
LEFT JOIN reg_entity_licenses AS ls ON ent.ID = ls.REG_ENTITY_ID 
where ind.FISHER_ID is not null AND ind.FISHER_ID != '' OR ls.PERMIT_NUMBER is not null OR 
fr.CODE  LIKE '%CAP' OR fr.CODE  LIKE '%OWN' OR fr.CODE LIKE '%FIS' UNION 
SELECT 'nonfisher' as Category, gend.I18n_DEFAULT AS Gender,ind.DATE_OF_BIRTH AS DOB,edulevel.I18n_DEFAULT AS Edulevel, 
ind.REG_ENTITY_ID AS individualNumber,enttyp.CODE AS DOC_CODE,fr.CODE AS FSH_CODE,
fr.I18n_DEFAULT AS FSH_ROLE,ls.PERMIT_NUMBER,ind.FISHER_ID FROM reg_entity_individuals AS ind 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN reg_entity_individual_education AS edu ON ent.ID = edu.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 
LEFT JOIN reg_entity_individual_role_fishery AS entfr ON ind.ID = entfr.REG_ENTITY_ID 
LEFT JOIN cl_fish_role_in_fishery AS fr ON entfr.CL_FISH_ROLE_IN_FISHERY_ID = fr.ID 
LEFT JOIN cl_app_entity_document_types AS enttyp ON ind.CL_APP_ENTITY_DOCUMENT_TYPE_ID = enttyp.ID 
LEFT JOIN reg_entity_licenses AS ls ON ent.ID = ls.REG_ENTITY_ID 
where fr.CODE  not Like '%CAP' AND fr.CODE  not LIKE '%OWN' AND fr.CODE not LIKE '%FIS' UNION 
SELECT 'notdefined' as Category, gend.I18n_DEFAULT AS Gender,ind.DATE_OF_BIRTH AS DOB,edulevel.I18n_DEFAULT AS Edulevel, 
ind.REG_ENTITY_ID AS individualNumber,enttyp.CODE AS DOC_CODE,fr.CODE AS FSH_CODE,
fr.I18n_DEFAULT AS FSH_ROLE,ls.PERMIT_NUMBER,ind.FISHER_ID FROM reg_entity_individuals AS ind 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN reg_entity_individual_education AS edu ON ent.ID = edu.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 
LEFT JOIN reg_entity_individual_role_fishery AS entfr ON ind.ID = entfr.REG_ENTITY_ID 
LEFT JOIN cl_fish_role_in_fishery AS fr ON entfr.CL_FISH_ROLE_IN_FISHERY_ID = fr.ID 
LEFT JOIN cl_app_entity_document_types AS enttyp ON ind.CL_APP_ENTITY_DOCUMENT_TYPE_ID = enttyp.ID 
LEFT JOIN reg_entity_licenses AS ls ON ent.ID = ls.REG_ENTITY_ID 
where fr.CODE  is null