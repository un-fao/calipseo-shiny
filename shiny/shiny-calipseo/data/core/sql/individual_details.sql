SELECT 'fisher' AS Category, Salutation,FIRST_NAME,MIDDLE_NAME,SUFFIX_NAME,NAME, Gender, DOB, IndividualNumber, Fisher_Id,Owner,Captain,License 
FROM(
SELECT distinct (ind.REG_ENTITY_ID) AS IndividualNumber, salu.I18n_DEFAULT as Salutation, ind.FIRST_NAME, ind.MIDDLE_NAME, ind.SUFFIX_NAME, 
ent.NAME, gend.I18n_DEFAULT AS Gender,ind.DATE_OF_BIRTH AS DOB,edulevel.I18n_DEFAULT AS Edulevel,
IF(regv.REG_ENTITY_OWNER_ID is null, 0,1) AS Owner, IF(regv.REG_ENTITY_CAPTAIN_ID IS NULL, 0,1) AS Captain, IF(ls.PERMIT_NUMBER IS NULL, 0,1) AS License,
IF(ind.FISHER_ID IS NULL, 0,1) AS Fisher_Id FROM reg_entity_individuals AS ind 
LEFT JOIN reg_vessels AS regv ON ind.REG_ENTITY_ID = regv.ID 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN reg_entity_licenses AS ls ON ind.REG_ENTITY_ID = ls.REG_ENTITY_ID 
LEFT JOIN reg_entity_individual_education AS edu ON ind.REG_ENTITY_ID = edu.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 
) AS ind_detail 
WHERE Owner > 0 OR Captain > 0 OR License > 0 OR Fisher_Id > 0 UNION 
SELECT 'nonfisher' AS Category, Salutation,FIRST_NAME,MIDDLE_NAME,SUFFIX_NAME,NAME, Gender, DOB, IndividualNumber, Fisher_Id,Owner,Captain,License 
FROM(
SELECT distinct (ind.REG_ENTITY_ID) AS IndividualNumber, salu.I18n_DEFAULT as Salutation, ind.FIRST_NAME, ind.MIDDLE_NAME, ind.SUFFIX_NAME, 
ent.NAME, gend.I18n_DEFAULT AS Gender,ind.DATE_OF_BIRTH AS DOB,edulevel.I18n_DEFAULT AS Edulevel,
IF(regv.REG_ENTITY_OWNER_ID IS NULL, 0,1) AS Owner, IF(regv.REG_ENTITY_CAPTAIN_ID IS NULL, 0,1) AS Captain, IF(ls.PERMIT_NUMBER IS NULL, 0,1) AS License,
IF(ind.FISHER_ID IS NULL, 0,1) AS Fisher_Id FROM reg_entity_individuals AS ind 
LEFT JOIN reg_vessels AS regv ON ind.REG_ENTITY_ID = regv.ID 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN reg_entity_licenses AS ls ON ind.REG_ENTITY_ID = ls.REG_ENTITY_ID 
LEFT JOIN reg_entity_individual_education AS edu ON ind.REG_ENTITY_ID = edu.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
LEFT JOIN cl_app_salutations AS salu ON ind.CL_APP_SALUTATION_ID = salu.ID 
) AS ind_detail 
WHERE Owner = 0 AND Captain = 0 AND License = 0 AND Fisher_Id = 0