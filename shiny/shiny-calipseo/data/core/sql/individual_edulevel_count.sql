SELECT edulevel.CODE, edulevel.I18n_DEFAULT as NAME, count(*) as COUNT FROM reg_entity_individual_education AS edu 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN reg_entity_individuals AS ind ON edu.ID = ind.ID 
LEFT JOIN cl_app_genders as gend ON CL_APP_GENDER_ID = gend.ID 
WHERE ent.ENABLED = 1 