SELECT * FROM (SELECT 
ind.REG_ENTITY_ID as ID, 
gend.I18n_DEFAULT AS Gender, 
ind.DATE_OF_BIRTH AS DOB, 
edulevel.I18n_DEFAULT as Eduction, 
role.I18n_DEFAULT as Role, 
ind.FISHER_ID_REGISTRATION_DATE as Regdate, 
wt.I18n_DEFAULT as Worktime, 
lt.I18n_DEFAULT as License, 
ls.I18n_DEFAULT as Site 
FROM reg_entity_individuals AS ind 
  LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
  LEFT JOIN reg_entity_individual_education AS edu ON ind.REG_ENTITY_ID = edu.ID 
  LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID  
  LEFT JOIN cl_app_genders AS gend ON ind.CL_APP_GENDER_ID = gend.ID 
  LEFT JOIN reg_entity_individual_role_fishery AS r ON ind.REG_ENTITY_ID = r.REG_ENTITY_ID 
  LEFT JOIN cl_fish_role_in_fishery AS role ON r.CL_FISH_ROLE_IN_FISHERY_ID = role.ID 
  LEFT JOIN cl_fish_fisher_activity_type AS wt ON ind.CL_FISH_FISHER_ACTIVITY_TYPE_ID = wt.ID 
  LEFT JOIN reg_entity_licenses AS lc ON ind.REG_ENTITY_ID = lc.REG_ENTITY_ID 
  LEFT JOIN cl_fish_entity_license_types AS lt ON lc.CL_FISH_ENTITY_LICENSE_TYPE_ID = lt.ID 
  LEFT JOIN cl_fish_landing_sites AS ls ON ls.ID = r.CL_OPERATING_SITE_ID 
   WHERE ent.ENABLED = 1
) AS ind_overview 