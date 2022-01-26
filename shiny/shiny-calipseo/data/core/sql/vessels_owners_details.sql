SELECT 
DISTINCT e.ID,
g.I18n_DEFAULT as gender, year(ei.DATE_OF_BIRTH) as year_of_birth,
e.NAME as name,
et.CODE as type_code, et.I18n_DEFAULT as type,
vt.I18n_DEFAULT as vesseltype 
FROM reg_vessels as v 
LEFT JOIN reg_entities as e ON v.REG_ENTITY_OWNER_ID = e.ID 
LEFT JOIN cl_app_entity_types as et ON e.CL_APP_ENTITY_TYPE_ID = et.ID 
LEFT JOIN reg_entity_individuals as ei ON e.ID = ei.REG_ENTITY_ID  
LEFT JOIN cl_app_genders as g ON ei.CL_APP_GENDER_ID = g.ID 
LEFT JOIN cl_ref_vessel_types as vt ON vt.ID = v.CL_REF_VESSEL_TYPE_ID 