SELECT DISTINCT ent.ID, ent.NAME  FROM dt_fishing_trip as ft 
LEFT JOIN reg_vessels as v ON v.ID = ft.REG_VESSEL_ID 
LEFT JOIN reg_entity_companies as comp ON comp.ID = v.REG_ENTITY_OWNER_ID 
LEFT JOIN reg_entities as ent ON ent.ID = comp.ID 
WHERE v.CL_APP_VESSEL_STAT_TYPE_ID = 2 ORDER BY ent.NAME