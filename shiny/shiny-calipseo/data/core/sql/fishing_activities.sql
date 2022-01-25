SELECT 
ft.ID as landing_id, 
ft.DATE_FROM as dep_datetime, ft.DATE_TO as ret_datetime, 
v.REGISTRATION_NUMBER as regnum, vt.I18n_DEFAULT as vesstype,v.CL_APP_VESSEL_STAT_TYPE_ID as stat_type_id,ft.CREW_NUMBER as crew,
fz.CODE as gr_f_area_id, fz.I18n_DEFAULT as gr_f_area,
ls.CODE as arrivalport, ls.ID as bch_id, ls.I18n_DEFAULT as bch_name,
g.CODE as f_mthd_id, g.I18n_DEFAULT as f_mthd,
sp.CODE  as species_id, sp.I18n_DEFAULT as species_desc,
fas.QUANTITY as quantity, uom.I18n_DEFAULT as quantity_unit, fas.TOTAL_VALUE as value 
FROM dt_fishing_activities as fa 
LEFT JOIN dt_fishing_activities_gear as fag ON fa.ID = fag.DT_FISHING_ACTIVITY_ID 
LEFT JOIN cl_ref_gears as g ON fag.CL_REF_GEAR_ID = g.ID 
LEFT JOIN dt_fishing_activities_species as fas ON fa.ID = fas.DT_FISHING_ACTIVITY_ID 
LEFT JOIN cl_ref_species as sp ON fas.CL_REF_SPECIES_ID = sp.ID 
LEFT JOIN cl_app_quantity_units uom ON fas.CL_APP_QUANTITY_UNIT_ID = uom.ID 
LEFT JOIN dt_fishing_trip as ft ON fa.DT_FISHING_TRIP_ID = ft.ID 
LEFT JOIN reg_vessels as v ON ft.REG_VESSEL_ID = v.ID 
LEFT JOIN cl_ref_vessel_types as vt ON vt.ID = v.CL_REF_VESSEL_TYPE_ID 
LEFT JOIN cl_fish_fishing_zones as fz ON fz.ID = ft.CL_FISH_FISHING_ZONE_ID 
LEFT JOIN cl_fish_landing_sites as ls ON ls.ID = ft.CL_TO_PORT_SITE_ID 
LEFT JOIN reg_entities as ent ON ent.ID = v.REG_ENTITY_OWNER_ID