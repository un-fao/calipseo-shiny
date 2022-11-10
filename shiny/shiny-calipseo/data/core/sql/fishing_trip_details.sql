SELECT 
ft.ID,
ft.DATE_FROM as date_from , ft.DATE_TO as date_to, 
v.NAME as vessel_name,v.REGISTRATION_NUMBER as reg_number, vt.I18n_DEFAULT as vesseltype, 
fz.I18n_DEFAULT as fishing_zone, fz.COORDINATE_LONGITUDE as fz_longitude, fz.COORDINATE_LATITUDE as fz_latitude,
ls.I18n_DEFAULT as landing_site, ls.LONGITUDE as ls_longitude, ls.LATITUDE as ls_latitude,
g.NAME as fishing_gear,
sp.I18n_DEFAULT as species_desc, sp.SCIENTIFIC_NAME as species_sci, sp.ASFIS_CODE as species_asfis, sp.COMMENT as comment, 
fas.CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT as quantity, uom.I18n_DEFAULT as quantity_unit 
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