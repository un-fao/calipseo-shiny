SELECT 
ft.ID, 
ft.DATE_TO as DATE, 
fs.CODE as FLEET_SEGMENT, 
g.NAME as FISHING_GEAR, 
sp.FAMILY_NAME_EN as FAMILY,sp.SCIENTIFIC_NAME,sp.I18n_DEFAULT as species_desc, sp.ASFIS_CODE as species_asfis, 
fas.CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT as TOTAL_LANDINGS, fas.CATCH_NUMBER as TOTAL_NUMBER, sp.COMMENT as comment 
FROM dt_fishing_activities as fa 
LEFT JOIN dt_fishing_activities_gear as fag ON fa.ID = fag.DT_FISHING_ACTIVITY_ID 
LEFT JOIN cl_ref_gears as g ON fag.CL_REF_GEAR_ID = g.ID 
LEFT JOIN dt_fishing_activities_species as fas ON fa.ID = fas.DT_FISHING_ACTIVITY_ID 
LEFT JOIN cl_ref_species as sp ON fas.CL_REF_SPECIES_ID = sp.ID 
LEFT JOIN cl_app_quantity_units uom ON fas.CL_APP_QUANTITY_UNIT_ID = uom.ID 
LEFT JOIN dt_fishing_trip as ft ON fa.DT_FISHING_TRIP_ID = ft.ID 
LEFT JOIN cl_fish_fishing_unit_segment_mapping as fufs ON fufs.CL_FISH_FISHING_UNIT_ID = ft.CL_FISH_FISHING_UNIT_ID 
LEFT JOIN cl_fish_fleet_segments as fs ON fs.ID = fufs.CL_FISH_FLEET_SEGMENT_ID 