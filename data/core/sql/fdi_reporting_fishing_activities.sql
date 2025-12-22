SELECT 
v.REGISTRATION_NUMBER as vessel,
ft.DATE_FROM as time_start,
ft.DATE_TO as time_end,
ft.ID as fishing_trip,
ftt.CODE as fishing_trip_type,
ftt.RANKING as fishing_trip_type_priority,
ds.CODE_FOR_CLASSIFICATION_TYPE as data_source,
fa.ID as fishing_activity,
fz.CODE_FOR_CLASSIFICATION_TYPE as fishing_zone,
geo.longitude_start,
geo.latitude_start,
geo.longitude_end,
geo.latitude_end,
sp.ASFIS_CODE as species,
gear.CODE_FOR_CLASSIFICATION_TYPE as gear_type,
'catch' as measurement,
'NL' as measurement_type,
fas.CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT as measurement_value,
lower(unit.CODE) as measurement_unit 
FROM dt_fishing_activities as fa 
LEFT JOIN dt_fishing_trip as ft ON ft.ID = fa.DT_FISHING_TRIP_ID 
LEFT JOIN cl_fish_fishing_trip_types as ftt ON ftt.ID = ft.CL_FISH_FISHING_TRIP_TYPE_ID 
LEFT JOIN reg_vessels as v ON v.ID = ft.REG_VESSEL_ID 
LEFT JOIN jt_fish_fishing_trip_type_ref_data_source_classification_item as fttmap ON fttmap.CL_FISH_FISHING_TRIP_TYPE_ID = ft.CL_FISH_FISHING_TRIP_TYPE_ID 
LEFT JOIN cl_ref_data_source_classification_items as ds ON ds.ID = fttmap.CL_REF_DATA_SOURCE_CLASSIFICATION_ITEM_ID 
LEFT JOIN cl_ref_data_source_classification_types as dsct ON dsct.ID = ds.CL_REF_DATA_SOURCE_CLASSIFICATION_TYPE_ID 
LEFT JOIN dt_fishing_activities_species as fas ON fa.ID = fas.DT_FISHING_ACTIVITY_ID 
LEFT JOIN cl_ref_species as sp ON sp.ID = fas.CL_REF_SPECIES_ID 
LEFT JOIN cl_app_quantity_units as unit ON unit.ID = fas.CL_APP_QUANTITY_UNIT_ID 
LEFT JOIN dt_fishing_activities_gear as fag ON fag.ID = fas.DT_FISHING_ACTIVITY_GEAR_ID 
LEFT JOIN jt_ref_gear_ref_gear_classification_item as fagmap ON fagmap.CL_REF_GEAR_ID = fag.CL_REF_GEAR_ID 
LEFT JOIN cl_ref_gear_classification_items as gear ON gear.ID = fagmap.CL_REF_GEAR_CLASSIFICATION_ITEM_ID 
LEFT JOIN cl_ref_gear_classification_types as gearct ON gearct.ID = gear.CL_REF_GEAR_CLASSIFICATION_TYPE_ID 
LEFT JOIN jt_fish_fishing_zone_ref_fishing_zone_classification_item as fzmap ON fzmap.CL_FISH_FISHING_ZONE_ID = ft.CL_FISH_FISHING_ZONE_ID 
LEFT JOIN cl_ref_fishing_zone_classification_items as fz ON fz.ID = fzmap.CL_REF_FISHING_ZONE_CLASSIFICATION_ITEM_ID 
LEFT JOIN cl_ref_fishing_zone_classification_types as fzct ON fzct.ID = fz.CL_REF_FISHING_ZONE_CLASSIFICATION_TYPE_ID 
LEFT JOIN (SELECT 
    pos.DT_FISHING_ACTIVITY_ID,
    MAX(CASE WHEN post.CODE = 'SPT' THEN pos.LONGITUDE END) AS longitude_start,
    MAX(CASE WHEN post.CODE = 'SPT' THEN pos.LATITUDE  END) AS latitude_start,
    MAX(CASE WHEN post.CODE = 'EPT' THEN pos.LONGITUDE END) AS longitude_end,
    MAX(CASE WHEN post.CODE = 'EPT' THEN pos.LATITUDE  END) AS latitude_end 
FROM dt_fishing_activity_positions pos 
LEFT JOIN cl_fish_fishing_activity_positions_types as post ON post.ID = pos.CL_FISH_FISHING_ACTIVITY_POSITIONS_TYPE_ID 
WHERE pos.CL_FISH_FISHING_ACTIVITY_POSITIONS_TYPE_ID IN(1,2) 
GROUP BY pos.DT_FISHING_ACTIVITY_ID) as geo ON geo.DT_FISHING_ACTIVITY_ID = fa.ID 
WHERE fa.CL_FISH_FISHING_ACTIVITY_TYPE_ID = 1 AND fag.NO_CATCHES = '0'