SELECT 
v.REGISTRATION_NUMBER as vessel,
ft.DATE_FROM as time_start,
ft.DATE_TO as time_end,
ft.TIME_SPENT_FISHING_ZONE as effort_fishing_duration,
fdi.CODE_FOR_CLASSIFICATION_TYPE as effort_fishing_duration_unit,
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
pt.CODE_FOR_CLASSIFICATION_TYPE as processing_type,
gear.CODE_FOR_CLASSIFICATION_TYPE as gear_type,
fagc.VALUE_MEASURE as effort_number_gears,
gchi.CODE_FOR_CLASSIFICATION_TYPE as effort_number_gears_unit,
fa.NB_SETS as effort_number_sets,
fmci.CODE_FOR_CLASSIFICATION_TYPE as fishing_mode,
'catch' as measurement,
'RC' as measurement_type,
fas.CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT as measurement_value,
lower(unit.CODE) as measurement_unit 
FROM dt_fishing_activities as fa 
LEFT JOIN dt_fishing_trip as ft ON ft.ID = fa.DT_FISHING_TRIP_ID 
LEFT JOIN cl_app_quantity_units as qu ON qu.ID = ft.CL_TIME_SPENT_FISHING_UNIT_ID 
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
LEFT JOIN dt_fishing_activity_gear_characteristics as fagc ON fagc.ID = fas.DT_FISHING_ACTIVITY_GEAR_ID 
LEFT JOIN jt_fish_gear_characteristic_ref_effort_unit_classification_item as fagcmap ON fagcmap.CL_REF_GEAR_CHARACTERISTIC_ID = fagc.CL_REF_GEAR_CHARACTERISTIC_ID 
LEFT JOIN cl_ref_gear_characteristic_classification_items as gchi ON gchi.ID = fagcmap.CL_REF_GEAR_CHARACTERISTIC_CLASSIFICATION_ITEM_ID 
LEFT JOIN cl_ref_gear_characteristic_classification_types as gcht ON gcht.ID = gchi.CL_REF_GEAR_CHARACTERISTIC_CLASSIFICATION_TYPE_ID 
LEFT JOIN jt_app_quantity_unit_ref_effort_unit_classification_item as fdmap ON fdmap.CL_APP_QUANTITY_UNIT_ID = qu.ID 
LEFT JOIN cl_ref_gear_characteristic_classification_items as fdi ON fdi.ID = fdmap.CL_REF_GEAR_CHARACTERISTIC_CLASSIFICATION_ITEM_ID 
LEFT JOIN cl_ref_gear_characteristic_classification_types as fdt ON fdt.ID = fdi.CL_REF_GEAR_CHARACTERISTIC_CLASSIFICATION_TYPE_ID 
LEFT JOIN cl_fish_fishing_modes as ffm ON ffm.ID = fa.CL_FISH_FISHING_MODE_ID 
LEFT JOIN jt_fish_fishing_mode_ref_fishing_mode_classification_item as fmmap ON fmmap.CL_FISH_FISHING_MODE_ID = ffm.ID 
LEFT JOIN cl_ref_fishing_mode_classification_items as fmci ON fmci.ID = fmmap.CL_REF_FISHING_MODE_CLASSIFICATION_ITEM_ID 
LEFT JOIN cl_ref_fishing_mode_classification_types as fmct ON fmct.ID = fmci.CL_REF_FISHING_MODE_CLASSIFICATION_TYPE_ID 
LEFT JOIN jt_fish_fishing_zone_ref_fishing_zone_classification_item as fzmap ON fzmap.CL_FISH_FISHING_ZONE_ID = ft.CL_FISH_FISHING_ZONE_ID 
LEFT JOIN cl_ref_fishing_zone_classification_items as fz ON fz.ID = fzmap.CL_REF_FISHING_ZONE_CLASSIFICATION_ITEM_ID 
LEFT JOIN cl_ref_fishing_zone_classification_types as fzct ON fzct.ID = fz.CL_REF_FISHING_ZONE_CLASSIFICATION_TYPE_ID 
LEFT JOIN cl_fish_processing_types as fpt ON fpt.ID = fas.CL_FISH_PROCESSING_TYPE_ID 
LEFT JOIN jt_fish_processing_type_ref_processing_type_classification_item as fptmap ON fptmap.CL_FISH_PROCESSING_TYPE_ID = fas.CL_FISH_PROCESSING_TYPE_ID 
LEFT JOIN cl_ref_processing_type_classification_items as pt ON pt.ID = fptmap.CL_REF_PROCESSING_TYPE_CLASSIFICATION_ITEM_ID 
LEFT JOIN cl_ref_processing_type_classification_types as ptct ON ptct.ID = pt.CL_REF_PROCESS_TYPE_CLASSIFICATION_TYPE_ID 
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
WHERE fa.CL_FISH_FISHING_ACTIVITY_TYPE_ID IN(1,3) AND fag.NO_CATCHES = '0' 