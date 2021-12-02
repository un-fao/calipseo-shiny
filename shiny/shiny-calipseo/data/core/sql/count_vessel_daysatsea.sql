SELECT 
distinct 
ft.ID, ft.DATE_FROM AS dep_datetime, ft.DATE_TO as ret_datetime 
FROM dt_fishing_trip AS ft 
LEFT JOIN reg_vessels AS v ON ft.REG_VESSEL_ID = v.ID 