SELECT 
distinct 
ft.DATE_FROM AS dep_datetime, ft.DATE_TO AS ret_datetime 
FROM dt_fishing_trip AS ft 
LEFT JOIN reg_vessels AS v ON ft.REG_VESSEL_ID = v.ID 