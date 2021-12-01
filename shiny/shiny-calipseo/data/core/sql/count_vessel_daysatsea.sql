SELECT 
distinct 
ft.DATE_FROM as dep_datetime, ft.DATE_TO as ret_datetime 
FROM dt_fishing_trip as ft 
LEFT JOIN reg_vessels as v ON ft.REG_VESSEL_ID = v.ID 