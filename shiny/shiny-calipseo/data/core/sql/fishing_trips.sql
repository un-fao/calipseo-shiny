SELECT 
ft.ID as trip_id, 
ft.DATE_FROM as date_from, 
ft.DATE_TO as date_to, 
v.NAME as vesselname, vt.NAME as vesseltype 
FROM dt_fishing_trip as ft 
LEFT JOIN reg_vessels as v ON ft.REG_VESSEL_ID = v.ID 
LEFT JOIN cl_ref_vessel_types as vt ON vt.ID = v.CL_REF_VESSEL_TYPE_ID 