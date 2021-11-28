SELECT 
year(ft.DATE_TO) as Year,
count(*) as sum_no_trips_per_year 
FROM dt_fishing_trip as ft 
LEFT JOIN reg_vessels as v ON ft.REG_VESSEL_ID = v.ID 
