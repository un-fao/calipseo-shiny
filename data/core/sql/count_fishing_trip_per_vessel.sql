SELECT 
year(ft.DATE_TO) as Year,
count(*) AS sum_no_trips_per_year 
FROM dt_fishing_trip AS ft 
LEFT JOIN reg_vessels AS v ON ft.REG_VESSEL_ID = v.ID 
