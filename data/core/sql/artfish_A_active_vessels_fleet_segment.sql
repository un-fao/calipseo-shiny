SELECT 
fs.`YEAR` as year, 
fs.CL_APP_MONTH_ID as month, 
fs.CL_FISH_LANDING_SITE_ID as landing_site, 
fs.CL_FISH_FISHING_UNIT_ID as fishing_unit, 
s.CODE as repart_var, 
SUM(fs.NB_FISHING_UNITS) as fleet_engagement_number 
FROM dt_frame_survey as fs 
LEFT JOIN cl_fish_fishing_unit_segment_mapping as fufs ON fufs.CL_FISH_FISHING_UNIT_ID = fs.CL_FISH_FISHING_UNIT_ID 
LEFT JOIN cl_fish_fleet_segments as s ON s.ID = fufs.CL_FISH_FLEET_SEGMENT_ID 
GROUP BY fs.YEAR, fs.CL_APP_MONTH_ID, fs.CL_FISH_LANDING_SITE_ID, fs.CL_FISH_FISHING_UNIT_ID, s.CODE