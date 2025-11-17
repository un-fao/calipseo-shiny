SELECT 
s.YEAR as year, 
s.CL_APP_MONTH_ID as month, 
s.DAYS as day, 
s.CL_FISH_LANDING_SITE_ID as landing_site,
s.CL_FISH_FISHING_UNIT_ID as fishing_unit,
s.NB_ACTIVE_FISHING_UNITS as fleet_engagement_number,
fs.NB_FISHING_UNITS as fleet_engagement_max 
FROM dt_effort_survey as s 
LEFT JOIN dt_frame_survey as fs 
ON fs.YEAR = s.YEAR AND 
fs.CL_APP_MONTH_ID = s.CL_APP_MONTH_ID AND 
fs.CL_FISH_LANDING_SITE_ID = s.CL_FISH_LANDING_SITE_ID AND 
fs.CL_FISH_FISHING_UNIT_ID = s.CL_FISH_FISHING_UNIT_ID 
