SELECT
fs.`YEAR` as year, 
fs.CL_APP_MONTH_ID as month, 
fs.CL_FISH_LANDING_SITE_ID as landing_site, 
fs.CL_FISH_FISHING_UNIT_ID as fishing_unit, 
ls.CL_REF_ADMIN_LEVEL_1_ID as repart_var, 
fs.NB_FISHING_UNITS as fleet_engagement_number 
FROM dt_frame_survey as fs 
LEFT JOIN cl_fish_landing_sites as ls ON ls.ID = fs.CL_FISH_LANDING_SITE_ID 