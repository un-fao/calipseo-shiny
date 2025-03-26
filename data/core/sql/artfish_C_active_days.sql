SELECT 
YEAR as year, 
CL_APP_MONTH_ID as month,
CL_FISH_LANDING_SITE_ID as landing_site,
CL_FISH_FISHING_UNIT_ID as fishing_unit, 
MAX(NB_ACTIVE_DAYS) as effort_fishable_duration 
FROM dt_frame_survey 