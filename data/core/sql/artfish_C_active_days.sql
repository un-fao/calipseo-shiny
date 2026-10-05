SELECT 
YEAR as year, 
CL_APP_MONTH_ID as month,
site.CL_STAT_STRATA_ID as minor_strata,
CL_FISH_LANDING_SITE_ID as landing_site,
CL_FISH_FISHING_UNIT_ID as fishing_unit, 
MAX(NB_ACTIVE_DAYS) as effort_fishable_duration 
FROM dt_frame_survey as fs 
LEFT JOIN cl_fish_landing_sites as site 
ON CL_FISH_LANDING_SITE_ID = site.ID