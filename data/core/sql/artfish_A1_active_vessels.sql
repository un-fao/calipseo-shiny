SELECT 
YEAR as year, 
CL_APP_MONTH_ID as month, 
site.CL_STAT_STRATA_ID as minor_strata,
CL_FISH_LANDING_SITE_ID as landing_site, 
CL_FISH_FISHING_UNIT_ID as fishing_unit, 
SUM(NB_FISHING_UNITS) as fleet_engagement_number 
FROM dt_frame_survey 
LEFT JOIN cl_fish_landing_sites as site 
ON CL_FISH_LANDING_SITE_ID = site.ID