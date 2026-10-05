SELECT * FROM (
SELECT 
  YEAR as year, 
  CL_APP_MONTH_ID as month, 
  site.CL_STAT_STRATA_ID as minor_strata,
  CL_FISH_LANDING_SITE_ID as landing_site, 
  CL_FISH_FISHING_UNIT_ID as fishing_unit, 
  COUNT(DISTINCT CONCAT(YEAR, '-', CL_APP_MONTH_ID, '-', DAYS)) AS effort_fishable_duration 
FROM 
  dt_effort_survey 
LEFT JOIN cl_fish_landing_sites as site 
ON CL_FISH_LANDING_SITE_ID = site.ID 
WHERE 
  NB_ACTIVE_FISHING_UNITS IS NOT NULL AND NB_ACTIVE_FISHING_UNITS > 0 
GROUP BY 
  YEAR, 
  CL_APP_MONTH_ID, 
  site.CL_STAT_STRATA_ID,
  CL_FISH_LANDING_SITE_ID, 
  CL_FISH_FISHING_UNIT_ID
) as q 