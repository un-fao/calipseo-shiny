SELECT 
s.YEAR as year, 
s.CL_APP_MONTH_ID as month, 
s.DAYS as days, 
st.MAXIMUM_NUMBER_DAYS_SAMPLED as effort_type,
s.CL_FISH_LANDING_SITE_ID as landing_site,s.CL_FISH_FISHING_UNIT_ID as fishing_unit,s.NB_DAYS_SAMPLED as days_sampled,
f.BG,
f.NB_ACTIVE_DAYS 
FROM dt_effort_survey as s 
LEFT JOIN cl_stat_effort_survey_types as st ON st.ID = s.CL_STAT_EFFORT_SURVEY_TYPE_ID 
LEFT JOIN(
SELECT 
YEAR, CL_APP_MONTH_ID, CL_FISH_FISHING_UNIT_ID, SUM(NB_FISHING_UNITS) as BG, MAX(NB_ACTIVE_DAYS) as NB_ACTIVE_DAYS FROM dt_frame_survey 
GROUP BY YEAR, CL_APP_MONTH_ID, CL_FISH_FISHING_UNIT_ID 
) as f USING (CL_APP_MONTH_ID, YEAR,CL_FISH_FISHING_UNIT_ID)