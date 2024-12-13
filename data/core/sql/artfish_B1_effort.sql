SELECT 
s.YEAR as year, 
s.CL_APP_MONTH_ID as month, 
s.DAYS as day, 
s.CL_FISH_FISHING_UNIT_ID as fishing_unit,
st.MAXIMUM_NUMBER_DAYS_SAMPLED as effort_type,
s.NB_DAYS_SAMPLED as effort_fishing_duration 
FROM dt_effort_survey as s 
LEFT JOIN cl_stat_effort_survey_types as st ON st.ID = s.CL_STAT_EFFORT_SURVEY_TYPE_ID