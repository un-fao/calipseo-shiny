SELECT 
	surv.ID as interview_id,
	YEAR(surv.DATE_INTERVIEW) as year,
	MONTH(surv.DATE_INTERVIEW) as month,
	DAY(surv.DATE_INTERVIEW) as day,
	sit.CL_STAT_STRATA_ID as minor_strata,
	sit.ID as landing_site,
	eh.HOUSEHOLD_SURVEY_IDENTIFIER as household,
	eh.CL_STAT_HOUSEHOLD_TYPE_ID as household_type,
	surv.FISHING_DAY as fishing_day,
	rec.YEAR as fishing_trip_year,
	rec.CL_APP_MONTH_ID as fishing_trip_month,
	rec.QUARTER as fishing_trip_quarter,
	rec.DAY as fishing_trip_day,
	rec.CL_FISH_FISHING_UNIT_ID as fishing_unit,
	rec.NB_GEARS as gear_number,
	rec.TIME_SPENT_FISHING as effort_fishing_duration,
	rec.CL_TIME_SPENT_FISHING_QUANTITY_UNIT_ID as effort_fishing_duration_unit 
FROM dt_activity_surveys as surv 
LEFT JOIN cl_fish_landing_sites as sit ON surv.CL_FISH_LANDING_SITE_ID = sit.ID 
LEFT JOIN reg_entity_households as eh ON surv.REG_ENTITY_ID = eh.REG_ENTITY_ID 
LEFT JOIN dt_activity_records as rec ON surv.DT_SURVEY_ID = rec.DT_ACTIVITY_SURVEY_ID
