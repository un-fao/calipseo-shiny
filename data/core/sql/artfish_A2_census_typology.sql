SELECT 
	YEAR(ht.DATE) as year, 
	MONTH(ht.DATE) as month,
	ref.CL_REF_ADMIN_LEVEL_1_ID as minor_strata,
	ht.CL_REF_ADMIN_LEVEL_4_ID as settlement,
	ht.CL_STAT_HOUSEHOLD_TYPE_ID as household_type,
	sum(ht.NB_HH) as household_number 
FROM dt_household_typology as ht 
LEFT JOIN cl_ref_admin_level_4 as ref ON ht.CL_REF_ADMIN_LEVEL_4_ID = ref.ID 
GROUP BY year, month, minor_strata, settlement, household_type 