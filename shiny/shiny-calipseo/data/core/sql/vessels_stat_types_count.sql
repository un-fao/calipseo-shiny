select ref.ID, ref.CODE, ref.NAME, count(*) as COUNT from reg_vessels reg 
left join cl_app_vessel_stat_type as ref ON CL_APP_VESSEL_STAT_TYPE_ID = ref.ID 
group by reg.CL_APP_VESSEL_STAT_TYPE_ID;