SELECT cu.CODE FROM ad_country_param as cp 
left join cl_ref_currencies as cu ON (cp.CL_CODE_ID = cu.ID) 
where cp.CODE = 'PREFCURRENCY'