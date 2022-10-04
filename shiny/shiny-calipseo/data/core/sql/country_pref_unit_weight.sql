SELECT qu.NAME FROM ad_country_param as cp 
left join cl_app_quantity_units as qu ON (cp.CL_CODE_ID = qu.ID) 
where cp.CODE = 'PREFUNITWEIGHT'