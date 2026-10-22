SELECT cnt.ISO_3_CODE 
FROM ad_country_param as cp 
LEFT JOIN cl_ref_countries cnt ON cnt.ID = cp.VALUE_NO_UNIT 
WHERE cp.CODE = 'ISOCODE'