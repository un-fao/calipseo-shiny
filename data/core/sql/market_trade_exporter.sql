SELECT
    dt_market_visits.ID AS visit_id,
    YEAR(dt_market_visits.RECORD_DATE) AS year,
    dt_market_visits.RECORD_DATE AS date,
    cl_ref_fish_markets.CODE AS market_code,
	  cl_ref_fish_markets.I18N_DEFAULT AS market_name,
	  cl_ref_vendor_trade_types.CODE AS trade_flow_type,
    sg.I18N_DEFAULT AS species_group,
    s.I18N_DEFAULT AS species,
    dt_market_visit_details.QUANTITY AS quantity, 
    dt_market_visit_details.PRICE_PER_UNIT_WEIGHT AS price 

FROM dt_market_visits 

INNER JOIN dt_market_visit_details 
    ON dt_market_visits.ID = dt_market_visit_details.DT_MARKET_VISIT_ID 

INNER JOIN cl_ref_fish_markets 
    ON dt_market_visits.CL_REF_FISH_MARKET_ID = cl_ref_fish_markets.ID 

INNER JOIN cl_ref_vendor_trade_types 
    ON dt_market_visit_details.CL_REF_VENDOR_TRADE_TYPE_ID = cl_ref_vendor_trade_types.ID 

INNER JOIN cl_ref_species s 
    ON dt_market_visit_details.CL_REF_SPECIES_ID = s.ID 

LEFT JOIN jt_ref_species_ref_species_group j 
    ON s.ID = j.CL_REF_SPECIES_ID 

LEFT JOIN cl_ref_species_groups sg 
    ON j.CL_REF_SPECIES_GROUP_ID = sg.ID 

LEFT JOIN cl_ref_species_group_types sgt 
    ON sg.CL_REF_SPECIES_GROUP_TYPE_ID = sgt.ID 

WHERE sgt.ID = 1