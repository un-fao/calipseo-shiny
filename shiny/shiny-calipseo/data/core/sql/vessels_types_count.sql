select ref.CODE, ref.I18n_DEFAULT as NAME, count(*) as COUNT from reg_vessels reg 
left join cl_ref_vessel_types as ref ON CL_REF_VESSEL_TYPE_ID = ref.ID 
group by reg.CL_REF_VESSEL_TYPE_ID;