select gend.CODE, gend.I18n_DEFAULT as NAME, count(*) as COUNT from reg_entity_individuals as ind 
left join cl_app_genders as gend ON CL_APP_GENDER_ID = gend.ID 
group by ind.CL_APP_GENDER_ID;