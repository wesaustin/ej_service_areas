
* Creating indicators for arsenic and nitrate using the SYR3 and SYR4 data
* last edited: 5/10/24

********************************************************************************
* SYR3
********************************************************************************

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\processed\syr3_arsenic.csv" , clear
* varlist analyte_code analyte_name state_code pws_id system_name system_type retail_population_served adjusted_total_population_served source_water_type water_facility_id water_facility_type sampling_point_id sampling_point_type source_type_code sample_type_code laboratory_assigned_id six_year_id sample_id sample_collection_date detection_limit_value detection_limit_unit detection_limit_code detect result_value unit presence_indicator_code residual_field_free_chlorine_mg_ residual_field_total_chlorine_mg
rename (pws_id result_value) (pwsid value)
tab unit 
tempfile syr3_arsenic
save `syr3_arsenic'


import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\processed\syr3_nitrate.csv" , clear
rename (pws_id result_value) (pwsid value)
tab unit 
tempfile syr3_nitrate
save `syr3_nitrate'


********************************************************************************
* SYR4
********************************************************************************

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\arsenic\data\SUMMARY_ANALYTE_ARSENIC.txt", clear 
tab unit 
append using `syr3_arsenic' , force
destring value,  replace  ignore("NA")

replace value= 0 if value == . 
collapse (mean) value , by(pwsid)
rename value arsenic
export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_arsenic.csv", replace





import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\SYR4\SUMMARY_ANALYTE_NITRATE.txt", clear 

* varlist: analyte_code analyte_name state_code pwsid system_name system_type retail_population_served adjusted_total_population_served source_water_type water_facility_id water_facility_type sampling_point_id sampling_point_type source_type_code sample_type_code laboratory_assigned_id six_year_id sample_id sample_collection_date detection_limit_value detection_limit_unit detection_limit_code detect value unit presence_indicator_code residual_field_free_chlorine_mg_ residual_field_total_chlorine_mg
tab unit 
append using `syr3_nitrate' , force 
destring value,  replace  ignore("NA")
replace value= 0 if value == . 
collapse (mean) value , by(pwsid ) /*collapsing to annual values and then overall value */
rename value nitrate
export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_nitrate.csv", replace
