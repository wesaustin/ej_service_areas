


********************************************************************************
* TCR indicator  
********************************************************************************


* Editing coliform indicator 

use "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\Flooding and Drinking Water\data\AP_coliform_v2.dta" 

drop dam_property injuries deaths flood flash_flood heavy_rain flood_21d flood_10d flood_3d flash_flood_21d flash_flood_10d flash_flood_3d heavy_rain_21d heavy_rain_10d heavy_rain_3d both both_3d both_10d both_21d tmean precip_mean precip_sum l1tmean l1precip_mean l2tmean l2precip_mean l3tmean l3precip_mean l4tmean l4precip_mean l5tmean l5precip_mean l6tmean l6precip_mean l7tmean l7precip_mean f1precip_mean f2precip_mean f3precip_mean precip_l14 precip_l13 precip_l12 precip_l11 precip_l10 precip_l9 precip_l8 precip_l7 precip_l6 precip_l5 precip_l4 precip_l3 precip_l2 precip_l1 precip_l0 precip_f1 precip_f2 precip_f3 precip_f4 precip_f5 precip_f6 precip_f7 precip_f8 precip_f9 precip_f10 precip_f11 precip_f12 precip_f13 precip_f14 precip_3d precip_7d source_sdwa system_size population_served_count service_connections pop_served med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov50 month

drop if year >2019 | year <2006

collapse (mean) result , by(pwsid facility_id)
collapse (mean) result  , by(pwsid)

rename result detection_share_alt
replace pwsid = trim(pwsid)

********************************************************************************
* Raw SYR data 
********************************************************************************

preserve
import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_tcr.csv", varnames(1) clear 
replace pwsid = trim(pwsid)
egen tag = tag(pwsid)
tab tag
keep if tag == 1 
tempfile alt
save `alt'
restore
merge 1:1 pwsid using `alt'
drop tag 
drop _merge 

destring detection_share , replace ignore("NA")
replace detection_share = detection_share_alt if detection_share=. & detection_share_alt!=.

drop detection_share
rename detection_share_alt detection_share
drop tag 


export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_tcr_v2.csv", replace

