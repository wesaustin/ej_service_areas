

********************************************************************************
* Georgia and Mississippi 
********************************************************************************

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\qlik\chem_Georgia.csv" , clear
tostring code, replace 
tempfile ga 
save `ga'
import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\qlik\chem_Mississippi.csv" , clear
append using `ga' , force
keep if code == "2450" | code =="2451" | code =="2452" | code == "2453" | code == "2454" | code =="2456" | code == "2941" | code == "2942" | code == "2943" | code == "2944" | code == "2950"
destring code, replace 
gen year = substr(sample_date,-4,.)
destring year, replace 
keep if year >2005 & year <2020

rename code analyte_code 
rename concentration result_value 
replace result_value = 0 if less_det == "Y" 
replace concentration_uom = trim(concentration_uom)
replace result_value = result_value * 1000 if  concentration_uom  =="MG/L"

levelsof analyte_code, local(codes)
foreach code in 2450 2451 2452 2453 2454 2456 2941 2942 2943 2944 2950 {
	bys pwsid analyte_code : egen mn_`code' = mean(result_value) if analyte_code == `code'
}

collapse (mean) mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2456 mn_2941 mn_2942 mn_2943 mn_2944 mn_2950 , by(pwsid)

sum mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2456 mn_2941 mn_2942 mn_2943 mn_2944 mn_2950
mdesc mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2456 mn_2941 mn_2942 mn_2943 mn_2944 mn_2950 

rename mn_2950 tthm 
rename mn_2456 haa5 

foreach var in 	mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2941 mn_2942 mn_2943 mn_2944{
	replace `var' = 0 if `var' ==. 
}
	
egen tthm_alt = rowtotal(mn_2941 mn_2942 mn_2943 mn_2944)
	replace tthm_alt = . if tthm_alt ==0 /*Turn missings back into missing*/
egen haa5_alt = rowtotal(mn_2450 mn_2451 mn_2452 mn_2453 mn_2454)
	replace haa5_alt = . if haa5_alt ==0
sum haa5 tthm tthm_alt haa5_alt

replace tthm = tthm_alt if tthm<5 & tthm_alt!=.
replace haa5 = haa5_alt if haa5<5 & haa5_alt!=. 
replace tthm = tthm_alt if tthm==.  & tthm_alt!=.
replace haa5 = haa5_alt if haa5==.  & haa5_alt!=.

gen combined_dbp  = tthm + haa5
replace combined_dbp = tthm if haa5==. 
replace combined_dbp = haa5 if tthm==. 

drop mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2941 mn_2942 mn_2943 mn_2944

tempfile states 
save `states'



********************************************************************************
* Six Year Review Data
********************************************************************************


import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\processed\syr_dbp.csv", clear 

gen state = substr(pwsid, 1, 2)
destring result_value, replace ignore("NA")
replace result_value = 0 if detect ==0 

* This is less than 35 observations
*replace result_value = result_value/1000 if result_value>1000 & result_value !=. 


* Need to combine concentration information for each analyte 

levelsof analyte_code, local(codes)
foreach code in 2450 2451 2452 2453 2454 2456 2941 2942 2943 2944 2950 {
	bys pwsid analyte_code : egen mn_`code' = mean(result_value) if analyte_code == `code'
}


collapse (mean) mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2456 mn_2941 mn_2942 mn_2943 mn_2944 mn_2950 , by(pwsid)

sum mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2456 mn_2941 mn_2942 mn_2943 mn_2944 mn_2950
mdesc mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2456 mn_2941 mn_2942 mn_2943 mn_2944 mn_2950 

/* 
2450 MONOCHLOROACETIC ACID
2451  DICHLOROACETIC ACID
2452  TRICHLOROACETIC ACID 
2453  MONOBROMOACETIC ACID
2454   DIBROMOACETIC ACID
2941  CHLOROFORM
2942  BROMOFORM
2943 BROMODICHLOROMETHANE 
2944  DIBROMOCHLOROMETHANE
*/

rename mn_2950 tthm 
rename mn_2456 haa5 


* Now need to generate a variable that's equal to the average sum of the acids and halomethanes. 
	* note first start with replacing missing as zeroes so the additions work, but these zeros won't be factored in later. 
foreach var in 	mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2941 mn_2942 mn_2943 mn_2944{
	replace `var' = 0 if `var' ==. 
}
	
egen tthm_alt = rowtotal(mn_2941 mn_2942 mn_2943 mn_2944)
	replace tthm_alt = . if tthm_alt ==0 /*Turn missings back into missing*/
egen haa5_alt = rowtotal(mn_2450 mn_2451 mn_2452 mn_2453 mn_2454)
	replace haa5_alt = . if haa5_alt ==0

sum haa5 tthm tthm_alt haa5_alt


replace tthm = tthm_alt if tthm<5 & tthm_alt!=.
replace haa5 = haa5_alt if haa5<5 & haa5_alt!=. 
replace tthm = tthm_alt if tthm==.  & tthm_alt!=.
replace haa5 = haa5_alt if haa5==.  & haa5_alt!=.

gen combined_dbp  = tthm + haa5
replace combined_dbp = tthm if haa5==. 
replace combined_dbp = haa5 if tthm==. 


drop mn_2450 mn_2451 mn_2452 mn_2453 mn_2454 mn_2941 mn_2942 mn_2943 mn_2944

append using `states'


export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_dbp_v2.csv", replace

