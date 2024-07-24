

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
replace result_value = result_value * 1000 if  concentration_uom  =="MG/L" /*converting all to ug/l*/
replace result_value = result_value /1000 if result_value >1000 & result_value !=. /*replace likely incorrect unit of measurement */


levelsof analyte_code, local(codes)
foreach code in 2450 2451 2452 2453 2454 2456 2941 2942 2943 2944 2950 {
	bys pwsid analyte_code : egen mn_`code' = mean(result_value) if analyte_code == `code'
	bys pwsid analyte_code : egen mn_`code'_syr3 = mean(result_value) if analyte_code == `code' & (year<2012)
	bys pwsid analyte_code : egen mn_`code'_syr4 = mean(result_value) if analyte_code == `code' & (year>2011)
	
}

collapse (mean) *mn* , by(pwsid)

sum *mn*
mdesc *mn*

* Separate out the TTHM and HAA5 samples 
rename mn_2950 tthm 
rename mn_2456 haa5 
rename  ( mn_2950_syr3 mn_2950_syr4 )  ( tthm_syr3 tthm_syr4 )
rename  ( mn_2456_syr3 mn_2456_syr4 )  ( haa5_syr3 haa5_syr4 )

* replace non-detects with zeros
foreach var in 	mn_2450 mn_2450_syr3 mn_2450_syr4 mn_2451 mn_2451_syr3 mn_2451_syr4 mn_2452 mn_2452_syr3 mn_2452_syr4 mn_2453 mn_2453_syr3 mn_2453_syr4 mn_2454 mn_2454_syr3 mn_2454_syr4 haa5 haa5_syr3 haa5_syr4 mn_2941 mn_2941_syr3 mn_2941_syr4 mn_2942 mn_2942_syr3 mn_2942_syr4 mn_2943 mn_2943_syr3 mn_2943_syr4 mn_2944 mn_2944_syr3 mn_2944_syr4 tthm tthm_syr3 tthm_syr4 {
	replace `var' = 0 if `var' ==. 
	
}
	
* generate tthm variables where only the constituent chemicals are listed 
egen tthm_alt = rowtotal(mn_2941 mn_2942 mn_2943 mn_2944)
	replace tthm_alt = . if tthm_alt ==0 /*Turn missings back into missing*/
foreach var in syr3 syr4{
	egen tthm_alt_`var' = rowtotal(mn_2941_`var' mn_2942_`var' mn_2943_`var' mn_2944_`var' )
	replace tthm_alt_`var' = . if tthm_alt_`var' ==0 /*Turn missings back into missing*/
}

	
egen haa5_alt = rowtotal(mn_2450 mn_2451 mn_2452 mn_2453 mn_2454)
	replace haa5_alt = . if haa5_alt ==0
foreach var in syr3 syr4{	
	egen haa5_alt_`var' = rowtotal(mn_2450_`var' mn_2451_`var' mn_2452_`var' mn_2453_`var' mn_2454_`var' )
	replace haa5_alt_`var' = . if haa5_alt_`var' ==0 /*Turn missings back into missing*/
}
	
sum haa5 tthm tthm_alt haa5_alt

* replacing these aggregated values with the new computed ones if they are missing or unrealistically small

foreach var in syr3 syr4{	
	replace tthm_`var' = tthm_alt_`var' if (tthm_`var' ==. | tthm_`var' <2.5)   & tthm_alt_`var' !=.
	replace haa5_`var' = haa5_alt_`var' if ( haa5_`var' ==. | haa5_`var' <2.5)  & haa5_alt_`var' !=.
}

gen combined_dbp  = tthm + haa5
replace combined_dbp = tthm if haa5==. 
replace combined_dbp = haa5 if tthm==. 

* Now SYR-specific version
foreach var in syr3 syr4{	
	gen combined_dbp_`var'  = tthm_`var' + haa5_`var'
	replace combined_dbp_`var' = tthm_`var' if haa5_`var'==. 
	replace combined_dbp_`var' = haa5_`var' if tthm_`var'==. 
}

keep pwsid haa5 haa5_syr3 haa5_syr4 tthm tthm_syr3 tthm_syr4 combined_dbp combined_dbp_syr3 combined_dbp_syr4

tempfile states 
save `states'



********************************************************************************
* Six Year Review Data
********************************************************************************


import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\processed\syr_dbp.csv", clear 

gen state = substr(pwsid, 1, 2)
destring result_value, replace ignore("NA")
replace result_value = 0 if detect ==0 
replace result_value = result_value /1000 if result_value >1000 & result_value !=. /*replace likely incorrect unit of measurement */


* This is less than 35 observations
*replace result_value = result_value/1000 if result_value>1000 & result_value !=. 


* Need to combine concentration information for each analyte 

* date formats are different in SYR3 and SYR4. Using this as a shorthand to separate samples in one vs. the other. 
gen len = strlen(sample_collection_date)

levelsof analyte_code, local(codes)
foreach code in 2450 2451 2452 2453 2454 2456 2941 2942 2943 2944 2950 {
	bys pwsid analyte_code : egen mn_`code' = mean(result_value) if analyte_code == `code'
	bys pwsid analyte_code : egen mn_`code'_syr3 = mean(result_value) if analyte_code == `code' & len ==23
	bys pwsid analyte_code : egen mn_`code'_syr4 = mean(result_value) if analyte_code == `code' & len == 9 
	
}


collapse (mean) *mn* , by(pwsid)

sum *mn*
mdesc *mn*


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

* Separate out the TTHM and HAA5 samples 
rename mn_2950 tthm 
rename mn_2456 haa5 
rename  ( mn_2950_syr3 mn_2950_syr4 )  ( tthm_syr3 tthm_syr4 )
rename  ( mn_2456_syr3 mn_2456_syr4 )  ( haa5_syr3 haa5_syr4 )


* Now need to generate a variable that's equal to the average sum of the acids and halomethanes. 
	* note first start with replacing missing as zeroes so the additions work, but these zeros won't be factored in later. 
foreach var in 	mn_2450 mn_2450_syr3 mn_2450_syr4 mn_2451 mn_2451_syr3 mn_2451_syr4 mn_2452 mn_2452_syr3 mn_2452_syr4 mn_2453 mn_2453_syr3 mn_2453_syr4 mn_2454 mn_2454_syr3 mn_2454_syr4 haa5 haa5_syr3 haa5_syr4 mn_2941 mn_2941_syr3 mn_2941_syr4 mn_2942 mn_2942_syr3 mn_2942_syr4 mn_2943 mn_2943_syr3 mn_2943_syr4 mn_2944 mn_2944_syr3 mn_2944_syr4 tthm tthm_syr3 tthm_syr4 {
	replace `var' = 0 if `var' ==. 
}
	
* generate tthm variables where only the constituent chemicals are listed 
egen tthm_alt = rowtotal(mn_2941 mn_2942 mn_2943 mn_2944)
	replace tthm_alt = . if tthm_alt ==0 /*Turn missings back into missing*/
foreach var in syr3 syr4{
	egen tthm_alt_`var' = rowtotal(mn_2941_`var' mn_2942_`var' mn_2943_`var' mn_2944_`var' )
	replace tthm_alt_`var' = . if tthm_alt_`var' ==0 /*Turn missings back into missing*/
}
	
egen haa5_alt = rowtotal(mn_2450 mn_2451 mn_2452 mn_2453 mn_2454)
	replace haa5_alt = . if haa5_alt ==0
foreach var in syr3 syr4{	
	egen haa5_alt_`var' = rowtotal(mn_2450_`var' mn_2451_`var' mn_2452_`var' mn_2453_`var' mn_2454_`var' )
	replace haa5_alt_`var' = . if haa5_alt_`var' ==0 /*Turn missings back into missing*/
}
	
sum haa5 tthm tthm_alt haa5_alt

* replacing these aggregated values with the new computed ones if they are missing or unrealistically small

foreach var in syr3 syr4{	
	replace tthm_`var' = tthm_alt_`var' if (tthm_`var' ==. | tthm_`var' <2.5)   & tthm_alt_`var' !=.
	replace haa5_`var' = haa5_alt_`var' if ( haa5_`var' ==. | haa5_`var' <2.5)  & haa5_alt_`var' !=.
}

gen combined_dbp  = tthm + haa5
replace combined_dbp = tthm if haa5==. 
replace combined_dbp = haa5 if tthm==. 

* Now SYR-specific version
foreach var in syr3 syr4{	
	gen combined_dbp_`var'  = tthm_`var' + haa5_`var'
	replace combined_dbp_`var' = tthm_`var' if haa5_`var'==. 
	replace combined_dbp_`var' = haa5_`var' if tthm_`var'==. 
}



keep pwsid haa5 haa5_syr3 haa5_syr4 tthm tthm_syr3 tthm_syr4 combined_dbp combined_dbp_syr3 combined_dbp_syr4

append using `states'
duplicates drop 

export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_dbp_v3.csv", replace

