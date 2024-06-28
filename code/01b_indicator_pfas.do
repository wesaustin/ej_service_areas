
* Program to Create PFAS Drinking Water Indicator 
* National Center for Environmental Economics 
* Last edited: 8/28/23






********************************************************************************
* UCMR 5 Samples
********************************************************************************

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\UCMR5\UCMR5_All.txt", bindquote(strict) clear 

keep pwsid pwsname size collectiondate contaminant mrl units analyticalresultssign analyticalresultvalue sampleeventcode

* Keep just PFAS
* tab contaminant 
drop if contaminant == "lithium"

gen detection = (analyticalresultssign == "=")
gen detection_share = detection
gen pfas_count = detection 
gen concentration_sum = 0 
replace concentration_sum = analyticalresultvalue if detection ==1 
gen max = analyticalresultvalue

keep pwsid pwsname contaminant detection detection_share pfas_count concentration_sum max

tempfile ucmr5
save `ucmr5'



********************************************************************************
* UCMR 3 Samples 
********************************************************************************

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\ucmr\ucmr_3_main.csv", bindquote(strict) clear 

rename (pws_id pws_name analytical_results_sign analytical_results_value) (pwsid pwsname analyticalresultssign analyticalresultvalue)

* Keep just PFAS
tab contaminant
keep if contaminant =="PFBS" | contaminant == "PFHpA" | contaminant =="PFHxS" | contaminant == "PFNA" | contaminant =="PFOA" | contaminant == "PFOS"

gen detection = (analyticalresultssign == "=")
gen detection_share = detection
gen pfas_count = detection 
gen concentration_sum = 0 
replace concentration_sum = analyticalresultvalue if detection ==1 
gen max = analyticalresultvalue
keep pwsid pwsname contaminant detection detection_share pfas_count concentration_sum max


tempfile ucmr3
save `ucmr3'

********************************************************************************
* PFAS Analytic Tools Samples 
********************************************************************************

import excel "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\PFAS\PFAS_analytic_tools_allsamples.xlsx", sheet("Sheet1") firstrow case(lower) clear

gen one = 1
bys contaminant : egen ttl = total(one)
drop if ttl<1000

* Summarize information
egen group  = group(contaminant)
egen group_pws = group(pwsid)
sum group group_pws 


gen detection = (detected == "Yes")
gen detection_share = detection
gen pfas_count = detection
destring concentrationngl, replace ignore("-")
gen concentration_sum = 0 
replace concentration_sum = concentrationngl if detection ==1 
gen max = concentrationngl


keep pwsid pwsname contaminant detection detection_share pfas_count concentration_sum max

tempfile pat
save `pat'

********************************************************************************
* Combine Samples into one Panel 
********************************************************************************
 
append using `ucmr5'
append using `ucmr3'


* Summarize information
replace contaminant = lower(contaminant) /*capitalizations differ across datasets */
egen group  = group(contaminant)
egen group_pws = group(pwsid)
sum group group_pws 


********************************************************************************
* Collapse Samples to be PWSID Level
********************************************************************************

* First create PWSID-by-Contaminant Panel
	* Note that some of these variables are created by performing different types of collapses. 

gen one = 1
bys pwsid : egen total_samples = total(one)
collapse  	(mean) total_samples ///
			(sum) detection_share   ///
			(max) detection concentration_sum pfas_count max , ///
			by(pwsid contaminant) 

* Next collapse to PWSID-level  
collapse 	(mean) total_samples  ///
			(sum) detection_share pfas_count concentration_sum ///
			(max) detection max  ,	by(pwsid ) 
		

replace detection_share = 	detection_share	/ total_samples
drop if pwsid == ""
replace max = 0 if max == . 




********************************************************************************
* Save Indicator 
********************************************************************************

save "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicators_pfas.dta" , replace

export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicators_pfas.csv", replace



