* Program to Create PFAS Drinking Water Indicator 
* National Center for Environmental Economics 
* Last edited: 7/24/24


********************************************************************************
* Set Paths and Install Packages 
********************************************************************************

* Main filepaths 
global path "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\Documents - pfas_perinatal_health\" 
global data "${path}data\" 
global output "${path}output\" 

********************************************************************************
* UCMR 3
********************************************************************************

import delimited "${data}ucmr_3_main.csv" , clear 

* varlist:  size facility_id facility_name facility_water_type sample_point_id sample_point_name sample_point_type associated_facility_id associated_sample_point_id collection_date sample_id contaminant mrl method_id analytical_results_sign analytical_results_value sample_event_code monitoring_requirement region state

replace contaminant = upper(contaminant)
keep if contaminant == "PFBS" | contaminant == "PFHPA" | contaminant == "PFHXS" | contaminant == "PFNA" | contaminant == "PFOA" | contaminant == "PFOS"

rename 	(pws_id pws_name facility_id facility_name facility_water_type sample_point_id ///
		sample_point_name sample_point_type associated_facility_id associated_sample_point_id ///
		collection_date method_id analytical_results_sign analytical_results_value sample_event_code ///
		monitoring_requirement sample_id ) ///
		(pwsid pwsname facilityid facilityname facilitywatertype samplepointid ///
		samplepointname samplepointtype associatedfacilityid associatedsamplepointid ///
		collectiondate methodid analyticalresultssign analyticalresultvalue sampleeventcode ///
		monitoringrequirement sampleid)

	* Units are missing, but these are all reported in ug/l based on the MRLs. Need to convert to ng/l or ppt 
	replace mrl = mrl*1000
	replace analyticalresultvalue = analyticalresultvalue * 1000		
	gen units = "NG/L"		

	* Make variables the same type as in UCMR 5
	foreach var in facilityid associatedfacilityid{
		cap tostring `var', replace 
	}

	* Clean Date Variables
	gen date = date(collectiondate, "YMD")
	format date %td
	format %tdNN/DD/CCYY date
	generate date_text2 = string(date, "%tdNN/DD/CCYY")
	drop date collectiondate
	rename date_text2 date
	
	gen ucmr3_system = 1

tempfile ucmr3 
save `ucmr3'	
		
********************************************************************************		
* UCMR 5
********************************************************************************

*import delimited "${data}UCMR5_All.txt" , bindquote(strict) clear
import delimited "${data}ucmr5-occurrence-data\UCMR5_All.txt" , bindquote(strict) clear

* varlist: pwsid pwsname size facilityid facilityname facilitywatertype samplepointid samplepointname samplepointtype associatedfacilityid associatedsamplepointid collectiondate sampleid contaminant mrl units methodid analyticalresultssign analyticalresultvalue sampleeventcode monitoringrequirement region state ucmr1sampletype

	* Some quick cleaning steps 
	drop ucmr1sampletype
	drop if contaminant == "lithium"

	* Make variables the same type as in UCMR 5
	foreach var in associatedfacilityid associatedsamplepointid{
		cap tostring `var', replace 
	}
	
	* MRLs are  reported in ug/l. Let's switch these to ng/l or ppt 
	replace mrl = mrl*1000
	replace analyticalresultvalue = analyticalresultvalue * 1000 
	replace units = "NG/L" if units =="µg/L"
	
	* Clean Date Variables
	gen date = date(collectiondate, "MDY")
	format date %td
	format %tdNN/DD/CCYY date
	generate date_text2 = string(date, "%tdNN/DD/CCYY")
	drop date collectiondate
	rename date_text2 date

append using `ucmr3'

	* make variable names more like PFAS analytic tools 
	rename (analyticalresultvalue) (concentration)
	gen detect = 0 if analyticalresultssign =="Y"
	replace detect = 1 if analyticalresultssign == "="
	
tempfile ucmrs 	
save `ucmrs'

********************************************************************************
* Maine Data
********************************************************************************


import excel "${data}PFAS_Samples_Results_qry.xlsx", sheet("PFAS_Samples_Results_qry") firstrow clear
renvars *, lower
gen state = "ME"

tostring detectn_limit_num , replace 
gen detectionlimit = detectn_limit_num + " " + detectn_lim_uom_cd
drop detectn_limit_num detectn_lim_uom_cd
rename (pws_type systemname facility collectiondate analytecode analytename lessthanindicator concentrationuom ) ///
		(fed_type pwsname samplepointid  date code contaminant detect units )
		
* Clean dates
	format date %td
	format %tdNN/DD/CCYY date
	generate date_text2 = string(date, "%tdNN/DD/CCYY")
	drop date 
	rename date_text2 date

* Clean detection variable	
drop detect 
gen detect =1 if concentration >0 & concentration!=. 
replace detect = 0 if concentration == 0 

* One error in units
replace units = "NG/L" if units == "MG/L"

tempfile maine
save `maine'


********************************************************************************
* PFAS Analytic Tools
********************************************************************************

use "${data}pfas_analytic_tools_allsamples.dta" , clear 

* varlist:  state county populationserved size sampleid samplepointid code contaminant detectionmethod detected concentration units concentrationngl detectionlimit healthadvisorylevelngl sampledate sampletype mrlmdl reporting samples

	* Standardize select variables to be more like the UCMR records
	drop healthadvisorylevelngl reporting samples /*unnecessary variables */
	rename (detected detectionmethod) (detect methodid)
	replace detect = "1" if detect =="Yes"
	replace detect = "0" if detect == "No"
	replace detect = "0" if detect == "Unknown"
	destring detect, replace 

	
	* Transition to all ng/l or PPT 
	foreach var in concentration concentrationngl mrlmdl{
		cap replace `var' = "" if `var' =="-"
		destring `var' , replace 
	}
	drop concentration
	rename concentrationngl concentration
	replace units = "NG/L" /*there appears to have been one typo in filling these out */
	
	gen check = 1 if concentration>0 & concentration !=. & detect == 0 

	* Clean Date Variables
	gen date = date(sampledate, "MDY")
	format date %td
	format %tdNN/DD/CCYY date
	generate date_text2 = string(date, "%tdNN/DD/CCYY")
	drop date sampledate
	rename date_text2 date

append using `ucmrs' 
append using `maine'

********************************************************************************
* Miscellaneous Cleaning Steps 
********************************************************************************

	* check which variables are mostly missing
	mdesc 
	drop if pwsid == ""
	drop if date == "."
	
	* Fill in detect variables if it's clear what they should be 
	replace detect = 1 if concentration !=.
	replace detect = 0 if concentration == .
	
	* Fill in Concentrations with zero if not detected
	replace concentration = 0 if detect == 0 & concentration ==. 

		* first fill in missing codes
	replace contaminant = upper(contaminant)
	bys contaminant : egen mode= mode(code) , minmode 
	replace code = mode if code ==""
		* then replace names with codes where codes are available 
	drop mode
	bys code: egen mode = mode(contaminant)
	replace contaminant = mode if code !=""
			
	* drop unnecessary variables 	
		
	drop pwsname county populationserved size methodid detectionlimit sampletype mrlmdl facilityid facilityname facilitywatertype samplepointname samplepointtype associatedfacilityid associatedsamplepointid mrl analyticalresultssign sampleeventcode monitoringrequirement region ucmr3_system fed_type n	
	
	* drop duplicate 
	drop if contaminant == "TOTAL PFOA AND PFOS"
		
	* Drop obscure PFAS that are rarely sampled and that would bias results towards systems that sample more
	gen one = 1
	bys contaminant:  egen ttl = total(one)
	drop if ttl <500 /*700 obs deleted*/
	drop ttl one
	
	* Drop likely duplicate samples - sample IDs can also now be dropped 
	egen tag = tag(pwsid date  concentration contaminant detect)
	tab tag
	drop if tag == 0 
	drop tag
	/*  detect) |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |    642,451       40.18       40.18
          1 |    956,570       59.82      100.00
------------+----------------------------------- */	
	
	* variables no longer necessary 
	drop sampleid samplepointid mode state units date

	
* Generate PFAS measures 
gen one = 1
bys pwsid : egen total_samples = total(one)
bys pwsid contaminant : egen  concentration_avg  = mean(concentration )
bys pwsid contaminant : egen  concentration_max  = max(concentration ) 
bys pwsid : egen detection_share = mean(detect)
bys pwsid contaminant : egen  pfas_count  = max(detect ) 

* first collapse to pwsid-by-contaminant level 
collapse  	(mean) total_samples concentration_avg concentration_max detection_share pfas_count , ///
			by(pwsid contaminant) 

* Next we need to sum the average or maximum concentrations by contaminant while collapsing to PWSID-level  
collapse 	(mean) total_samples detection_share ///
			(sum) concentration_avg concentration_max pfas_count  ,	by(pwsid ) 
		

drop if pwsid == ""

	

********************************************************************************
* Save Indicator 
********************************************************************************

save "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicators_pfas_v2.dta" , replace

export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicators_pfas_v2.csv", replace



	
	