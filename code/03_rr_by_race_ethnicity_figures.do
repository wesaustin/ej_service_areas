********************************************************************************
********************************************************************************
* Creating Figures for EJ & Service Areas PAper 
* National Center for Environmental Economics 
* Last Edited: 6/28/2024
********************************************************************************
********************************************************************************


********************************************************************************
* I. Initialize Program - Set Paths and Install Packages 
********************************************************************************

global path "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\" 
global plus "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\code\packages\"
global output "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\output\"
sysdir set PLUS "${plus}"
sysdir set PERSONAL "${plus}"
net install scheme-modern, from("https://raw.githubusercontent.com/mdroste/stata-scheme-modern/master/")
set scheme modern, perm 
graph set window fontface "Times New Roman"

* ssc install colorpallette
grstyle init
grstyle set color hue, n(5) opacity(75)

	
********************************************************************************
* Histogram of relative risks by demographic group 
********************************************************************************

import delimited "${path}ej_service_areas\data\rel_risk\rel_risk_all_subpop.csv" , clear 	

replace boundary = "EPA ORD" if boundary == "hm"
replace boundary = "EPIC" if boundary == "epic"
replace boundary = "USGS" if boundary == "usgs"
replace boundary = "County" if boundary == "county"
replace boundary = "Zipcode" if boundary == "zc"

replace race_cat = "American Indian" if race_cat == "amer_ind"
replace race_cat = "Asian" if race_cat == "asian"
replace race_cat = "Black" if race_cat == "black"
replace race_cat = "Hispanic" if race_cat == "hisp"
replace race_cat = "Pacific Islander" if race_cat == "pac_isl"
replace race_cat = "People of Color" if race_cat == "poc_risk"
cap replace race_cat = "Low-Income" if race_cat == "income"

gen border = 1 if boundary == "County"
replace border = 2 if boundary == "Zipcode"
replace border = 3 if boundary == "USGS"
replace border = 4 if boundary == "EPIC"
replace border = 5 if boundary == "EPA ORD"
	
foreach var in hb_rel_risk lcr_rel_risk pfas_rel_risk dbp_rel_risk tcr_rel_risk{
	if `"`var'"'=="hb_rel_risk"  {
		local i "Health-based SDWA Violations"
	}
	if `"`var'"'=="lcr_rel_risk"  {
		local i "Lead Action Level Exceedances"
	}
	if `"`var'"'=="pfas_rel_risk"  {
		local i "Unique PFAS Detected"
	}
	if `"`var'"'=="dbp_rel_risk"  {
		local i "Average DBP Concentration"
	}
	if `"`var'"'=="tcr_rel_risk"  {
		local i "Total Coliform Detection Share"
	}
	graph bar `var' , over(border) over(race_cat) asyvars name(overtime, replace) ///
	ytitle("Relative Risk") yline(1) ///
	title("`i' by Race and Ethnicity") ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_`var'.png" , as(png) replace 
}
	
********************************************************************************
* Simple version of basic comparisons across indicators 
********************************************************************************

import delimited "${path}ej_service_areas\data\rel_risk\rel_risk_all.csv", clear 

drop v1
replace boundary = "EPA ORD" if boundary == "hm"
replace boundary = "EPIC" if boundary == "epic"
replace boundary = "USGS" if boundary == "usgs"
replace boundary = "County" if boundary == "county"
replace boundary = "Zipcode" if boundary == "zc"
replace risk_type = "Race and Ethnicity" if risk_type == "race"
replace risk_type = "Income" if risk_type == "income"
label var hb_rel_risk "Health-based Violations"
label var lcr_rel_risk "Lead ALEs"
label var pfas_rel_risk "PFAS"
label var dbp_rel_risk "DBPs"
label var tcr_rel_risk "Total Coliform"

gen border = 1 if boundary == "County"
replace border = 2 if boundary == "Zipcode"
replace border = 3 if boundary == "USGS"
replace border = 4 if boundary == "EPIC"
replace border = 5 if boundary == "EPA ORD"


foreach var in hb_rel_risk lcr_rel_risk pfas_rel_risk dbp_rel_risk tcr_rel_risk{
	if `"`var'"'=="hb_rel_risk"  {
		local i "Health-based SDWA Violations"
	}
	if `"`var'"'=="lcr_rel_risk"  {
		local i "Lead Action Level Exceedances"
	}
	if `"`var'"'=="pfas_rel_risk"  {
		local i "Unique PFAS Detected"
	}
	if `"`var'"'=="dbp_rel_risk"  {
		local i "Average DBP Concentration"
	}
	if `"`var'"'=="tcr_rel_risk"  {
		local i "Total Coliform Detection Share"
	}
	graph bar `var' , over(risk_type) over(boundary, sort(border))  asyvars name(overtime, replace) ///
	ytitle("Relative Risk") yline(1) ///
	title("`i' by Service Area Boundary Type") ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_sab_`var'.png" , as(png) replace 
}

/* Dropping these results 

	graph bar hb_rel_risk lcr_rel_risk pfas_rel_risk dbp_rel_risk tcr_rel_risk if risk_type == "Income",  over(boundary) asyvars name(overtime, replace) ///
	ytitle("Risk Relative to 2X Federal Poverty Limit") yline(1) ///
	title("Relative Risk by Income for all Indicators") legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_income_allrrs.png" , as(png) replace 

	graph bar hb_rel_risk lcr_rel_risk pfas_rel_risk dbp_rel_risk tcr_rel_risk if risk_type == "Race and Ethnicity",  over(boundary) asyvars name(overtime, replace) ///
	ytitle("Risk Relative to non-Hispanic White") yline(1) ///
	title("Relative Risk by Race and Ethnicity for all Indicators") legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_raceeth_allrrs.png" , as(png) replace 
*/

********************************************************************************
* Key Results - Basic Comparisons across Indicators 
********************************************************************************

import delimited "${path}ej_service_areas\data\rel_risk\rel_risk_all.csv", clear 

drop v1
replace boundary = "EPA ORD" if boundary == "hm"
replace boundary = "EPIC" if boundary == "epic"
replace boundary = "USGS" if boundary == "usgs"
replace boundary = "County" if boundary == "county"
replace boundary = "Zipcode" if boundary == "zc"
replace risk_type = "Race and Ethnicity" if risk_type == "race"
replace risk_type = "Income" if risk_type == "income"
label var hb_rel_risk "Health-based Violations"
label var lcr_rel_risk "Lead ALEs"
label var pfas_rel_risk "PFAS"
label var dbp_rel_risk "DBPs"
label var tcr_rel_risk "Total Coliform"

rename (hb_rel_risk lcr_rel_risk pfas_rel_risk dbp_rel_risk tcr_rel_risk) (rr_hb rr_lcr rr_pfas rr_dbp rr_tcr)

reshape long rr_ , ///
		i(risk_type boundary) j(indicator) string
rename rr_ rel_risk


replace indicator =  "Health-based Violations" if indicator == "hb"
replace indicator =  "Lead ALEs" if indicator == "lcr"
replace indicator =  "PFAS" if indicator == "pfas"
replace indicator =  "DBPs" if indicator == "dbp"
replace indicator =  "Total Coliform" if indicator == "tcr"

gen border = 1 if boundary == "County"
replace border = 2 if boundary == "Zipcode"
replace border = 3 if boundary == "USGS"
replace border = 4 if boundary == "EPIC"
replace border = 5 if boundary == "EPA ORD"

gen iorder = 1 if indicator == "Health-based Violations"
replace iorder = 2 if indicator == "Lead ALEs"
replace iorder = 3 if indicator == "DBPs"
replace iorder = 4 if indicator ==  "PFAS" 
replace iorder = 5 if indicator == "Total Coliform"

	graph bar rel_risk if risk_type == "Income",  over(border)  over(indicator, sort(iorder)) asyvars name(overtime, replace) ///
	ytitle("Relative Risk Ratio") yline(1) ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	*	title("Relative Risk by Income for all Indicators") ///
	graph export "${output}bar_income_allrrs_final.png" , as(png) replace 

	graph bar rel_risk if risk_type == "Race and Ethnicity",  over(border)  over(indicator, sort(iorder)) asyvars name(overtime, replace)  ///
	ytitle("Relative Risk Ratio") yline(1) ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	*title("Relative Risk by Race and Ethnicity for all Indicators") ///
	graph export "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\output\bar_raceeth_allrrs_final.png" , as(png) replace 
	
	
	
	
********************************************************************************
* Histogram of relative risks by demographic group 
********************************************************************************

*import delimited "${path}ej_service_areas\data\rel_risk_by_race_all_v2.csv" , clear 	
import delimited "${path}ej_service_areas\data\rel_risk_all_subpop.csv", clear 

replace boundary = "EPA ORD" if boundary == "hm"
replace boundary = "EPIC" if boundary == "epic"
replace boundary = "USGS" if boundary == "usgs"
replace boundary = "County" if boundary == "county"
replace boundary = "Zipcode" if boundary == "zc"

replace race_cat = "American Indian" if race_cat == "amer_ind"
replace race_cat = "Asian" if race_cat == "asian"
replace race_cat = "Black" if race_cat == "black"
replace race_cat = "Hispanic" if race_cat == "hisp"
replace race_cat = "Pacific Islander" if race_cat == "pac_isl"
replace race_cat = "People of Color" if race_cat == "poc_risk"
cap replace race_cat = "Low-Income" if race_cat == "lowinc"
drop if race_cat == "People of Color"

gen border = 1 if boundary == "County"
replace border = 2 if boundary == "Zipcode"
replace border = 3 if boundary == "USGS"
replace border = 4 if boundary == "EPIC"
replace border = 5 if boundary == "EPA ORD"

gen rorder =1 if race_cat  == "American Indian" 
replace rorder =2 if  race_cat == "Asian" 
replace rorder =3 if  race_cat == "Black" 
replace rorder =4 if  race_cat == "Hispanic" 
replace rorder =5 if  race_cat == "Pacific Islander" 
replace rorder =6 if  race_cat == "Low-Income" 
	
foreach var in hb_rel_risk lcr_rel_risk pfas_rel_risk dbp_rel_risk tcr_rel_risk ars_rel_risk nitrate_rel_risk{
	if `"`var'"'=="hb_rel_risk"  {
		local i "Health-based SDWA Violations"
	}
	if `"`var'"'=="lcr_rel_risk"  {
		local i "Lead Action Level Exceedances"
	}
	if `"`var'"'=="pfas_rel_risk"  {
		local i "Unique PFAS Detected"
	}
	if `"`var'"'=="dbp_rel_risk"  {
		local i "Average DBP Concentration"
	}
	if `"`var'"'=="tcr_rel_risk"  {
		local i "Total Coliform Detection Share"
	}
	if `"`var'"'=="ars_rel_risk"  {
		local i "Arsenic Concentration (mg/l)"
	}	
	if `"`var'"'=="nitrate_rel_risk"  {
		local i "Nitrate Concentration (mg/l)"
	}
	graph bar `var' , over(border) over(race_cat, sort(rorder)) asyvars name(overtime, replace) ///
	ytitle("Relative Risk Ratio") yline(1) ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_`var'_final.png" , as(png) replace
		*title("`i' by Race and Ethnicity") ///

}
		 
		 
		 
		 
		 
		 


********************************************************************************
* Histogram of relative risks by demographic group 
********************************************************************************

*import delimited "${path}ej_service_areas\data\rel_risk_by_race_all_v2.csv" , clear 	
import delimited "${path}data\rel_risk\rel_risk_pfas_sensitivity.csv", clear 

replace boundary = "EPA ORD" if boundary == "hm"
replace boundary = "EPIC" if boundary == "epic"
replace boundary = "USGS" if boundary == "usgs"
replace boundary = "County" if boundary == "county"
replace boundary = "Zipcode" if boundary == "zc"

replace race_cat = "American Indian" if race_cat == "amerind"
replace race_cat = "Asian" if race_cat == "asian"
replace race_cat = "Black" if race_cat == "black"
replace race_cat = "Hispanic" if race_cat == "hisp"
replace race_cat = "Pacific Islander" if race_cat == "pacisl"
replace race_cat = "People of Color" if race_cat == "poc"
cap replace race_cat = "Low-Income" if race_cat == "lowinc"
drop if race_cat == "People of Color"

gen border = 1 if boundary == "County"
replace border = 2 if boundary == "Zipcode"
replace border = 3 if boundary == "USGS"
replace border = 4 if boundary == "EPIC"
replace border = 5 if boundary == "EPA ORD"

gen rorder =1 if race_cat  == "American Indian" 
replace rorder =2 if  race_cat == "Asian" 
replace rorder =3 if  race_cat == "Black" 
replace rorder =4 if  race_cat == "Hispanic" 
replace rorder =5 if  race_cat == "Pacific Islander" 
replace rorder =6 if  race_cat == "Low-Income" 
	
	
	
foreach var in total_samples_rel_risk detection_share_rel_risk concentration_max_rel_risk concentration_avg_rel_risk pfas_count_rel_risk	{
	if `"`var'"'=="total_samples_rel_risk"  {
		local i "Total Samples Taken"
	}
	if `"`var'"'=="detection_share_rel_risk"  {
		local i "Detection Share"
	}
	if `"`var'"'=="concentration_max_rel_risk"  {
		local i "Sum of Maximum PFAS Concentrations"
	}
	if `"`var'"'=="concentration_avg_rel_risk"  {
		local i "Sum of PFAS-Specific Average Concentrations"
	}
	if `"`var'"'=="pfas_count_rel_risk"  {
		local i "Unique PFAS Count"
	}

	graph bar `var' , over(border) over(race_cat, sort(rorder)) asyvars name( `var' , replace) ///
	ytitle("Prevalence Ratio Ratio") yline(1) ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_`var'_pfas.png" , as(png) replace

}







********************************************************************************
* Histogram of relative risks by demographic group - DBPs
********************************************************************************


* note dem_cat = race_cat elsewhere

*import delimited "${path}ej_service_areas\data\rel_risk_by_race_all_v2.csv" , clear 	
import delimited "${path}data\rel_risk\rel_risk_dbp_sensitivity.csv", clear 

replace boundary = "EPA ORD" if boundary == "hm"
replace boundary = "EPIC" if boundary == "epic"
replace boundary = "USGS" if boundary == "usgs"
replace boundary = "County" if boundary == "county"
replace boundary = "Zipcode" if boundary == "zc"

replace dem_cat = "American Indian" if dem_cat == "amerind"
replace dem_cat = "Asian" if dem_cat == "asian"
replace dem_cat = "Black" if dem_cat == "black"
replace dem_cat = "Hispanic" if dem_cat == "hisp"
replace dem_cat = "Pacific Islander" if dem_cat == "pacisl"
replace dem_cat = "People of Color" if dem_cat == "poc"
cap replace dem_cat = "Low-Income" if dem_cat == "lowinc"
drop if dem_cat == "People of Color"

gen border = 1 if boundary == "County"
replace border = 2 if boundary == "Zipcode"
replace border = 3 if boundary == "USGS"
replace border = 4 if boundary == "EPIC"
replace border = 5 if boundary == "EPA ORD"

gen rorder =1 if dem_cat  == "American Indian" 
replace rorder =2 if  dem_cat == "Asian" 
replace rorder =3 if  dem_cat == "Black" 
replace rorder =4 if  dem_cat == "Hispanic" 
replace rorder =5 if  dem_cat == "Pacific Islander" 
replace rorder =6 if  dem_cat == "Low-Income" 
	
haa5_rel_risk haa5_syr3_rel_risk haa5_syr4_rel_risk 
tthm_rel_risk tthm_syr3_rel_risk tthm_syr4_rel_risk 
combined_dbp_rel_risk combined_dbp_syr3_rel_risk combined_dbp_syr4_rel_risk

* First check if these look similar across SYR 3 and SYR 4

foreach var in combined_dbp_rel_risk combined_dbp_syr3_rel_risk combined_dbp_syr4_rel_risk	{
	if `"`var'"'=="combined_dbp_rel_risk"  {
		local i "Combined DBP Measure - All Time"
	}
	if `"`var'"'=="combined_dbp_syr3_rel_risk"  {
		local i "Combined DBP Measure - SYR3"
	}
	if `"`var'"'=="combined_dbp_syr4_rel_risk"  {
		local i "Combined DBP Measure - SYR4"
	}

	graph bar `var' , over(border) over(dem_cat, sort(rorder)) asyvars name( `var' , replace) ///
	ytitle("Prevalence Ratio Ratio") yline(1) ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_`var'_dbp.png" , as(png) replace

}


foreach var in tthm_rel_risk tthm_syr3_rel_risk tthm_syr4_rel_risk 	{
	if `"`var'"'=="tthm_rel_risk"  {
		local i "TTHM - All Time"
	}
	if `"`var'"'=="tthm_syr3_rel_risk"  {
		local i "TTHM - SYR3"
	}
	if `"`var'"'=="tthm_syr4_rel_risk"  {
		local i "TTHM - SYR4"
	}

	graph bar `var' , over(border) over(dem_cat, sort(rorder)) asyvars name( `var' , replace) ///
	ytitle("Prevalence Ratio") yline(1) ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_`var'_dbp.png" , as(png) replace

}


foreach var in haa5_rel_risk haa5_syr3_rel_risk haa5_syr4_rel_risk 	{
	if `"`var'"'=="haa5_rel_risk"  {
		local i "HAA5 - All Time"
	}
	if `"`var'"'=="haa5_syr3_rel_risk"  {
		local i "HAA5 - SYR3"
	}
	if `"`var'"'=="haa5_syr4_rel_risk"  {
		local i "HAA5 - SYR4"
	}

	graph bar `var' , over(border) over(dem_cat, sort(rorder)) asyvars name( `var' , replace) ///
	ytitle("Prevalence Ratio") yline(1) ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_`var'_dbp.png" , as(png) replace

}