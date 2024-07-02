********************************************************************************
* Program to Compare the Demographic Percentages Across each Boundary 
* NCEE
* last edited 7/1/24
********************************************************************************

* NOTES: This program needs to be run in its entirety (i.e., you can't just run a subset of the code and expect it to run).


global path "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\"


********************************************************************************
* Start with EPA ORD  boundaries as a point of comparison
********************************************************************************

import delimited "${path}data\risk_by_pws\risk_hm_hb.csv" , clear
* keep variable subset and rename to clearly identify EPA ORD boundaries
keep pwsid minorpct lowinc med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov99 frac_pov199 whitepct minor_served black_served amer_ind_served asian_served pac_isl_served hispanic_served nhw_served
destring frac_pov199, replace force 
renvars minorpct lowinc med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov99 frac_pov199 whitepct minor_served black_served amer_ind_served asian_served pac_isl_served hispanic_served nhw_served , postfix("_hm")
egen tag = tag(pwsid)
keep if tag ==1 

********************************************************************************
* Import each of the other boundaries and create a temporary file 
********************************************************************************

preserve 
foreach var in county usgs zc epic{
	import delimited "${path}data\risk_by_pws\risk_`var'_hb.csv" , clear
keep pwsid minorpct lowinc med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov99 frac_pov199 whitepct minor_served black_served amer_ind_served asian_served pac_isl_served hispanic_served nhw_served
destring frac_pov199, replace force 

renvars minorpct lowinc med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov99 frac_pov199 whitepct minor_served black_served amer_ind_served asian_served pac_isl_served hispanic_served nhw_served , postfix("_`var'")
egen tag = tag(pwsid)
keep if tag ==1 
drop tag
tempfile f`var'
save `f`var''
}
restore


********************************************************************************
* Scatter plots 
********************************************************************************

foreach dem in minorpct frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov199 {
	
	foreach var in  county usgs zc epic{
	preserve
		if `"`var'"'=="county"  {
			local i "County"
		}
		if `"`var'"'=="usgs"  {
			local i "USGS"
		}
		if `"`var'"'=="zc"  {
			local i "Zipcodes"
		}
		if `"`var'"'=="epic"  {
			local i "EPIC"
		}
		
		
		if `"`dem'"'=="minorpct"  {
			local j "Percent Minority"
		}
		if `"`dem'"'=="frac_white"  {
			local j "Percent White"
		}
		if `"`dem'"'=="frac_black"  {
			local j "Percent Black"
		}
		if `"`dem'"'=="frac_amerind"  {
			local j "Percent American Indian"
		}
		if `"`dem'"'=="frac_asian"  {
			local j "Percent Asian"
		}
		if `"`dem'"'=="frac_pacisl"  {
			local j "Percent Pacific Islander"
		}
		if `"`dem'"'=="frac_hisp"  {
			local j "Percent Hispanic"
		}
		if `"`dem'"'=="frac_pov199"  {
			local j "Percent Low Income"
		}
		
		if `"`dem'"'=="minorpct" | `"`dem'"'=="frac_white" | `"`dem'"'=="frac_black" | `"`dem'"'=="frac_pov199" {
			local k "vtiny"
		}
		if `"`dem'"'=="frac_black" | `"`dem'"'=="frac_amerind" | `"`dem'"'=="frac_asian" | `"`dem'"'=="frac_pacisl" | `"`dem'"'=="frac_hisp" {
			local k "tiny"
		}


	merge 1:1 pwsid using `f`var''
	label var `dem'_hm "`j' - EPA ORD"
	label var `dem'_`var' "`j' - `i'"
	scatter  `dem'_`var' `dem'_hm , msize(`k') name(gr_`var', replace) 
	restore
	}
	
	gr combine gr_county gr_zc gr_usgs gr_epic 
	graph export "${path}output\scatter\scatter_`dem'.png" , as(png) replace 
	
}




	