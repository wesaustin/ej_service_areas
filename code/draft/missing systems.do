


*import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\data\demographics\SDWA_PUB_WATER_SYSTEMS.csv", clear 

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\sdwis\SDWA_PUB_WATER_SYSTEMS.csv", clear 
keep if pws_type_code == "CWS"
keep if pws_activity_code =="A"
keep pwsid pws_activity_code pws_type_code gw_sw_code owner_type_code primacy_type is_wholesaler_ind is_school_or_daycare_ind population_served_count

tempfile wholesales
save `wholesales'

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\data\demographics\epic_dems.csv" , clear
tempfile epic_dems
save `epic_dems'

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\data\demographics\hm_pws_list.csv", clear 
tempfile hm_dems
save `hm_dems'

use `wholesales' , clear
merge 1:1 pwsid using `hm_dems' , gen(m_missing)


* Are these just wholesalers?
tab m_missing if is_wholesaler_ind == "Y"
tab is_wholesaler_ind if m_missing == 1
tab m_missing if primacy_type == "Tribal"
	* only 350 of the missing systems are wholesalers 
	
* Is it just the tier 3 systems? 

tab tier if m_missing ==1

 /*      tier |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |        544        4.10        4.10
          2 |      1,049        7.90       11.99
          3 |      9,511       71.60       83.59
         NA |      2,180       16.41      100.00
------------+-----------------------------------
      Total |     13,284      100.00
*/

* Where are most of these coming from?

tab method

/*                method |      Freq.     Percent        Cum.
-----------------------+-----------------------------------
             1:1 Match |      8,859       24.81       24.81
    Mobile Homes - OSM |        295        0.83       25.64
Mobile Homes - Parcels |      1,214        3.40       29.04
         Random Forest |      4,191       11.74       40.77
          Secondary RF |      3,398        9.52       50.29
            State Data |     17,751       49.71      100.00
-----------------------+-----------------------------------
                 Total |     35,708      100.00 */


* Calculate total missing population 
egen total_tribal_pop =total(population_served_count) if m_missing ==1 & primacy_type == "Tribal"
egen total_pop = total(population_served_count) if m_missing ==1


egen ttl_pop_tier1 = total(population_served_count) if  m_missing ==1 & tier =="1"
br if  m_missing ==1 & tier =="1"