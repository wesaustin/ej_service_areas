


global path "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\Documents - pfas_perinatal_health\" 
global data "${path}data\" 
global output "${path}output\" 
global figures "${path}output\figures\" 
global plus "${data}packages\"
sysdir set PLUS "${plus}"
sysdir set PERSONAL "${plus}"
net install scheme-modern, from("https://raw.githubusercontent.com/mdroste/stata-scheme-modern/master/")
set scheme modern, perm 
graph set window fontface "Times New Roman"



* Issues 

- 86 duplicates observations, probably from H&M boundaries itself 
- 44,416 observations... seems like more than they reported generating

* Checking the new and the old values 

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\data\demographics\hm_dems_awinterpolate.csv", bindquote(strict)  varnames(1) clear

keep pwsid minorpct lowincpct lingisopct unemppct under5pct lesshspct over64pct med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov99 frac_pov199   
renvars minorpct lowincpct lingisopct unemppct under5pct lesshspct over64pct med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov99 frac_pov199   , postfix("_new")
egen tag = tag(pwsid )
tab tag 
keep if tag ==1 
drop tag
tempfile new 
save `new'

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\data\demographics\hm_dems_final.csv", bindquote(strict)  varnames(1) clear 

* 42,000 obs 
keep pwsid minorpct lowincpct lingisopct unemppct under5pct lesshspct over64pct med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov99 frac_pov199 pop_sum  
renvars minorpct lowincpct lingisopct unemppct under5pct lesshspct over64pct med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov99 frac_pov199 pop_sum  , postfix("_old")
egen tag = tag(pwsid )
tab tag 
keep if tag ==1 
drop tag

merge 1:1 pwsid using `new'  

foreach var in minorpct_old lowincpct_old lingisopct_old unemppct_old under5pct_old lesshspct_old over64pct_old med_inc_old frac_white_old frac_black_old frac_amerind_old frac_asian_old frac_pacisl_old frac_hisp_old frac_pov99_old frac_pov199_old pop_sum_old minorpct_new lowincpct_new lingisopct_new unemppct_new under5pct_new lesshspct_new over64pct_new med_inc_new frac_white_new frac_black_new frac_amerind_new frac_asian_new frac_pacisl_new frac_hisp_new frac_pov99_new frac_pov199_new{
	
	destring `var' , ignore("NA") replace
}


* data checks 
mdesc 
sum 


* scatter plots - second one is x axis 

xlabel("Old Variable") ylabel("New Variable") 

label var minorpct_old "Old Variable"
label var minorpct_new "New Variable"
scatter minorpct_old minorpct_new  , msize(tiny) title("Percent Minority")  

label var frac_amerind_old "Old Variable"
label var frac_amerind_new "New Variable"
scatter frac_amerind_old frac_amerind_new  , msize(tiny) title("Percent American Indian")  

label var frac_black_old "Old Variable"
label var frac_black_new "New Variable"
scatter frac_black_old frac_black_new  , msize(tiny) title("Percent Black")  



