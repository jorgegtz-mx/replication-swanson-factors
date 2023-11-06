//Swanson a la Mexicana
//October 24th 2023

clear 

cd "/Users/jagtz/Library/CloudStorage/OneDrive-Personal/Libreta/1. Monetary Policy Shocks/"

import delimited "data/swanson a la mexicana (irregular frequency).csv", clear
save "data/irregular freq series.dta"

clear


use "data/irregular freq series.dta"

gen t = _n
tsset t

//I cannot use date2 to set time series data because date intervals are not constant
gen date2 = date(date, "YMD")
format date2 %td

//I. Check that depvars and indepvars are covariance stationary

//Verify that the dependent variables are I(0)
vl create depvars = (dlog_naftrac dcetes28 dcetes91 dcetes182 dcetes364)
vl create tests = (t)
foreach var of varlist $depvars {
	quietly dfuller `var'
	quietly gen dfT_`var' = r(Zt)
	vl modify tests = tests + (dfT_`var')
} 
vl drop (t), user
sum $tests // Are all I(0)

//Verify that the shock factors are I(0)
vl create xvars = (ffr_shock fw_shock lsap_shock)
vl create xvars_tests = (t)
foreach var of varlist $xvars {
	quietly dfuller `var'
	quietly gen dfT_`var' = r(Zt)
	vl modify xvars_tests = xvars_tests + (dfT_`var')
} 
vl drop (t), user
sum $xvars_tests // Are all I(0)

//II. Model selection

//In theory we do not add laggs because HIF changes are exogenous
//However, in my case these are not exactly HIF changes. I will try to do this later

//ac dlog_naftrac
//pac dlog_naftrac

//ac dcetes28
//pac dcetes28


// III. Measure effect on treasury yields and stock price returns

//install outreg2
//ssc install outreg2

// A: Full Sample
regress dlog_naftrac ffr_shock fw_shock lsap_shock
outreg2 using fullsample, tex(pr) ctitle(Naftrac) dec(4) title(Full Sample, May 2006-Jun 2019)

regress dlog_bmvipc ffr_shock fw_shock lsap_shock
outreg2 using fullsample, tex(pr) ctitle(BMV/IPC) dec(4)

regress dcetes28 ffr_shock fw_shock lsap_shock
outreg2 using fullsample, tex(pr) ctitle(Cetes-28) dec(4)

regress dcetes91 ffr_shock fw_shock lsap_shock
outreg2 using fullsample, tex(pr) ctitle(Cetes-91) dec(4)

regress dcetes182 ffr_shock fw_shock lsap_shock
outreg2 using fullsample, tex(pr) ctitle(Cetes-182) dec(4)

regress dcetes364 ffr_shock fw_shock lsap_shock
outreg2 using fullsample, tex(pr) ctitle(Cetes-364) dec(4)

// B: Pre ZLB
regress dlog_naftrac ffr_shock fw_shock lsap_shock if date2 <= date("31dec2008", "DMY")
outreg2 using pre_zlb, tex(pr) ctitle(Naftrac) dec(4) title(Pre-ZLB Sample, May 2006-Dec 2008)

regress dlog_bmvipc ffr_shock fw_shock lsap_shock if date2 <= date("31dec2008", "DMY")
outreg2 using pre_zlb, tex(pr) ctitle(BMV/IPC) dec(4)

regress dcetes28 ffr_shock fw_shock lsap_shock if date2 <= date("31dec2008", "DMY")
outreg2 using pre_zlb, tex(pr) ctitle(Cetes-28) dec(4)

regress dcetes91 ffr_shock fw_shock lsap_shock if date2 <= date("31dec2008", "DMY")
outreg2 using pre_zlb, tex(pr) ctitle(Cetes-91) dec(4)

regress dcetes182 ffr_shock fw_shock lsap_shock if date2 <= date("31dec2008", "DMY")
outreg2 using pre_zlb, tex(pr) ctitle(Cetes-182) dec(4)

regress dcetes364 ffr_shock fw_shock lsap_shock if date2 <= date("31dec2008", "DMY")
outreg2 using pre_zlb, tex(pr) ctitle(Cetes-364) dec(4)

//C: ZLB
regress dlog_naftrac ffr_shock fw_shock lsap_shock if date2 > date("31dec2008", "DMY") & date2 <= date("30nov2015", "DMY")
outreg2 using zlb, tex(pr) ctitle(Naftrac) dec(4) title(ZLB Sample, Jan 2009-Nov 2015)

regress dlog_bmvipc ffr_shock fw_shock lsap_shock if date2 > date("31dec2008", "DMY") & date2 <= date("30nov2015", "DMY")
outreg2 using zlb, tex(pr) ctitle(BMV/IPC) dec(4)

regress dcetes28 ffr_shock fw_shock lsap_shock if date2 > date("31dec2008", "DMY") & date2 <= date("30nov2015", "DMY")
outreg2 using zlb, tex(pr) ctitle(Cetes-28) dec(4)

regress dcetes91 ffr_shock fw_shock lsap_shock if date2 > date("31dec2008", "DMY") & date2 <= date("30nov2015", "DMY")
outreg2 using zlb, tex(pr) ctitle(Cetes-91) dec(4)

regress dcetes182 ffr_shock fw_shock lsap_shock if date2 > date("31dec2008", "DMY") & date2 <= date("30nov2015", "DMY")
outreg2 using zlb, tex(pr) ctitle(Cetes-182) dec(4)

regress dcetes364 ffr_shock fw_shock lsap_shock if date2 > date("31dec2008", "DMY") & date2 <= date("30nov2015", "DMY")
outreg2 using zlb, tex(pr) ctitle(Cetes-364) dec(4)


//D: Post-ZLB
regress dlog_naftrac ffr_shock fw_shock lsap_shock if date2 > date("30nov2015", "DMY")
outreg2 using post_zlb, tex(pr) ctitle(Naftrac) dec(4) title(Post-ZLB Sample, Dec 2015 - Jun 2019)

regress dlog_bmvipc ffr_shock fw_shock lsap_shock if date2 > date("30nov2015", "DMY")
outreg2 using post_zlb, tex(pr) ctitle(BMV/IPC) dec(4)

regress dcetes28 ffr_shock fw_shock lsap_shock if date2 > date("30nov2015", "DMY")
outreg2 using post_zlb, tex(pr) ctitle(Cetes-28) dec(4)

regress dcetes91 ffr_shock fw_shock lsap_shock if date2 > date("30nov2015", "DMY")
outreg2 using post_zlb, tex(pr) ctitle(Cetes-91) dec(4)

regress dcetes182 ffr_shock fw_shock lsap_shock if date2 > date("30nov2015", "DMY")
outreg2 using post_zlb, tex(pr) ctitle(Cetes-182) dec(4)

regress dcetes364 ffr_shock fw_shock lsap_shock if date2 > date("30nov2015", "DMY")
outreg2 using post_zlb, tex(pr) ctitle(Cetes-364) dec(4)

//Direct Forecasting

//import data
clear

cd "/Users/jagtz/Library/CloudStorage/OneDrive-Personal/Libreta/1. Monetary Policy Shocks/"

// import delimited "data/swanson a la mexicana (regular frequency).csv", clear
// save "data/regular freq series.dta"
//
// clear

use "data/regular freq series.dta"

gen mdate = m(2011m10) + _n -1  //_n denotes current observation in stata

tsset mdate, m

replace dcetes28 = 100*dcetes28 
replace dcetes91 = 100*dcetes91
replace dcetes182 = 100*dcetes182
replace dcetes364 = 100*dcetes364

gen t = _n

// SET UP FOR LATER IRF PLOTTING
foreach var in dlog_naftrac dlog_bmvipc dcetes28 dcetes91 dcetes182 dcetes364 { 
  quietly gen b`var' = .
  quietly gen up90b`var' = .
  quietly gen lo90b`var' = . 
} 

gen h = t - 1  /* h is the horizon for the irfs */

// Estimate IRFs
forvalues i = 0/24 {

   foreach var in dlog_naftrac dlog_bmvipc dcetes28 dcetes364 {

      newey F`i'.`var' L(0/2).ffr_shock  L(0/2).fw_shock L(0/2).lsap_shock, lag(`=`i' + 1')
	 
	  gen b`var'h`i' = _b[ffr_shock]
  
      gen se`var'h`i' = _se[ffr_shock]
  
	  quietly replace b`var' = b`var'h`i' if h==`i'
      quietly replace up90b`var' = b`var'h`i' + 1.68*se`var'h`i' if h==`i'
	  quietly replace lo90b`var' = b`var'h`i' - 1.68*se`var'h`i' if h==`i'
	  

  }
}


foreach var in dlog_naftrac dlog_bmvipc dcetes28 dcetes364  { 

tw (rarea up90b`var' lo90b`var' h, bcolor(gs14) clw(medthin medthin)) ///
  (scatter b`var' h, c(l) clp(l) ms(i) clc(black) mc(black) clw(medthick))if h<=24
  
  graph export plots/paper/irf_`var'_ffrshock.png, replace

}


//Previous code
forvalues i = 0/120 {

      quietly regress dlog_naftrac ffr_shock fw_shock lsap_shock if h == `i'
	  
	  foreach var in ffr_shock fw_shock {
	  	 gen b_`var'_h`i' = _b[`var']
		 gen se_`var'_h`i' = _se[`var']
		 
		 quietly replace b_`var' = b_`var'_h`i' if h==`i'
		 quietly replace up90b_`var' = b_`var'_h`i' + 1.68*se_`var'_h`i' if h==`i'
		 quietly replace lo90b_`var' = b_`var'_h`i' - 1.68*se_`var'_h`i' if h==`i'
		 
		 drop b_`var'_h`i'
		 drop se_`var'_h`i'
	  } 
}

//Plot IRF and export plots

//Example, how to export
tw (rarea up90b_ffr_shock lo90b_ffr_shock h, bcolor(gs14) clw(medthin medthin)) ///
  (scatter b_ffr_shock h, c(l) clp(l) ms(i) clc(blue) mc(black) clw(medthick))if h<=120, xtitle(Days) ytitle(Percentage Price Change) ///
  title(Effect of Federal Funds Rate on Daily NAFTRAC Price) legend(label(1 "90% CI") label(2 "Estimated Coefficients")) xsc(range(0 120)) xlabel(0(20)120)
  
graph export plots/irf_naftrac_ffrshock.png


