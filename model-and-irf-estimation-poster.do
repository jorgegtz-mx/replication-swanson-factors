//Swanson a la Mexicana
//April 17th 2023

clear 

cd "/Users/jagtz/Library/CloudStorage/OneDrive-Personal/UT/2. Courses/2.7 Time Series/poster/"

//import delimited "data/swanson a la mexicana.csv", numericcols(8 910 12 13 14 15 16 17) clear
//save "/Users/jagtz/Library/CloudStorage/OneDrive-Personal/UT/2. Courses/2.7 Time Series/poster/data/swanson mex.dta"

use "data/swanson mex.dta"

gen t = _n
tsset t

//I cannot use date2 to set time series data because date intervals are not constant
gen date2 = date(date, "YMD")
format date2 %td

//I. Check that depvars and indepvars are covairance stationary

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

//import delimited "data/projection_naftrac.csv", clear
//save "data/projection_naftrac.dta"

//import delimited "data/projection_cetes.csv", clear
//save "data/projection_cetes.dta"

clear

use "data/projection_naftrac"

gen t = _n
tsset t

gen date2 = date(date, "YMD")
format date2 %td

foreach var in ffr_shock fw_shock  { 
  quietly gen b_`var' = .
  quietly gen up90b_`var' = .
  quietly gen lo90b_`var' = . 
} 

//Use Newey West
forvalues i = 0/30 {
	
	quietly newey dlog_naftrac L(0/2).ffr_shock  L(0/2).fw_shock L(0/2).lsap_shock, lag(`=`i' + 1')
		  
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

//Jorda Projections or Direct Forecasting
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

//federal funds rate shock
tw (rarea up90b_ffr_shock lo90b_ffr_shock h, bcolor(gs14) clw(medthin medthin)) ///
  (scatter b_ffr_shock h, c(l) clp(l) ms(i) clc(blue) mc(black) clw(medthick))if h<=120, xtitle(Days) ytitle(Percentage Price Change) ///
  title(Effect of Federal Funds Rate on Daily NAFTRAC Price) legend(label(1 "90% CI") label(2 "Estimated Coefficients")) xsc(range(0 120)) xlabel(0(20)120)
  
graph export plots/irf_naftrac_ffrshock.png

//forward guidance shock
tw (rarea up90b_fw_shock lo90b_fw_shock h, bcolor(gs14) clw(medthin medthin)) ///
  (scatter b_fw_shock h, c(l) clp(l) ms(i) clc(blue) mc(black) clw(medthick))if h<=120, xtitle(Days) ytitle(Percentage Price Change) ///
  title(Effect of Forward Guidance on Daily NAFTRAC Price) legend(label(1 "90% CI") label(2 "Estimated Coefficients")) xsc(range(0 120)) xlabel(0(20)120)
  
graph export plots/irf_naftrac_fwshock.png

// Now I will predict cetes364

clear

use "data/projection_cetes"

gen t = _n
tsset t

gen date2 = date(date, "YMD")
format date2 %td

replace dcetes364 = dcetes364*100

foreach var in ffr_shock fw_shock  { 
  quietly gen b_`var' = .
  quietly gen up90b_`var' = .
  quietly gen lo90b_`var' = . 
} 

//Jorda Projections or Direct Forecasting
forvalues i = 0/120 {

      quietly regress dcetes364 ffr_shock fw_shock lsap_shock if h == `i'
	  
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

//federal funds rate shock
tw (rarea up90b_ffr_shock lo90b_ffr_shock h, bcolor(gs14) clw(medthin medthin)) ///
  (scatter b_ffr_shock h, c(l) clp(l) ms(i) clc(blue) mc(black) clw(medthick))if h<=120, xtitle(Days) ytitle(Change in Basis Points) ///
  title(Effect of Federal Funds Rate on 1-Year CETES Yield) legend(label(1 "90% CI") label(2 "Estimated Coefficients")) xsc(range(0 120)) xlabel(0(20)120)
  
graph export plots/irf_cetes364_ffrshock.png

//forward guidance shock
tw (rarea up90b_fw_shock lo90b_fw_shock h, bcolor(gs14) clw(medthin medthin)) ///
  (scatter b_fw_shock h, c(l) clp(l) ms(i) clc(blue) mc(black) clw(medthick))if h<=120, xtitle(Days) ytitle(Chaneg in Basis Points) ///
  title(Effect of Forward Guidance on 1-Year CETES Yield) legend(label(1 "90% CI") label(2 "Estimated Coefficients"))  xsc(range(0 120)) xlabel(0(20)120)
  
graph export plots/irf_cetes364_fwshock.png


