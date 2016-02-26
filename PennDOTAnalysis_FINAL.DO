* Final Stata code for the Pennsylvania Rebound Effect Analysis
* 7/10/2015
* Ken Gillingham, Alan Jenn, Ines Azevedo
* Published in Energy Economics


//***************************************************************//
// Begin by bringing in the raw data and performing some data prep

clear all
cd C:\Users\ktg2\Documents\PennDOT\data

insheet using parsedInspections.csv, comma

sort id

merge m:1 id using DecodedVINs_sm.dta	// bring in vehicle characteristics, including MPG
keep if _merge==3
drop _merge
drop if make==""
drop if combined_mpg==.

ren year model_year
drop shortvin

split date, parse("-")
destring date1, force replace
destring date2, force replace
destring date3, force replace
ren date1 year
ren date2 month
ren date3 day

split pdate, parse("-")
destring pdate1, force replace
destring pdate2, force replace
destring pdate3, force replace
ren pdate1 pyear
ren pdate2 pmonth
ren pdate3 pday
drop date pdate

gen yearmonth = ym(year,month)
format yearmonth %tm

gen pyearmonth = ym(pyear,pmonth)
format pyearmonth %tm

keep if year > 2001 // very few observations prior to 2002, so they are unusual and thus would have introduced bias

gen age = year - model_year	// generate the vehicle age
replace age = 0 if age <0

// rename variables in a more sensible way
ren curbwt weight
ren displ liters

// generate log variables
gen lvmt = log(annualvmt)
gen lavggas = log(avggas)
gen lavgoil = log(avgoil)
gen lavgshutin = log(avgshutin)
gen lmpg = log(combined_mpg)
gen lgdp = log(avggdp)
gen lunempl = log(avgunempadj)
gen lage = log(age)
gen agesq = age^2
gen lweight = log(weight)
gen lliters = log(liters)

gen monthstotest = yearmonth - pyearmonth

gen earlytest = (monthstotest < 11)				// early test
gen normaltest = (monthstotest > 10 & monthstotest < 14)	// normal 1 year test
gen latetest = (monthstotest > 13 & monthstotest < 22)		// late test
gen vlatetest = (monthstotest > 21)				// very late test

gen byte diesel = fueltype=="D"

ren id vehid
sort vehid year
gen byte firstveh = (year ==2002)	// cohort around in 2002 and all subsequent years
by vehid: replace firstveh = firstveh[1]
gen byte earlyveh = (year < 2006)	// all vehicles in the 2002-2005
by vehid: replace earlyveh = earlyveh[1]

// Get the cohort that was around in 2002 and all subsequent years
forvalues y = 2002/2010 {
	gen byte firstveh1 = (year == `y')
	by vehid: egen firstveh`y' = max(firstveh1)
	drop firstveh1
}
gen byte startingveh = (firstveh2002 & firstveh2003 & firstveh2004 & firstveh2005 & firstveh2006 & firstveh2007 & firstveh2008 & firstveh2009 & firstveh2010)
drop firstveh2002-firstveh2010

// Get the cohort that was around in 2006 and all subsequent years
forvalues y = 2006/2010 {
	gen byte lateveh1 = (year == `y' & !earlyveh)
	by vehid: egen lateveh`y' = max(lateveh1)
	drop lateveh1
}
gen byte lateveh = (lateveh2006 & lateveh2007 & lateveh2008 & lateveh2009 & lateveh2010)
drop lateveh2006-lateveh2010

// bring in the county in which the vehicle had the insepection
sort zip
merge m:1 zip using ../data/zips_to_counties_PA.dta
drop if _merge==2
drop _merge
drop population

// remove vehicles with crazy high or low driving.
drop if annualvmt > 100000
drop if annualvmt==0

// remove missing values from the sample
foreach var in lvmt lgdp lunempl age agesq avgshutin {
	drop if `var'==.
}

// remove singleton groups from the sample (they will be dropped by the fixed effects estimation anyway)
ren vehid id
bysort id: egen numinid = count(id)
drop if numinid==1
drop numinid

qui tab year, gen(y)	// set these up since they speed things up with xi (and will be needed for the quantiles)
qui tab month, gen(mon)

compress
save ../data/PennDOT_full, replace

//***************************************************************//
// Figures

log using ../logs/PennDOT_estimation_v4.log, replace
use ../data/PennDOT_full, clear

label var monthstotest "Months between tests"
hist monthstotest if monthstotest< 37, bin(36) scheme(s1mono) 
graph export ../graphs/montotest_hist.eps, logo(off) orientation(portrait) replace
!epstopdf ../graphs/montotest_hist.eps

label var annualvmt "Annual VMT (miles)"
hist annualvmt if annualvmt<60000, scheme(s1mono) freq ylabel(0(200000)1000000, format(%-10.0gc))
graph export ../graphs/annualvmt_hist.eps, logo(off) orientation(portrait) replace
!epstopdf ../graphs/annualvmt_hist.eps


//***************************************************************//
// Primary Estimations and Summary Statistics

global T "earlytest normaltest latetest"

set more off
fvset base 2002 year
fvset base 11 monthstotest
fvset base 1 month

// Core results
// full sample
xtset id
set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin), fe first cluster(id) // if year>2004
estimates store FullIV_bothFE_T

keep if e(sample)==1 // make sure all results use the exact same sample as the IV results

set more off
reg lvmt lavggas lgdp lunempl age agesq i.year i.month $T, cluster(id)
estimates store Full_TimeFE
areg lvmt lavggas lgdp lunempl age agesq, absorb(id) vce(cluster id)
estimates store Full_idFE
areg lvmt lavggas lgdp lunempl age agesq i.year i.month $T, absorb(id) vce(cluster id)
estimates store Full_bothFE

// summary statistics on the estimation sample
sum annualvmt odometer avggas combined_mpg age avgunempadj avggdp avgshutin
sum annualvmt odometer avggas combined_mpg age avgunempadj avggdp avgshutin if earlyveh
sum annualvmt odometer avggas combined_mpg age avgunempadj avggdp avgshutin if lateveh

// Primary results without unemployment (robustness check)
set more off
reg lvmt lavggas lgdp age agesq i.year i.month $T, cluster(id)
estimates store Full_TimeFE_nounem
areg lvmt lavggas lgdp age agesq, absorb(id) vce(cluster id)
estimates store Full_idFE_nounem
areg lvmt lavggas lgdp age agesq i.year i.month $T, absorb(id) vce(cluster id)
estimates store Full_bothFE_nounem

xtset id
set more off
xtivreg2 lvmt lgdp age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin), fe first cluster(id)
estimates store FullIV_bothFE_T_nounem

// Primary results for Philly and Pittsburgh
set more off
reg lvmt lavggas lgdp lunempl age agesq i.year i.month $T if earlyveh, cluster(id)
estimates store PP_TimeFE
areg lvmt lavggas lgdp lunempl age agesq if earlyveh, absorb(id) vce(cluster id)
estimates store PP_idFE
areg lvmt lavggas lgdp lunempl age agesq i.year i.month $T if earlyveh, absorb(id) vce(cluster id)
estimates store PP_bothFE
xtset id
set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin) if earlyveh, fe first cluster(id)
estimates store PPIV_bothFE

// Primary results for the rest of PA
set more off
reg lvmt lavggas lgdp lunempl age agesq i.year i.month $T if lateveh, cluster(id)
estimates store Rest_TimeFE
areg lvmt lavggas lgdp lunempl age agesq if lateveh, absorb(id) vce(cluster id)
estimates store Rest_idFE
areg lvmt lavggas lgdp lunempl age agesq i.year i.month $T if lateveh, absorb(id) vce(cluster id)
estimates store Rest_bothFE
xtset id
set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin) if lateveh, fe cluster(id)
estimates store RestIV_bothFE

// Philly and Pitt - no unemployment robustness check
set more off
areg lvmt lavggas lgdp age agesq i.year i.month $T if earlyveh, absorb(id) vce(cluster id)
estimates store PP_bothFE_nounem
xtset id
set more off
xtivreg2 lvmt lgdp age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin) if earlyveh, fe first cluster(id)
estimates store PPIV_bothFE_nounem

// Rest of PA - no unemployment robustness check
set more off
areg lvmt lavggas lgdp age agesq i.year i.month $T if lateveh, absorb(id) vce(cluster id)
estimates store Rest_bothFE_nounem
xtset id
set more off
xtivreg2 lvmt lgdp age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin) if lateveh, fe cluster(id)
estimates store RestIV_bothFE_nounem

// Core results (Tables 1 and 2) output
set more off
estout Full_TimeFE Full_idFE Full_bothFE FullIV_bothFE_T, keep(lavggas lgdp lunempl age agesq) cells(b(star fmt(%9.2f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)
set more off
estout PP_bothFE PPIV_bothFE Rest_bothFE RestIV_bothFE, keep(lavggas lgdp lunempl age agesq) cells(b(star fmt(%9.2f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)

// Results without unemployment
set more off
estout Full_TimeFE_nounem Full_idFE_nounem Full_bothFE_nounem FullIV_bothFE_T_nounem, keep(lavggas lgdp age agesq) cells(b(star fmt(%9.2f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)
set more off
estout PP_bothFE_nounem PPIV_bothFE_nounem Rest_bothFE_nounem RestIV_bothFE_nounem, keep(lavggas lgdp age agesq) cells(b(star fmt(%9.2f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)


//***************************************************************************************************//
// Varying response with vehicle fuel economy

gen fecat1 = (combined_mpg < 20)
gen fecat2 = (combined_mpg >= 20 & combined_mpg <= 30)
gen fecat3 = (combined_mpg > 30)


// Full Sample IV Regression
xtset id
set more off
// full sample
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas = lavgshutin), fe cluster(id)
estimates store StratFEIV_Full

// fecat1 subsample
set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas = lavgshutin) if fecat1, fe cluster(id)
estimates store StratFEIV_1

// fecat2 subsample
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas = lavgshutin) if fecat2, fe cluster(id)
estimates store StratFEIV_2

// fecat3 subsample
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas = lavgshutin) if fecat3, fe cluster(id)
estimates store StratFEIV_3

set more off
estout StratFEIV_Full StratFEIV_1 StratFEIV_2 StratFEIV_3, cells(b(star fmt(%9.3f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)


//***************************************************************************************************//
// Varying response with vehicle age

sort id
by id: egen meanage = mean(age)

gen agecat1 = (age < 3) 
gen agecat2 = (age >= 3 & age < 7)
gen agecat3 = (age >= 7)
gen lavggas_agecat2 = lavggas*agecat2
gen lavggas_agecat3 = lavggas*agecat3
gen lavgoil_agecat2 = lavgoil*agecat2
gen lavgoil_agecat3 = lavgoil*agecat3
gen lavgshutin_agecat2 = lavgshutin*agecat2
gen lavgshutin_agecat3 = lavgshutin*agecat3


// full sample
xtset id
set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas lavggas_agecat2 lavggas_agecat3 = lavgshutin lavgshutin_agecat2 lavgshutin_agecat3), fe cluster(id)
estimates store VaryAge_FullIV

// cat1 subsample
set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas = lavgshutin) if agecat1, fe cluster(id)
estimates store StratAgeIV_1

// cat2 subsample
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas = lavgshutin) if agecat2, fe cluster(id)
estimates store StratAgeIV_2

// cat3 subsample
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas = lavgshutin) if agecat3, fe cluster(id)
estimates store StratAgeIV_3

set more off
estout StratAgeIV_1 StratAgeIV_2 StratAgeIV_3, cells(b(star fmt(%9.2f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)



//***************************************************************************************************//
// Robustness checks

//*************
// 1. Include average price per mile rather than gas price (including fuel economy would drop with vehicle fixed effects)
gen avgpricepermi = avggas/combined_mpg
gen lavgpricepermi = log(avgpricepermi)

set more off
fvset base 2002 year
fvset base 11 monthstotest
fvset base 1 month

xtset id
set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavgpricepermi=lavgshutin), fe cluster(id)
estimates store PricePerMi_FullIV

set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavgpricepermi=lavgshutin) if earlyveh, fe cluster(id)
estimates store PricePerMi_PPIV

set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavgpricepermi=lavgshutin) if lateveh, fe cluster(id)
estimates store PricePerMi_RestIV

set more off
estout PricePerMi_FullIV PricePerMi_PPIV PricePerMi_RestIV, cells(b(star fmt(%9.2f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)


//**************
// 2. Examine robustness to a linear specification (mentioned in footnote 10)

fvset base 2002 year
fvset base 11 monthstotest
fvset base 1 month
xtset id
set more off
xtivreg2 annualvmt avggdp avgunempadj age agesq y2-y9 mon2-mon12 $T (avggas=avgshutin), fe first cluster(id)
estimates store LinFullIV_bothFE_T

set more off
estout LinFullIV_bothFE_T, cells(b(star fmt(%9.2f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)

log close

//***************************************************************************************************//
// Varying response by quantile of response

log using ../logs/PennDOT_quantiles_v3.log, replace

set seed 1001
gen randomval = runiform()
sort id
by id: replace randomval = randomval[1]
drop if randomval > 0.10 	// Use a 10% sample of vehicles

// IV qreg
set more off
ivqreg lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgoil), q(0.25)
estimates store FullIV_quant25
set more off
ivqreg lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgoil), q(0.5)
estimates store FullIV_quant50
set more off
ivqreg lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgoil), q(0.75)
estimates store FullIV_quant75

set more off
estout FullIV_quant25 FullIV_quant50 FullIV_quant75, cells(b(star fmt(%9.2f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)


// IV qreg with vehicle FE using Canay (2011) two-step estimator approach

// First-stage (FE regression)
xtset id
set more off
xtivreg2 lvmt lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin), fe first cluster(id)
estimates store FullIV_bothFE_T

predict double resid if e(sample), e
bysort id: egen vehFE = mean(resid)
gen ynew = lvmt - vehFE	// this is subtracting off X*b for each vehicle
destring ynew, replace

set more off
ivqreg ynew lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin), q(0.25)
estimates store FullIVFE_quant25
set more off
ivqreg ynew lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin), q(0.50)
estimates store FullIVFE_quant50
set more off
ivqreg ynew lgdp lunempl age agesq y2-y9 mon2-mon12 $T (lavggas=lavgshutin), q(0.75)
estimates store FullIVFE_quant75

set more off
estout FullIVFE_quant25 FullIVFE_quant50 FullIVFE_quant75, cells(b(star fmt(%9.3f)) se(par)) stats(r2 N,fmt(%9.3f %9.0g) labels(R-squared N)) label collabels(none) style(tex)

log close



