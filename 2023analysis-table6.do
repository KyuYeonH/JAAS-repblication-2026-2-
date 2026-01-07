clear
cd "/Users/kyuyeonhwang/Desktop/Dissertation/Data/한국재정패널조사데이터/NEW"
use "NaSTaB16H.dta"

merge 1:m hid16 using NaSTaB16P.dta

tab _merge
drop _merge
save 2023_merged_dataset_origin.dta, replace



////////// load data ////////////////////////////

clear
use "2023_merged_dataset_origin.dta"



////////////////////////////////////
gen age = 2023 - w16byr01

// 
recode age (10/19=10) (20/29=20) (30/39=30) (40/49=40) (50/59=50) (60/69=60) (70/79=70) (80/max=80), gen(age_group)

// 
drop if age_group < 20
tab age_group

// 
drop if age_group < 30
tab age_group


replace h16fc100 = 0 if h16fc100 == 2
replace w16gen01 = 0 if w16gen01 == 2
replace w16mar01 = 0 if w16mar01 > 1
drop if w16edu01 < 0


// 
replace h16fc001 = 0 if missing(h16fc001)
egen asset = rowtotal(h16fa002 h16fa004 h16fa006 h16fa008 h16fa010 h16fa018 h16fa012 h16fa014 h16fb031 h16fb010 h16fb012 h16fb022 h16fb024)

gen ratio = h16fc001 / asset
drop if ratio < 0
drop if h16fa002 < 0

gen lh16fc001 = log(h16fc001)
gen lh16fb031 = log(h16fb031)
gen lh16inc = log(h16inc)

sum h16fc100 ratio age w16fnum h16fb031 h16fa002 h16inc w16gen01 w16edu01 w16mar01 p16ge001

// Probit 
probit h16fc100 i.age_group w16fnum h16fb031 h16fa002 h16inc w16gen01 w16edu01 w16mar01 p16ge001
estout, cells(b(star fmt(3)) se(fmt(3))) stats(N r2) starlevels(* 0.10 ** 0.05 *** 0.01)

// 
probit h16fc100 i.age_group w16fnum h16fb031 h16fa002 h16inc w16gen01 w16edu01 w16mar01 p16ge001
test w16fnum h16fb031 h16fa002 h16inc w16gen01 w16edu01 w16mar01 p16ge001

// 
probit h16fc100 i.age_group w16fnum h16fb031 h16fa002 h16inc w16gen01 w16edu01 w16mar01 p16ge001
predict xbhat, xb  // 
gen xbhat_sq = xbhat^2  // 

probit h16fc100 xbhat xbhat_sq  // link test

// Tobit
tobit ratio i.age_group w16fnum lh16fb031 h16fa002 lh16inc w16gen01 w16edu01 w16mar01 p16ge001, ll(0) vce(robust)
estout, cells(b(star fmt(3)) se(fmt(3))) stats(N r2) starlevels(* 0.10 ** 0.05 *** 0.01)

// OLS 
reg ratio i.age_group w16fnum lh16fb031 h16fa002 lh16inc w16gen01 w16edu01 w16mar01 p16ge001
estout, cells(b(star fmt(3)) se(fmt(3))) stats(N r2) starlevels(* 0.10 ** 0.05 *** 0.01)

// Tobit
tobit ratio i.age_group w16fnum lh16fb031 h16fa002 lh16inc w16gen01 w16edu01 w16mar01 p16ge001, ll(0)
estimates store tobit_model

// OLS
reg ratio i.age_group w16fnum lh16fb031 h16fa002 lh16inc w16gen01 w16edu01 w16mar01 p16ge001
estimates store ols_model

// 
estout tobit_model ols_model, cells(b(star fmt(3)) se(fmt(3))) stats(N r2) starlevels(* 0.10 ** 0.05 *** 0.01)

// Tobit marginal effects
tobit ratio i.age_group w16fnum lh16fb031 h16fa002 lh16inc w16gen01 w16edu01 w16mar01 p16ge001, ll(0) vce(robust)
esttab, se star(* 0.10 ** 0.05 *** 0.01)

margins, dydx(*) predict(ystar(0,.)) //
margins, dydx(*) predict(ystar(0,0)) //
margins, dydx(*) predict(ystar(.,.)) //

// Probit marginal effects
probit h16fc100 i.age_group w16fnum h16fb031 h16fa002 h16inc w16gen01 w16edu01 w16mar01 p16ge001, vce(robust)
margins, dydx(*) //
