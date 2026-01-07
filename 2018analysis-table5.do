clear
// merge data
cd "/Users/kyuyeonhwang/Desktop/Dissertation/Data/한국재정패널조사데이터/NEW"
use "NaSTaB11H.dta"

merge 1:m hid11 using NaSTaB11P.dta

tab _merge
drop _merge
save 2018_merged_dataset_origin.dta, replace

//////////load data ////////////////////////////

clear
cd "/Users/kyuyeonhwang/Desktop/Dissertation/Data/한국재정패널조사데이터/NEW"
use "2018_merged_dataset_origin.dta"


////////////////////////////////////
gen age = 2018 - w11byr01

// 
recode age (10/19=10) (20/29=20) (30/39=30) (40/49=40) (50/59=50) (60/69=60) (70/79=70) (80/max=80), gen(age_group)

// 
drop if age_group < 30
tab age_group


replace h11fc100 = 0 if h11fc100 == 2
replace h11fc011 = 0 if h11fc011 == 2
replace h11fc016 = 0 if h11fc016 == 2
replace w11gen01 = 0 if w11gen01 == 2
replace w11mar01 = 0 if w11mar01 > 1
drop if w11edu01 < 0
drop if h11fb031 < 0
drop if h11fa002 < 0
drop if age > 100


// 
replace h11fc001 = 0 if missing(h11fc001)
egen asset = rowtotal(h11fa002 h11fa004 h11fa006 h11fa008 h11fa010 h11fa018 h11fa012 h11fa014 h11fb031 h11fb010 h11fb012 h11fb022 h11fb024)

gen ratio = h11fc001 / asset
drop if ratio < 0

gen lh11fc001 = log(h11fc001)
gen lh11fb031 = log(h11fb031)
gen lh11inc = log(h11inc)

sum h11fc100 lh11fc001 ratio age w11fnum h11fb031 h11fa002 h11inc w11gen01 w11edu01 w11mar01 p11ge001


// Probit 
probit h11fc100 i.age_group w11fnum h11fb031 h11fa002 h11inc w11gen01 w11edu01 w11mar01 p11ge001
estout, cells(b(star fmt(3)) se(fmt(3))) stats(N r2) starlevels(* 0.10 ** 0.05 *** 0.01)

// 
probit h11fc100 i.age_group w11fnum h11fb031 h11fa002 h11inc w11gen01 w11edu01 w11mar01 p11ge001
test w11fnum h11fb031 h11fa002 h11inc w11gen01 w11edu01 w11mar01 p11ge001

// 
probit h11fc100 i.age_group w11fnum h11fb031 h11fa002 h11inc w11gen01 w11edu01 w11mar01 p11ge001
predict xbhat, xb  // 
gen xbhat_sq = xbhat^2  // 

probit h11fc100 xbhat xbhat_sq  // link test

// Tobit 
tobit ratio i.age_group w11fnum lh11fb031 h11fa002 lh11inc w11gen01 w11edu01 w11mar01 p11ge001, ll(0) vce(robust)
estout, cells(b(star fmt(3)) se(fmt(3))) stats(N r2) starlevels(* 0.10 ** 0.05 *** 0.01)

// OLS 
reg ratio i.age_group w11fnum lh11fb031 h11fa002 lh11inc w11gen01 w11edu01 w11mar01 p11ge001
estout, cells(b(star fmt(3)) se(fmt(3))) stats(N r2) starlevels(* 0.10 ** 0.05 *** 0.01)

// Tobit 
tobit ratio i.age_group w11fnum lh11fb031 h11fa002 lh11inc w11gen01 w11edu01 w11mar01 p11ge001, ll(0)
estimates store tobit_model

// OLS 
reg ratio i.age_group w11fnum lh11fb031 h11fa002 lh11inc w11gen01 w11edu01 w11mar01 p11ge001
estimates store ols_model

// estout
estout tobit_model ols_model, cells(b(star fmt(3)) se(fmt(3))) stats(N r2) starlevels(* 0.10 ** 0.05 *** 0.01)

// Tobit marginal effects
tobit ratio i.age_group w11fnum lh11fb031 h11fa002 lh11inc w11gen01 w11edu01 w11mar01 p11ge001, ll(0) vce(robust)
esttab, se star(* 0.10 ** 0.05 *** 0.01)

margins, dydx(*) predict(ystar(0,.)) //
margins, dydx(*) predict(ystar(0,0)) //
margins, dydx(*) predict(ystar(.,.)) //

// Probit marginal effects
probit h11fc100 i.age_group w11fnum h11fb031 h11fa002 h11inc w11gen01 w11edu01 w11mar01 p11ge001, vce(robust)
margins, dydx(*) //
