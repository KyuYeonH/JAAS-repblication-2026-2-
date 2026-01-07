// 데이터 파일 합치기
cd "/Users/kyuyeonhwang/Desktop/Dissertation/Data/CHPS 中国家庭面板数据/CFPS2022Stata"
use "/Users/kyuyeonhwang/Desktop/Dissertation/Data/CHPS 中国家庭面板数据/CFPS2022Stata/cfps2022famecon_202410.dta"

merge 1:m fid22 using cfps2022person_202410.dta

tab _merge

// generate variable (debt ratio)
gen ratio = (nonhousing_debts + house_debts) / total_asset
drop if ratio < 0 | ratio > 100

// generate variable (debt holding)
gen debt_binary = (nonhousing_debts > 0 | house_debts > 0 | ft601 > 0)

// data rebuilding by age group
recode age (10/19=10) (20/29=20) (30/39=30) (40/49=40) (50/59=50) (60/69=60) (70/79=70) (80/max=80), gen(age_group)

// remove under 20 age
drop if age_group < 20
tab age_group

// Data Checks and Cleaning: Education, Health, and Marital Status
codebook kw01 qea0 qp201
drop if kw01 < 0 | qea0 < 0 | qp201 < 0 | missing(kw01, qea0, qp201)

// Recode education variable: assign 0 to individuals with no formal education (originally coded as 10).
replace kw01 = 0 if kw01 == 10

// Descriptive Statistics of Variables
sum ratio age debt_binary familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201

// 
asdoc sum ratio familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201, detail save(summary_stats.doc) replace

// Tobit 
tobit ratio i.age_group familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201, ll(0)

// Tobit
asdoc tobit ratio i.age_group familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201, ll(0) replace stars(0.10 0.05 0.01) save(tobit_results.doc)

// Probit
probit debt_binary i.age_group familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201

// OLS
reg ratio i.age_group familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201

// Tobit, OLS, Probit
eststo model1: tobit ratio i.age_group familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201, ll(0)
eststo model2: reg ratio i.age_group familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201
eststo model3: probit debt_binary i.age_group familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201

// 
esttab model1 model2 model3, stats(N r2_p ll) se star(* 0.10 ** 0.05 *** 0.01) mtitles("Tobit" "OLS" "Probit")

// finance_asset houseasset_net fincome1 for ln
gen ln_finance_asset = log(finance_asset)
gen ln_houseasset_net = log(houseasset_net)
gen ln_fincome1 = log(fincome1)

// Tobit, OLS, Probit
eststo model1: tobit ratio i.age_group familysize22 ln_finance_asset ln_houseasset_net ln_fincome1 gender kw01 qea0 qp201, ll(0)
eststo model2: reg ratio i.age_group familysize22 ln_finance_asset ln_houseasset_net ln_fincome1 gender kw01 qea0 qp201
eststo model3: probit debt_binary i.age_group familysize22 ln_finance_asset ln_houseasset_net ln_fincome1 gender kw01 qea0 qp201

// 
esttab model1 model2 model3, stats(N r2_p ll) se star(* 0.10 ** 0.05 *** 0.01) mtitles("Tobit" "OLS" "Probit")

esttab model1 model2 model3 using regression_results.rtf, replace ///
    stats(N r2_p ll) se star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Tobit" "OLS" "Probit")
	
//
tobit ratio i.age_group familysize22 ln_finance_asset ln_houseasset_net ln_fincome1 gender kw01 qea0 qp201, ll(0)
tobit ratio i.age_group familysize22 ln_finance_asset ln_houseasset_net ln_fincome1 gender kw01 qea0 qp201, ll(0) vce(robust)
esttab, se star(* 0.10 ** 0.05 *** 0.01)
// 
tobit ratio i.age_group familysize22 finance_asset houseasset_net fincome1 gender kw01 qea0 qp201, ll(0)
predict ratiohat
gen ratiohat_sq = ratiohat^2
reg ratio ratiohat ratiohat_sq


/// Tobit Marginal
margins, dydx(*) predict(ystar(0,.)) // 전체 샘플에 대한 한계효과
esttab, se star(* 0.10 ** 0.05 *** 0.01)
margins, dydx(*) predict(ystar(0,0)) // 검열된 부분에 대한 한계효과
margins, dydx(*) predict(ystar(.,.)) // 비검열된 부분에 대한 한계효과
* Marginal effects on the probability of a positive outcome
margins, dydx(*) predict(pu0)
esttab, se star(* 0.10 ** 0.05 *** 0.01)

/// Probit Marginal
probit debt_binary i.age_group familysize22 ln_finance_asset ln_houseasset_net ln_fincome1 gender kw01 qea0 qp201, vce(robust)
margins, dydx(*)
esttab, se star(* 0.10 ** 0.05 *** 0.01)
