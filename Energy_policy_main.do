/* 
The heterogenous role of energy policies in the energy transition of Asia-Pacific emerging economies

Created by Peipei Chen, November, 2020.

*/

clear
set more off


********************************************************************************
// Data exploration
use "Energy_policy.dta", replace

* set up the panel data, `id` is encoding for country codes
xtset id year

g y711 = ln(SDG_711_raw+1)
g y712 = ln(SDG_712_raw+1) 
g y731 = ln(SDG_731_raw+1)
g y7b1 = ln(SDG_7b1_raw+1)

g sto = L.stock 
g s711 = L.stock711
g s712 = L.stock712
g s731 = L.stock731
g s7b1 = L.stock7b1

g law = L.tlaw
g regu = L.tregu 
g plan = L.tplan
g other = L.tother

g law1 = L.law711
g regu1 = L.reg711
g plan1 = L.plan711
g other1 = L.other711

g law2 = L.law712
g regu2 = L.reg712
g plan2 = L.plan712
g other2 = L.other712

g law3 = L.law731
g regu3 = L.reg731
g plan3 = L.plan731
g other3 = L.other731

g law4 = L.law7b1
g regu4 = L.reg7b1
g plan4 = L.plan7b1
g other4 = L.other7b1

g urb = L.urban
g gdp_raw = ln(adj_de_gdp+1)
g gdp = L.gdp_raw
g exp = L.exp_share
g imp = L.imp_share 
g enim = L.en_import
g serv = L.serv_share 

g voice = L.VoiceandAccountability
g poli = L.PoliticalStability 
g gove = L.GovernmentEffectiveness 
g regula = L.RegulatoryQuality 
g corp = L.ControlofCorruption 
g rule = L.RuleofLaw 

egen ecoid = group(economic)

********************************************************************************
// Table S5: Decriptive statistics
summarize y711 y712 y731 y7b1 sto law regu plan other urb gdp exp imp serv enim ///
	voice gove poli regula corp rule
	
outreg2 using decriptive_statistics.doc, ///
	replace sum(log) keep(y711 y712 y731 y7b1 sto law regu plan other urb gdp /// 
	exp imp serv enim voice gove poli regula corp rule) title(Decriptive statistics)


********************************************************************************
// Table 1: Overall effect of energy policy
global convars = "urb gdp exp imp serv enim voice gove poli regula corp rule"
global effect = "sto"
global keepvars = "sto urb gdp exp imp serv enim voice gove poli regula corp rule"
local sdgs y711 y712 y731 y7b1

eststo clear
forvalues i=1/4 {
	local sdg: word `i' of `sdgs'
	eststo: qui xtscc `sdg' $effect $convars i.ecoid#i.year, fe lag(0)
	estadd local fe "YES"
	estadd local year "YES"
}

esttab using "overall_effect.rtf", ///
	replace se eqlabels(none) nonotes nomtitles ///
	mlabels("Access to electricity(Indicator 7.1.1)" ///
	"Access to clean cooking (Indicator 7.1.2)" ///
	"Energy intensity (Indicator 7.3.1)" ///
	"Renewable electricity capacity (Indicator 7.b.1)") ///
	stat(fe year N r2_w, ///
	labels("Country FE" "Year FE" "Obs." "Adj. R{\super 2}") ///
	fmt(%10s %10s %9.0fc %9.3fc)) ///
	star(* 0.1 ** 0.05 *** 0.01) ///
	keep($keepvars) ///
	varlabels(sto "Stock" urb "Urbanization rate" gdp "GDP per capita"  ///
	exp "Export share" imp "Import share" serv "Service share" ///
	enim "Energy import share" voice "Voice and accountability" ///
	gove "Government effectiveness" poli "Political stability" ///
	regula "Regulatory quality" corp "Control of corruption" rule "Rule of law")


********************************************************************************
// Data 1: Counterfactual calculation
/* 
  The counterfactual values of sub-goals are represented by the gap between 
  true values of `y` and predicted values of `y` (without policies).
*/
* SDG 711:
qui xtreg y711 $effect $convars i.ecoid#i.year, fe coefl
g b1 = _b[sto]
g exp_ = b1 * sto
g rsdg711 = SDG_711_raw * exp(-exp_)
drop b1 exp_


* SDG 712
qui xtreg y712 $effect $convars i.ecoid#i.year, fe coefl
g b1 = _b[sto]
g exp_ = b1 * sto
g rsdg712 = SDG_712_raw * exp(-exp_)
drop b1 exp_


* SDG 731
qui xtreg y731 $effect $convars i.ecoid#i.year, fe coefl
g b1 = _b[sto]
g exp_ = b1 * sto
g rsdg731 = SDG_731_raw * exp(-exp_)
drop b1 exp_

* SDG 7b1
qui xtreg y7b1 $effect $convars i.ecoid#i.year, fe coefl
g b1 = _b[sto]
g exp_ = b1 * sto
g rsdg7b1 = SDG_7b1_raw * exp(-exp_)
drop b1 exp_

* Output predicted and true values into the spreadsheet
export excel nation year region income_group SDG_711_raw rsdg711 SDG_712_raw rsdg712 SDG_731_raw rsdg731 SDG_7b1_raw rsdg7b1 using "export_est_sdg.xlsx", firstrow(variables) replace



********************************************************************************
// Table S8: Effect of energy policy type

* colinearlity test
local sdgs y711 y712 y731 y7b1

forvalues i=1/4 {
	local sdg: word `i' of `sdgs'
	local indep law`i' regu`i' plan`i'
	qui reg `sdg' `indep' $convars i.ecoid#i.year
	estat vif
}

* estimate effects of various types policy
local sdgs y711 y712 y731 y7b1

eststo clear
forvalues i=1/4 {
	local sdg: word `i' of `sdgs'
	local indep law`i' regu`i' plan`i' other`i'
	eststo: qui xtscc `sdg' `indep' $convars i.ecoid#i.year, fe lag(0)
	estadd local fe "YES"
	estadd local year "YES"
}

esttab using "effect_policy_type.rtf", ///
	replace se eqlabels(none) nonotes nomtitles ///
	mlabels("Access to electricity(Indicator 7.1.1)" ///
	"Access to clean cooking (Indicator 7.1.2)" ///
	"Energy intensity (Indicator 7.3.1)" ///
	"Renewable electricity capacity (Indicator 7.b.1)") ///
	stat(fe year N r2_w, ///
	labels("Country FE" "Year FE" "Obs." "Adj. R{\super 2}") ///
	fmt(%10s %10s %9.0fc %9.3fc)) ///
	star(* 0.1 ** 0.05 *** 0.01) ///
	keep(*1 *2 *3 *4)
	

********************************************************************************
// Data 2: Counterfactual calculation on policy type
qui xtreg y711 law1 regu1 plan1 $convars i.ecoid#i.year, fe coefl
g b1 = _b[law1]
g b2 = _b[regu1]
g b3 = _b[plan1]
g exp_1 = b1 * law1
g exp_2 = b2 * regu1
g exp_3 = b3 * plan1
g rsdg711_law = SDG_711_raw * exp(-exp_1)
g rsdg711_reg = SDG_711_raw * exp(-exp_2)
g rsdg711_plan = SDG_711_raw * exp(-exp_3)
drop b1 b2 b3 exp_1 exp_2 exp_3

qui xtreg y712 law2 regu2 plan2 $convars i.ecoid#i.year, fe coefl
g b1 = _b[law2]
g b2 = _b[regu2]
g b3 = _b[plan2]
g exp_1 = b1 * law2
g exp_2 = b2 * regu2
g exp_3 = b3 * plan2
g rsdg712_law = SDG_712_raw * exp(-exp_1)
g rsdg712_reg = SDG_712_raw * exp(-exp_2)
g rsdg712_plan = SDG_712_raw * exp(-exp_3)
drop b1 b2 b3 exp_1 exp_2 exp_3

qui xtreg y731 law3 regu3 plan3 $convars i.ecoid#i.year, fe coefl
g b1 = _b[law3]
g b2 = _b[regu3]
g b3 = _b[plan3]
g exp_1 = b1 * law3
g exp_2 = b2 * regu3
g exp_3 = b3 * plan3
g rsdg731_law = SDG_731_raw * exp(-exp_1)
g rsdg731_reg = SDG_731_raw * exp(-exp_2)
g rsdg731_plan = SDG_731_raw * exp(-exp_3)
drop b1 b2 b3 exp_1 exp_2 exp_3

qui xtreg y7b1 law4 regu4 plan4 $convars i.ecoid#i.year, fe coefl
g b1 = _b[law4]
g b2 = _b[regu4]
g b3 = _b[plan4]
g exp_1 = b1 * law4
g exp_2 = b2 * regu4
g exp_3 = b3 * plan4
g rsdg7b1_law = SDG_7b1_raw * exp(-exp_1)
g rsdg7b1_reg = SDG_7b1_raw * exp(-exp_2)
g rsdg7b1_plan = SDG_7b1_raw * exp(-exp_3)
drop b1 b2 b3 exp_1 exp_2 exp_3

* Output predicted and true values into the spreadsheet
export excel nation year region income_group SDG_711_raw rsdg711_law rsdg711_reg rsdg711_plan SDG_712_raw rsdg712_law rsdg712_reg rsdg712_plan SDG_731_raw rsdg731_law rsdg731_reg rsdg731_plan SDG_7b1_raw rsdg7b1_law rsdg7b1_reg rsdg7b1_plan using "counterfact_policy_type.xlsx", firstrow(variables) replace

