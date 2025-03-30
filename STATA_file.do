******************************************************************************
* Applied Econometrics Assignment	                                     *
* Group 5                                                                    *
* Economic Growth and Former Colonialization                                 *
* colonies.dta               	                                             *
*  	                                                                     *
******************************************************************************

******************************************************************************
* Create work environment                 				     *
******************************************************************************

* Clear the memory
clear

* Set working directory 
global path "/home/ali/Desktop/1st_emester/AE/project/STATA/"
cd "$path" 

* Create folders for project
cap mkdir orig // original data is stored here
cap mkdir log // log files are stored here
cap mkdir data // processed data is stored here
cap mkdir prog // do files are stored here

* Globals for paths
global orig "${path}/orig"
global data	"${path}/data"
global log 	"${path}/log"
global prog "${path}/prog" 
global outtex "${path}/latex"

* Open a log file (log file will be saved in folder "log")
capture log close
log using "$log/log_assignment.log", text replace

* import data 
use colonies.dta

*******************************************************************************
* Start with empirical analysis                                               *
*******************************************************************************
*******************************************************************************
* exercice 2        		                                              *
*******************************************************************************

* Summary statistics

* Create and export summary statistics table for continuous variables
estpost tabstat loggdppc1995 averagegovsh5 muslim80 catho80 protmg80 logavgcpinflat7098, c(stat) stat(mean sd min max n)
est store summary_stats

esttab summary_stats using "${outtex}/Basic_Summary_Stats.tex", replace ///
 cells("mean(fmt(2)) sd min(fmt(2)) max(fmt(2)) count(fmt(0))") ///
 nonumber ///
 line ///
 title("Basic Summary Statistics\label{tab1}") ///
 collabels("Mean" "SD" "Min" "Max" "N") ///
 coeflabels(loggdppc1995 "loggdppc1995" averagegovsh5 "averagegovsh5" muslim80 "muslim80" catho80 "catho80" protmg80 "protmg80" logavgcpinflat7098 "logavgcpinflat7098")

* Combine the two variable in one dummay
gen UK_origin =.
replace UK_origin = 1 if sjlouk == 1 
replace UK_origin = 0 if sjlofr == 1

* Create and export summary statistics for legal origin
estpost tabulate UK_origin
est store legal_origin_tabulate

esttab legal_origin_tabulate using "${outtex}/Legal_table.tex",replace ///
 cells("b(fmt(0)) pct(fmt(2)) cumpct(fmt(2))") ///
 nonumber ///
 line ///
 noobs ///
 title("Basic Summary Statistics for legal origin\label{tab1}") ///
 collabels("Freq" "Percent" "Cum") /// 
 coeflabels(1 "Uk legal origin" 0 "French legal origin")

* Ploting the Histograms
quietly sum logavgcpinflat7098
hist logavgcpinflat7098, normal xline(`r(mean)')
graph export "${data}/logavgcpinflat7098.jpg", replace

quietly sum averagegovsh5
quietly hist averagegovsh5, normal gap(5) xline(`r(mean)')
graph export "${data}/averagegovsh5.jpg", replace

quietly sum catho80
hist catho80, normal gap(5) xline(`r(mean)')
graph export "${data}/catho80.jpg", replace

quietly sum muslim80
hist muslim80, normal gap(5) xline(`r(mean)')
graph export "${data}/muslim80.jpg", replace

quietly sum protmg80
hist protmg80, normal gap(5) xline(`r(mean)')
graph export "${data}/protmg80.jpg",replace

quietly sum loggdppc1995
hist loggdppc1995, normal gap(5) xline(`r(mean)')
graph export "${data}/loggdppc1995.jpg",replace

* Make the variable for calcualte the majority of each relegion in each country
gen relegion_majority =.
label variable relegion_majority "Catholic = 0, Protestant = 1, Muslim = 2"	
label def majority_description 0 "Catholic" 1 "Protestant" 2 "Muslim"
label values relegion_majority majority_description

* Calcualte the majority of each country
replace relegion_majority = 0 if catho80 == max(catho80, protmg80, muslim80)
replace relegion_majority = 1 if protmg80 == max(catho80, protmg80, muslim80)
replace relegion_majority = 2 if muslim80 == max(catho80, protmg80, muslim80)

* Create a pie chart
tabulate relegion_majority
graph pie, over(relegion_majority) ///
plabel(_all percent, color(black) size(medium) format(%9.1f)) 
graph export "${data}/majority.jpg", replace

* Estimating the correlation between the variables
estpost correlate loggdppc1995 averagegovsh5 muslim80 catho80 protmg80 logavgcpinflat7098, matrix listwise
esttab using "${outtex}/Corr_mat.tex",  title("Correlation Matrix of Variables")  unstack not noobs compress replace

*******************************************************************************
* exercice 3        		                                              *
*******************************************************************************
* Globals for dependent variable
global dep "loggdppc1995"

*check the outliers
scatter $dep averagegovsh5
graph export "${data}/scatter_1.jpg", replace

* Outliers are Israel and Saudi Arabia
* Deleting outliers
gen outliers=0
replace outliers =1 if country == "Israel"
replace outliers =1 if country == "Saudi Arabia"
replace outliers =1 if country == "Oman"

* Globals for independent variables
* Generate Government share as percentage for easier intrepetation
gen averagegovsh_percent =averagegovsh5*100
global indep "UK_origin averagegovsh_percent muslim80 catho80 protmg80 "

* Estimate the baseline specefication using OLS
eststo OLS: reg $dep $indep if outliers==0
* Export the results
esttab OLS using "${outtex}/Main_OLS.tex", se r2 ar2 title("OLS estimation") replace

* Test the joint significance of the relegion variables
test muslim80 catho80 protmg80
mat Wald = (r(F),r(df),r(df_r),r(p))
matrix rownames Wald = Wald_Test
matrix colnames Wald = F df df_r p
esttab matrix(Wald) using "${outtex}/Wald.tex", replace nodepvar title("Results of The Joint Significance Test(muslim80 = 0, catho80 = 0, protmg80 = 0)")

*******************************************************************************
* exercice 4       		                                              *
*******************************************************************************
* Run the RESET Test on the model
quietly reg $dep $indep if outliers==0
estat ovtest 
mat RESET_1 = (r(F),r(df),r(p))
matrix rownames RESET_1 = RESET
matrix colnames RESET_1 = F df p
esttab matrix(RESET_1) using "${outtex}/RESET_1.tex", replace nodepvar title("Results of The RESET Test")

* Globals for additional variables (we inserted a space before addtional global for correct form in regression)
global additional " logavgcpinflat7098"
eststo OLS_with_CPI: reg $dep $indep $additional if outliers==0
esttab OLS_with_CPI using "${outtex}/second_OLS.tex", se r2 ar2 title("OLS estimation of the second model including log CPI") replace
estat ovtest
mat RESET_2 = (r(F),r(df),r(p))
matrix rownames RESET_2 = RESET
matrix colnames RESET_2 = F df p
esttab matrix(RESET_2) using "${outtex}/RESET_2.tex", replace nodepvar title("Results of The RESET Test for The Second Model")

* Get the residuals and plot them
predict resid, residuals
quietly sum resid
hist resid, normal xline(`r(mean)')
graph export "${data}/resid.jpg", replace
*swilk resid

* Comparing the models
esttab OLS OLS_with_CPI  using "$outtex/model_selection.tex",title("Comparing All Models") ar2 aic bic replace 

* Robustness check
eststo OLS_Robustness: reg $dep $indep $additional 
esttab OLS_Robustness using "${outtex}/OLS_Robustness.tex", se r2 ar2 title("OLS estimation of the second model including outliers") replace


*******************************************************************************
* exercice 5       		                                              *
*******************************************************************************

* Perform white test
quietly reg $dep $indep $additional if outliers==0
estat imtest, white
mat White = (r(chi2),r(df),r(p))
matrix rownames White = Heteroskedasticity
matrix colnames White = chi2 df p
esttab matrix(White) using "${outtex}/White.tex", replace nodepvar title("Results of The White Test")

* Plot residuals vs each independent variable
rvpplot logavgcpinflat7098, name(res_cpi, replace) 
rvpplot averagegovsh_percent, name(res_gov, replace) 
graph combine res_gov res_cpi,  iscale(0.5)
graph export "${data}/rvpplot.jpg", replace





