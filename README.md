# CPRD
Stata code for data analysis
set more off
cd D:\Covid\
 
 local casepath D:\Covid\Cases\
 local codepath D:\Codes\
 local datapath D:\Covid\

log using D:\Covid\Analysis.log, replace

*FIND PATIENT START AND END DATES  // Dataset includes all SMI and non-SMI patients from the CPRD with a COVID-19 infection diagnosis between 01/02/2020 and 31/12/2021

use  `casepath'Patients, clear
merge m:1 pracid using `casepath'Practice
drop _m
gen dod=date(cprd_ddate,"DMY")
gen dor=date(regstartdate,"DMY")
gen dot=date(regenddate,"DMY")
*gen douts=date(uts,"DMY")
gen dolcd=date(lcd,"DMY")
gen studyend=date("31032021","DMY")
gen studystart=date("01022020","DMY")
gen patstart=max(dor,studystart)
gen patend=min(dot,dolcd,studyend,dod)
*replace yob=yob+1800
drop if patend<=patstart
recode region (7/8=0) (1/3=1) (4/5=2) (6=3) (9=4) (12=5), gen(Region)
keep patid pracid patstart patend studyend studystart gender yob dod  Region
merge 1:1 patid using `casepath'Indexdate_Covid.dta, keep(matched)
drop _m
gen Age_Covid=year(Indexdate_Covid)- yob
save `casepath'PatCovid, replace


 
*FIND Covid EVENTS
forvalues i=1/134 {
	use `casepath'Observation`i', clear
	keep patid obsdate enterdate medcodeid consid obsid probobsid
    merge m:1 medcodeid using `codepath'Covidcodes.dta, keep (matched)
	drop _m
    save temp`i', replace
}


use temp1, clear

forvalues i=2/134 {
         append using temp`i'
 }
 
save `casepath'Medfile, replace
gen doe=date(obsdate,"DMY")
replace doe=date(enterdate, "DMY") if doe==.
collapse (min) Indexdate_Covid=doe, by(patid) 
save `casepath'MedCovid.dta, replace

forvalues i=1/134 {
         erase temp`i'.dta
 }

 
*FIND SMI EVENTS 

local conditionlist SMI

foreach condition of local conditionlist {
	forvalues i=1/134{
		use `casepath'\Observation`i', clear
		keep patid medcodeid obsdate enterdate value 
		merge m:1 medcodeid using `codepath'\`condition'codes, keep(matched)
		drop _m
		save temp`i', replace
	}
	use temp1, clear
	forvalues i=2/134{
	   append using temp`i'
	}
	save `casepath'\`condition', replace

}
forvalues i=1/134 {
	erase temp`i'.dta
}

foreach condition of local conditionlist {
	use `casepath'\`condition', clear
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.|doe==0
	collapse (min) start`condition'=doe, by(patid)
	gen `condition'=start`condition'!=.
	save `casepath'\Pat`condition', replace
}


*CONFOUNDERS
 
 
 * Body mass index
 
forvalues i=1/134{
	use `casepath'Observation`i', clear
	merge m:1 medcodeid using `codepath'BMIcodes, keep (matched)
	replace BMI=value if BMI==1 
	replace BMI=. if BMI<12|BMI>70
	replace Weight=value if (Weight==1 & (value>=30 & value<=200))
    replace Height=value/100 if Height==1 & value>100&value!=.
	replace Height=. if Height<1.2|Height>2.2
	bysort patid: egen meanHeight=mean(Height)
	replace Height=meanHeight if Height==.
	replace BMI=Weight/(Height^2) if BMI==.
	replace BMI=. if BMI<12|BMI>70
	replace BMIcat=1 if BMI<16.5
	replace BMIcat=1 if BMI<16.5
	replace BMIcat=2 if BMI>=16.5 &BMI<24.9999
	replace BMIcat=3 if BMI>=25 &BMI<29.999
	replace BMIcat=4 if BMI>=30 &BMI<.
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe BMI BMIcat 
	save `datapath'temp`i', replace
}

use `datapath'temp1, clear
forvalues i=2/134 {
	append using `datapath'temp`i'
}

collapse (mean) BMI (max) BMIcat , by(patid doe)
	recode BMI (min/18.4999999 = 1 "Underweight 1<18.5 kg/m2") (18.5/24.9999999 = 2 "Under/Normal weight 18.5-24.9999999 kg/m2") (25/30 = 3 "Overweight 25-30 kg/m2")   (30.00001/max = 4 "Obese >30 kg/m2"), gen(BMIgrp)
	label var BMIgrp "BMI categories at baseline (grouped)"
save `casepath'\PatBMIgrp, replace

forvalues i=1/134 {
	erase `datapath'temp`i'.dta
}


*SMOKING Status

forvalues i=1/134{
	use `casepath'Observation`i', clear
	merge m:1 medcodeid using `codepath'Smokercodes.dta, keep(matched)
	gen SmokerCigg=value if value>0 & value<121
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	*replace Smoker=3 if (Smoker==2 & (SmokerCigg>=11 & SmokerCigg<20))|SmokerType==3
	*replace Smoker=4 if (Smoker==2 & (SmokerCigg>=20 & SmokerCigg<40)) |SmokerType==4
	*replace Smoker=5 if (Smoker==2 & (SmokerCigg>=40 & SmokerCigg<121)) |SmokerType==5
	keep patid doe Smoker SmokerCigg
	save temp`i', replace

}
use temp1, clear
forvalues i=2/134{
	append using temp`i'
}

recode Smoker (0 = 0 "Never Smoked") (1 = 1 "Ex-Smoker") (2= 2 "Current Smoker") (3= 3 "Smoker < 20 ciggarettes") (4= 4 "Smoker 20-40 ciggarettes") (5= 5 "Smoker >40 ciggarettes") (. = 6 "Smoking data missing"), gen(SmokingStatus)
	drop Smoker
	rename SmokingStatus Smoker
	bysort patid doe: keep if [_n]==[_N]
	save `casepath'PatSmoker, replace


forvalues i=1/134{
	erase temp`i'.dta
}



*FIND ALCOHOL status	

forvalues i=1/134 {
	use `casepath'Observation`i', clear
	merge m:1 medcodeid using `codepath'Alcoholcodes.dta, keep(matched)
     gen Freq=0 
	replace Freq=1 if Alcohol==1
	replace Freq=2 if Alcohol==2
	replace Freq=3 if Alcohol==3
	replace Freq=4 if Alcohol==4
	replace Freq=5 if Alcohol==5 
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe Freq Alcohol
	save temp`i', replace
}
use temp1, clear
forvalues i=2/134 {
	append using temp`i'
}

	bysort patid doe: keep if [_n]==[_N]
	recode Alcohol (0 = 0 "Never drunk") (1 = 1 "Ex-Drinker")(2/5= 2 "Current Drinker") (3= 3 "Mild drinker") (4= 4 "Moderate drinker") (5= 5 "Excessive 	drinker") (. = 6 "Alcohol data missing"), gen(AlcoholStatus)
	drop Alcohol
	rename AlcoholStatus Alcohol
	save `casepath'PatAlcohol, replace

forvalues i=1/134{
	erase temp`i'.dta
}



*FIND BLOOD PRESSURE levels

forvalues i=1/134 {
	use `casepath'Observation`i', clear
	merge m:1 medcodeid using `codepath'BPcodes.dta, keep(matched)
	*keep if value!=.
	gen Systolic=value if SBP==1
	gen Diastolic=value if DBP==1
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe Diastolic Systolic 
	save temp`i', replace
}
use temp1, clear
forvalues i=2/134 {
	append using temp`i'
}

collapse (mean) Diastolic Systolic, by(patid doe)
	gen BP=1 if Diastolic<80&Systolic<120
	replace BP=2 if Diastolic>=80 & Diastolic<90|Systolic>=120 & Systolic<140
	replace BP=3 if Diastolic>=90 &Diastolic<.|Systolic>=140 & Systolic<.
	*replace BPgrp=6 if Diastolic==.|Systolic==.
	recode BP (1 = 1 "Optimal BP")(2= 2 "Pre-hypertension") (3= 3 "Hypertension") (. = 4 "BP data missing"), gen(BPgrp)
save `casepath'PatBPgrp, replace

forvalues i=1/134 {
	erase temp`i'.dta
}


*FIND Total cholesterol levels
forvalues i=1/134{
	use `casepath'\Observation`i', clear
	merge m:1 medcodeid using `codepath'Cholesterolcodes.dta, keep(matched)
	*keep if value!=.
	drop Cholesterol
	gen Cholesterol=value if TC==1 
	replace Cholesterol=. if Cholesterol>15|Cholesterol<=0
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe Cholesterol 
	save temp`i', replace
}
use temp1, clear
forvalues i=2/134 {
	append using temp`i'
}

collapse (mean) Cholesterol , by(patid doe)
	gen Cholesterolgrp=1 if Cholesterol<5.15
	replace Cholesterolgrp=2 if Cholesterol>=5.15  & Cholesterol<6.21
	replace Cholesterolgrp=3 if Cholesterol>6.20 & Cholesterol<.
	keep patid doe Cholesterolgrp HDLgrp LDLgrp nonHDLgrp Cholesterol LDL HDL TCratioHDL non_HDL
	recode Cholesterolgrp (1 = 1 " Total cholesterol <5.15")(2= 2 "Total cholesterol>=5.15 & <6.21") (3= 3 "Total cholesterol >6.20") (. = 4 "Total cholesterol data missing"), gen(TotalCholesterol)
	drop Cholesterolgrp
	rename TotalCholesterol Cholesterolgrp
save `casepath'PatCholesterolgrp, replace

forvalues i=1/134 {
	erase temp`i'.dta
}



* Find triglycerides levels

forvalues i=1/134 {
	use `casepath'Observation`i', clear
	merge m:1 medcodeid using `codepath'Triglyceridescodes.dta, keep(matched)
	keep if value!=.
	replace Triglycerides=value if value>0 & value<501
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe Triglycerides
	save temp`i', replace
 }

use temp1, clear
forvalues i=2/134 {
	append using temp`i'
}

collapse (mean) Triglycerides, by(patid doe)
gen Triglyceridesgrp=(Triglycerides>1.7 &Triglycerides<. )
keep patid doe Triglyceridesgrp Triglycerides
save `casepath'PatTriglycerides, replace

forvalues i=1/134 {
	erase temp`i'.dta
}

*FIND HbA1c levels
forvalues i=1/134 {
	use `casepath'Observation`i', clear
	merge m:1 medcodeid using `codepath'HbA1ccodes.dta, keep(matched)
	*keep if value!=. 
	replace HbA1c=value
	replace HbA1c=(46.7+value)/28.7 if numunitid==220|numunitid==892
	replace HbA1c=. if HbA1c>15 & HbA1c<.
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe HbA1c HbA1clevel
	save temp`i', replace
}
use temp1, clear
forvalues i=2/134 {
	append using temp`i'
}


collapse (mean) HbA1c (max) HbA1clevel, by(patid doe)
	gen HbA1cgrp=1 if HbA1c<5.7
	replace HbA1cgrp=2 if HbA1c>=5.7  & HbA1c<6.5
	replace HbA1cgrp=3 if HbA1c>=6.5 & HbA1c<.
	*replace HbA1cgrp=6 if HbA1c==.
	keep patid doe HbA1cgrp HbA1c
	recode HbA1cgrp (1 = 1 " HbA1c <5.7")(2= 2 "HbA1c>=5.7 & <6.5") (3= 3 "HbA1c >=6.5") (.= 4 "HbA1c data missing"), gen(HbA1clevel)
	save `casepath'PatHbA1cgrp, replace


forvalues i=1/134 {
	erase temp`i'.dta
}

*FIND CRP levels
forvalues i=1/134 {
	use `casepath'Observation`i', clear
	merge m:1 medcodeid using `codepath'CRPcodes.dta, keep(matched)
	keep if value!=.
	replace CRP=value
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe CRP  
	drop if CRP<0|CRP>250 
	save temp`i', replace
}

use temp1, clear	
forvalues i=2/134{
	append using temp`i'
}

   collapse (mean) CRP , by(patid doe)
	*replace CRPgrp=6 if CRP==.
	xtile CRPtertile=CRP, nq(3)
	recode CRP (0/1.000=0) (1.0001/2.99999=1) (3.0001/10.0000=2) (10.0001/max=3), gen(CRPgrp)
	keep patid doe CRP CRPgrp CRPtertile
save `casepath'PatCRP, replace

forvalues i=1/134 {
	erase temp`i'.dta
}

*Find Ethnicity 

  forvalues i=1/134 {
	use `casepath'Observation`i', clear
	merge m:1 medcodeid using `codepath'Ethnicitycodes, keep(matched)
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe Ethnicity Ethnicgrp term 
	save temp`i', replace
}

use temp1, clear
forvalues i=2/134{
	append using temp`i'
}

collapse (max) Ethnicity Ethnicgrp, by(patid doe)
save `casepath'\PatEthnicity, replace

forvalues i=1/134{
	erase temp`i'.dta
}


* FIND RELEVANT LONG-TEMR CONDITIONS/

local medlist Neoplasm Hypertension MI IHD Stroke CKD Liver T2DM COPD Asthma Autoimmune SUD Epilepsy Depression Anxiety EatingDisorder GERD Alzheimer SMI Thyroid Learningdisab ICU



foreach med of local medlist {
  forvalues i=1/134 {
	use `casepath'Observation`i', clear
	merge m:1 medcodeid using `codepath'\`med'codes, keep(matched)
	gen doe=date(obsdate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe `med' 
	save temp`i', replace
}
use temp1, clear

forvalues i=1/134{
	append using temp`i'
}

collapse (max) `med' , by(patid doe)
save `casepath'\Pat`med', replace


forvalues i=1/134{
	erase temp`i'.dta
}

}


*FIND drug prescriptions

local druglist Immunological Corticosteroids Statins AHT NSAID Antidepressant Antipsychotic Anticoagulant Antiplatelet


foreach drug of local druglist {
  forvalues i=1/79 {
	use `casepath'Therapy`i', clear
	merge m:1 prodcodeid using `drugpath'\`drug'codes, keep(matched)
	gen doe=date(issuedate,"DMY")
	gen sd=date(enterdate,"DMY")
	replace doe=sd if doe==.
	keep patid doe `drug'
	save temp`i', replace
}
use temp1, clear

forvalues i=1/79{
	append using temp`i'
}

collapse (max) `drug', by(patid doe)
save `casepath'Pat`drug', replace


forvalues i=1/79{
	erase temp`i'.dta
}

}



local catvarlist BMIgrp BPgrp Cholesterolgrp Smoker Alcohol HbA1cgrp CRP Ethnicity 
local druglist Immunological Corticosteroids Statins AHT NSAID Antidepressant Antipsychotic Anticoagulant Antiplatelet
local medlist Neoplasm Hypertension MI IHD Stroke CKD Liver T2DM COPD Asthma Autoimmune SUD Epilepsy Depression Anxiety EatingDisorder GERD Alzheimer SMI Thyroid Learningdisab ICU

local confounderlist `druglist' `medlist' `catvarlist'

*FIND EVENT NEAREST TO BASELINE (Covid-19 infection date - ensures that SMI diagnosis precededed a COVID-19 infection)

foreach confounder of local confounderlist {
  display "`confounder'"
  use `casepath'PatCovid, clear
  collapse (min) Indexdate_Covid dod patstart patend yob gender Region, by(patid)
  merge 1:m patid using `casepath'Pat`confounder', keep(matched)
  drop _merge
  drop if doe>Indexdate_Covid
  gen diff=abs(Indexdate_Covid-doe)
  bysort patid: egen mindiff=min(diff)
  keep if diff==mindiff
  *bysort patid: gen n=[_N]
  *drop if doe>Indexdate_Covid&n>1
  bysort patid: keep if [_n]==1
  keep patid doe `confounder'*
  save `casepath'\`confounder', replace
 }
 
 
 *MERGE CONFOUNDERS
	use `casepath'PatCovid, clear
	collapse (min) Indexdate_Covid dod patstart patend yob gender Region, by(patid)
	keep patid 
	foreach confounder of local confounderlist {
		display "`confounder'"
		merge 1:1 patid using `casepath'\`confounder'
		drop _merge
		
		}
		
		foreach drug of local druglist {
		replace `drug'=0 if `drug'==.
	}
	
	 foreach med of local medlist {
		replace `med'=0 if `med'==.
	}
	
	save `casepath'\AllConfounders, replace

	
	
	
	
	* STATISTICAL ANALYSIS

	* create a dataset for analysis by bringing together cases and matched controls, including relevant confounders
	
use `casepath'PatCovid, clear 
merge 1:1 patid using `casepath'\AllConfounders
drop _m
merge 1:1 patid using `casepath'imd2015_5.dta // linkage to patient-derived Index of Multiple Deprivation file provided by the CPRD
drop _m
save `casepath'\AllPatConfounders, replace

		
* Create a continuous and a binary multimorbidity variable from multiple long-term conditions

egen MM=rowtotal(Neoplasm Hypertension MI IHD Stroke CKD Liver T2DM COPD Asthma Autoimmune SUD Epilepsy Depression Anxiety EatingDisorder GERD Alzheimer SMI Parkinson Thyroid Learningdisab)

recode MM (0=0) (1=1) (2/max=2), gen(MM_binary)


* Multiple imputation - included a wider range of variables that might influence the relationships between different study variables and the study outcome

	
	mi set wide
	mi register imputed BMIgrp BPgrp Cholesterolgrp Smoker Alcohol Ethnicity CRPgrp imd2015_5
	mi register regular Age_Covid gender Neoplasm Hypertension MI IHD Stroke CKD Liver T2DM COPD Asthma Autoimmune SUD Epilepsy Depression Anxiety EatingDisorder GERD Alzheimer SMI Thyroid Learningdisab ICU Immunological Corticosteroids Statins AHT NSAID Antidepressant Antipsychotic Anticoagulant Antiplatelet Region MM_binary MM Death
	set seed 912346 
	mi impute chained (mlogit) Alcohol Smoker Ethnicity (ologit) BPgrp BMIgrp Cholesterolgrp CRPgrp imd2015_5 =Age_Covid gender Neoplasm Hypertension MI IHD Stroke CKD Liver T2DM COPD Asthma Autoimmune SUD Epilepsy Depression Anxiety EatingDisorder GERD Alzheimer SMI Thyroid Learningdisab ICU Immunological Corticosteroids Statins AHT NSAID Antidepressant Antipsychotic Anticoagulant Antiplatelet Region MM_binary MM Death , add(20) force augment 
	save `datapath'\MIPatCovid.dta, replace
	
	
	
* Main survival analysis syntax for predicting all-cause mortality 

					use `datapath'MIPatCovid.dta, clear
					gen ageDeath=year(dod)-yob
					gen Death_date=dod 
					gen Covidstart=date("01022020","DMY")
					*gen FirstCovid=date("30092020","DMY") // this is the end date of the first wabe of the COVID-19 pandemic
					*gen SecondCovid=date("01102020","DMY") // use this date for start time definition on analyses during the second wave
				    gen starttime=max(Covidstart,patstart)
					gen exittime=min(Death_date,patend) 
					*gen exittime=min(Death_date,patend, FirstCovid) // algorithm for determining the exit time of follow-up for the first wave analyses
					*gen starttime=max(SecondCovid, patstart) // algorithm for determining the start time of follow-up in the second wave analyses 
                    drop if starttime>=exittime
                    drop if Death_date<=starttime
				    replace Death_date=date("31122022","DMY") if Death_date==. //create an arbitray date for patients that were right censored
				    mi stset Death_date, failure(Dead) origin(starttime) exit(exittime) id(patid)
					sts graph,by(SMI) ci risktable(, order(1 "No SMI" 2 "SMI") rowtitle(, justification(left))) legend(ring(0) position(2) rows(2))
				    sts graph,by(SMI) adjustfor(gender BMIgrp Smoker Ethnicity Region Neoplasm Liver IHD MI Stroke T2DM CKD COPD Asthma Autoimmune Epilepsy Depression Anxiety GERD EatingDisorder Alzheimer SUD ICU Immunological Corticosteroid)
					strate SMI, per(1000)
					mi estimate, hr: stcox SMI gender Age_Covid , vce(robust)
					parmest, eform saving(`casepath'\AgeGender_adjusted, replace
					estat phtest, de
					mi estimate, hr: stcox SMI Age_Covid ib1.BMIgrp i.(gender Hypertension Smoker Liver Neoplasm IHD MI Stroke T2DM CKD Epilepsy COPD Autoimmune Asthma Depression Anxiety GERD EatingDisorder Alzheimer SUD Immunological Corticosteroid ICU Ethnicity Region), vce(robust) 		
					parmest, eform saving(`casepath'\AdjustedResults, replace)
					mi estimate: estat phtest, de
				
				
* MM macthed case-control study	- sensitivity analyses to reduce disparities in LTC between SMI and non SMI groups

					use `datapath'MIPatCovid.dta, clear
					gen Covidstart=date("01022020","DMY")
					mi stset Covidstart , failure(SMI) scale(365.25) id(patid)
					set seed 06072008
					sttocc, match(gender Age_Covid patstart MM) n(5) nodots
					save `datapath'MatchedCovid, replace
					gen ageDeath=year(dod)-yob
					gen Death_date=dod 
					gen starttime=max(Covidstart,patstart)
					gen exittime=min(Death_date,patend) 
					drop if starttime>=exittime
                    drop if Death_date<=starttime
				    replace Death_date=date("31122022","DMY") if Death_date==.
				    mi stset Death_date, failure(Dead) origin(starttime) exit(exittime) id(patid)
					strate, per(1000)
					sts graph,by(_case) ci risktable(, order(1 "No SMI" 2 "SMI") rowtitle(, justification(left))) legend(ring(0) position(2) rows(2))
				    sts graph,by(_case) adjustfor(gender BMIgrp Smoker Ethnicity Region Neoplasm Liver IHD MI Stroke T2DM CKD COPD Asthma Autoimmune Epilepsy Depression Anxiety GERD EatingDisorder Alzheimer SUD ICU Immunological Corticosteroid)
					mi estimate, hr: stcox _case gender Age_Covid , vce(robust)
					parmest, eform saving(`datapath'MatchedAgeGenderadjusted, replace)
					estat phtest, de
					mi estimate, hr: stcox _case Age_Covid i.b1.BMIgrp i.(gender Hypertension Smoker Liver Neoplasm IHD MI Stroke T2DM CKD Epilepsy COPD Autoimmune Asthma Depression Anxiety GERD EatingDisorder Alzheimer SUD Immunological Corticosteroid ICU Ethnicity Region), vce(robust) 		
					parmest, eform saving(`datapath'MatchedadjustedFull, replace)
					*estat phtest, de
				}
	
