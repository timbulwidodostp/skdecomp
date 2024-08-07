*! version 1.0  Aug2012
*! Viviane Sanfelice 

cap program drop drdecomp
program define drdecomp, rclass sortpreserve byable(recall)
	version 10.0, missing
	if c(more)=="on" set more off
	local version : di "version " string(_caller()) ", missing:"
	syntax varlist(numeric max=1 min=1) [if] [in] [aweight fweight], by(varname numeric)    ///
			varpl(varname numeric)                                              ///
			[mpl(numlist sort) INdicator(string)]	       
			            		                                 
	tempvar  varname2
	marksample touse
	
	preserve
	
	* * Indicator
	if ("`indicator'"=="") {
		local fgt0 "fgt0"
	}
	else {
		local t 0
		forvalues i = 0(1)2 {
			local t = regexm("`indicator'","fgt`i'") + `t'
		}
		if `t'==0 {
			di in red "Indicator not valid"
			exit 198
		}	
		if regexm("`indicator'","fgt0")!=0 local fgt0 "fgt0"
		if regexm("`indicator'","fgt1")!=0 local fgt1 "fgt1"
		if regexm("`indicator'","fgt2")!=0 local fgt2 "fgt2"
	}	
	
	
	* ** Weights
	if ("`weight'"!="") {
		local weight "[`weight'`exp']"				
		* local wvar : word 2 of `exp'
	}
	
	* ** Comparison variable
	cap tab `by' if `touse', matrow(temp)
	local ct = r(N)
	if r(N)==0 error 2000	
	if (`r(r)'~=2)|(_rc==134) {
		di in red "Only 2 groups allow" 
		exit 198
	}		
	local c = temp[1,1]
	local d = temp[2,1]
	
	local ing1 "`varlist'"
	local ing2 "`varname2'"	
	cap mat drop PATH
	
		* Generate the varibles that has the same distribution, but have the average from another year
			cap gen double `ing2' = .
			cap sum `ing1' `weight' if `by'==`c' & `touse', meanonly
			local m1=r(mean) 

			cap sum  `ing1' `weight' if `by'==`d' & `touse', meanonly
			local m2=r(mean)

			cap replace `ing2'=`ing1'*(`m2'/`m1') if `by'==`c'
			cap replace `ing2'=`ing1'*(`m1'/`m2') if `by'==`d'
		
						
				
			* **=== Datt-Ravallion Decomposition ===***
			
			**** Poverty
			foreach year in `c' `d'{
				foreach income in `ing1' `ing2' {
					_mpl `income' `weight' if (`by'==`year' & `touse') , varpl(`varpl') mpl(`mpl') in(`fgt0' `fgt1' `fgt2')
					mat a = r(b)
					mat `income'_`year' = a[1...,3] 
						
				}
			}
				
			mat A = ( `ing1'_`c', `ing2'_`c', `ing2'_`d', `ing1'_`d')	
			mat B = a[1...,1]	
			mat C = a[1...,2]
			
			* * Matrix of the three effects: Total, Growth and Distribution respectively
				
			mat PATH = nullmat(PATH) \ /*
				*/	( J(rowsof(A), 1, 1), C, B, A[1...,2] - A[1...,1], A[1...,4] - A[1...,3] \ /* Growth
				*/    J(rowsof(A), 1, 2), C, B, A[1...,3] - A[1...,1], A[1...,4] - A[1...,2] \ /* Distribution
				*/    J(rowsof(A), 1, 3), C, B, A[1...,4] - A[1...,1], A[1...,4] - A[1...,1] ) /* Total effect */
					
			
	
			
			mat colnames PATH = effect povertyline indicator effect1 effect2
			cap	drop _all
			cap svmat double PATH, n(col)	
			
				
			label define indicator 0 "FGT0" 1 "FGT1" 2 "FGT2" 
			label values indicator indicator
			
			label define effect 3 "Total change"  1 "Growth" 2 "Redistribution" 
			label values effect effect
			
			egen effect_avg=rowmean(effect1 effect2)
						
			if "`mpl'"!="" {
				local pl "povertyline"		
				label var `pl' "Multiples of poverty line"
				cap tab `pl', matrow(a)
				local m = `r(r)'-1
				label define `pl' `r(r)' "max"
				forvalues i =1(1)`m' {
					local j : word `i' of `mpl' 
					label define `pl' `i' "`j'", add	
				}
				label values `pl' `pl'
				
			}
			di as txt _new "Datt-Ravallion decomposition of change in poverty from `c' to `d', based on the `varlist' variable"	
			
			tabdisp effect indicator, cell(effect_avg) format(%12.2fc) by(`pl')
			
			sort `pl' indicator effect, stable
			mkmat `pl' indicator effect effect1 effect2 effect_avg, matrix(a) 
			mkmat `pl' indicator effect effect_avg, matrix(b) 
			
			return matrix b = b
			return matrix shapley = a
			
			clear
			
end






* ** Another program need to calculate poverty
cap program drop _mpl
program define _mpl, rclass sortpreserve byable(recall)
	version 10.0, missing
	if c(more)=="on" set more off
	local version : di "version " string(_caller()) ", missing:"
	syntax varlist(numeric min=1 max=1) [if] [in] [aweight fweight], [varpl(varname numeric) LINEs(numlist sort) mpl(numlist sort) INdicator(string)]	       
			            		                                 
	tempvar  w wwvar point0 point1 aux
	marksample touse
	
	qui {	
		
		* * Indicator
		if regexm("`indicator'","fgt0")!=0 local fgt0 "fgt0"
		if regexm("`indicator'","fgt1")!=0 local fgt1 "fgt1"
		if regexm("`indicator'","fgt2")!=0 local fgt2 "fgt2"
		
		* * Weight variable
		if ("`weight'"=="") {
			gen `w' = 1
			local wvar "`w'"
		}	
		else {
			local weight "[`weight'`exp']"				
			local wvar : word 2 of `exp'
		}
		sum `wvar'  if `touse' , meanonly
		local pop_sum = r(sum)
		local t_obs = r(N)
		gen double `wwvar' = `wvar'/`pop_sum'
		
		
		* * varpl
		if ("`varpl'"=="") {
			tempvar varpl
			gen byte `varpl' = 1
			local mpl "`lines'"
		}	
		if ("`mpl'"=="") local mpl "1"
		
		
		* * Points
		sum `varlist' if `touse' , meanonly
		local min `r(min)'
		local max `r(max)'
		
		gen double `point0' = .
		gen double `point1' = .
		gen double `aux' = .
		local list "`min' `mpl'"
		
		local signal "<"
		
		cap mat drop fgt
		cap mat drop fgt0
		cap mat drop fgt1
		cap mat drop fgt2
		
		local cont 0
		foreach pt1 in `mpl' `max' {
			
			local ++cont
			local pt0 : word `cont' of `list'
				
			replace `point0' = `pt0'*`varpl'
			replace `point1' = `pt1'*`varpl'
			
			if 	"`pt1'"=="`max'" {
				replace `point1' = `max'
				local signal "<="
			}			
			
			* Observation on range
			cap sum `varlist' if (`varlist'>=`point0' & `varlist'<=`point1') & `touse' , meanonly
				
			if (`r(N)'<=30) {
				di in red "The range used must have more than 30 observations" _new
				exit 198
			}	
		
			/* Indicators Calculation */
					
					
			* ** fgt0					
				
			tempvar in
			gen double `in' = (`varlist'>=(`point0') & `varlist'`signal'(`point1')) if `touse'
			if("`fgt0'"~="") {	
				sum `wvar' if `touse' & `in'==1, meanonly
				local pop_sum_in `r(sum)'
				local fgt0 = 100*`pop_sum_in'/`pop_sum'
													
				mat fgt = nullmat(fgt) \ (0, `cont', `fgt0')
			}			
			* ** fgt1
			if("`fgt1'"~="") {		
				replace `aux' = 100*`in'*`wwvar'*(((`point1')-`varlist')/(`point1')) if `touse' 
				sum `aux'  if `touse' , meanonly
				local fgt1  `r(sum)'
							
				mat fgt = nullmat(fgt) \ (1, `cont', `fgt1')
			}
						
			
			* ** fgt2
			if("`fgt2'"~="") {					
				replace `aux' = 100*`in'*`wwvar'*((((`point1')-`varlist')/(`point1'))^2) if `touse' 
				sum `aux'  if `touse' , meanonly
				local fgt2 `r(sum)'
						
				mat fgt = nullmat(fgt) \ (2, `cont', `fgt2')	
			}	
		}	
		
	}

		 
*** Save results			
return matrix b = fgt
	
end	


