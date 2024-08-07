cap program drop _ex_drdecomp
program define _ex_drdecomp, rclass

syntax, example(numlist)

preserve
if ( "`example'" == "1") {
	clear all
	datalib, country(pry) year(1999 2009) type(cedlas) incppp(ipcf) append
	keep if ipcf!=. & cohh==1
	
	drdecomp ipcf_ppp [w=pondera], by(ano) varpl(lp_4usd_ppp) in(fgt0 fgt1 fgt2) mpl(1 2.5 12.5)
}
restore
end
