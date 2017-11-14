***********SOME SETTINGS FOR STATA************
set more off
set matsize 10000
set seed 1234 //to always produce the same sequence of random numbers
cap mat drop pvalues


quietly forvalues i=1/1000 {
noi _dots `i' 0 0 //to display progress

	*****************SIMULATE DATA****************
	clear
	set obs 20 //20 schools
	gen school_id = _n
	gen school_unobservable = rnormal()^2 //Chi-squared

	expand 20 //20 classes per school = 400 classes
	gen class_unobservable = rnormal()^2 //Chi-squared
	sort school_id, stable
	gen class_id = _n
	gen treatment = mod(_n,2) //classes in odd rows are treated even rows are control


	expand 10 //10 students per  class =  4000 students
	
	gen testscores = class_unobservable + school_unobservable + 0*treatment + runiform()

	sort *_id, stable
	
	*****************ANALYZE DATA****************
	//plain difference in means
	regress testscores treatment
		quietly testparm treatment
		local pvalue1 = `r(p)'
	
	//adjusting standard errors
	regress testscores treatment, cluster(class_id)
		quietly testparm treatment
		local pvalue2 = `r(p)'

	//adding strata FEs
	regress testscores treatment i.school_id, cluster(class_id)
		testparm treatment
		local pvalue3 = `r(p)'

	//what if our sample was much smaller? (only one school, 8 classes, 80 students)
	keep if class_id<=8
	
	regress testscores treatment, cluster(class_id)
		testparm treatment
		local pvalue4 = `r(p)'
		
		mat pvalues=nullmat(pvalues)\(`pvalue1',`pvalue2',`pvalue3',`pvalue4')
}
clear
set obs 1000
svmat pvalues
set graphics off
hist pvalues1, name(a, replace) start(0) width(0.05)
hist pvalues2, name(b, replace) start(0) width(0.05)
hist pvalues3, name(c, replace) start(0) width(0.05)
hist pvalues4, name(d, replace) start(0) width(0.05)
set graphics on

graph combine a b c d 
