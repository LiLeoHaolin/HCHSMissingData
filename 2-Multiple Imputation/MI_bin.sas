proc mi data=samp_wide1 seed=12345 nimpute=5 out=midata;
	by dat_num;
    where y1_bin_gfr = 0;
	class y_egfr_mcar_30;
	var y_egfr_mcar_30 x6_mcar_30 x2 x8 x7 y1_con_bmi_v4 str1 str2 str3 age_base; * auxiliary variables and covariates;
	fcs logistic(y_egfr_mcar_30) reg(x6_mcar_30);
run;

proc surveylogistic data=midata;	 
	by dat_num _imputation_;
	strata strat; 
	cluster bgid; 
	weight bghhsub_s2;		
	domain y1_bin_gfr;
	model y_egfr_mcar_30 (descending) =x6_mcar_30 x7 y1_con_bmi_v4 / clparm; 
	ods output ParameterEstimates=betas CLparm=ci DomainSummary=nobs;
run;

data beta;
	merge betas(drop=df waldchisq probchisq domain where=(y1_bin_gfr=0)) ci(drop=estimate domain where=(y1_bin_gfr=0));
	by dat_num _imputation_;
run;

proc mianalyze parms=beta;
	by dat_num;
	modeleffects Intercept x6_mcar_30 x7 y1_con_bmi_v4;
run;
