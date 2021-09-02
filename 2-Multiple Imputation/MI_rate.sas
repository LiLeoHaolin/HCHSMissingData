proc mi data=samp_wide seed=12345 nimpute=5 out=midata(keep = dat_num _imputation_ bgid subid 
y_rate_mcar_30 y1_con_bmi_v4 bghhsub_s2 x1 x2 x3 x4 x5 age_base str2 str3 str4);
by dat_num ;
    var y_rate_mcar_30 x2 x8 age_base str1 str2 str3 x1 x5 y1_con_bmi_v4; * auxiliary variables and covariates;
    fcs reg(y_rate_mcar_30);
run;

proc surveyreg data=midata;
	by dat_num _imputation_;
	strata strat; 
	cluster bgid; 
	weight bghhsub_s2;		
	model y_rate_mcar_30 = x1 x5 y1_con_bmi_v4/ solution anova clparm; 
	ods output ParameterEstimates=betas;
run;

proc mianalyze parms=betas;
	by dat_num;
	modeleffects Intercept x1 x5 y1_con_bmi_v4 ;
run;

