proc mi data=samp_wide seed=12345 nimpute=5 out=midata;
	by dat_num ;
    var y_con_mcar_30 x2 x13 age_base str1 str2 str3 x1 x15 y1_con_bmi_v4 age_strat1 age_strat1_x15 str1_x2 str2_x2 str3_x2 hisp_strat1; * auxiliary variables and covariates;
	fcs reg(y_con_mcar_30);
run;

data midata;
	set midata;
	if dat_num = . then delete;
run;

proc surveyreg data=midata;
	by dat_num _imputation_;
	strata strat; 
	cluster bgid; 
	weight bghhsub_s2;
	model y_con_mcar_30 = x1 x15 y1_con_bmi_v4/ solution anova clparm; 
	ods output ParameterEstimates=betas;
run;

proc mianalyze parms=betas;
	by dat_num;
	modeleffects Intercept x1 x15 y1_con_bmi_v4 ;
run;
