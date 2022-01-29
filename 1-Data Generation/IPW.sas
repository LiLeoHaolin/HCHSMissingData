%let datdir=/netscr/doli/HSCL/newdata_Mar/; *directory for simulated data;
%let outdir=/netscr/doli/HSCL/newdata_Mar/; *directory for output;


****MAR****;
%macro getlogit(mydat=,miss=);
proc logistic data=&mydat;
	class y_&miss strat;
	model y_&miss(event="0")= age_base strat;
	output out = logit_&miss p = p_&miss;
	by dat_num;
run;
%mend;

%macro weight(name=);

%do i=1 %to 10;
%let a=%EVAL((&i-1)*100+1);
%let b=%EVAL((&i-1)*100+100);
proc import
		datafile= "&datdir.&name.&a._&b..csv"
		out=dat DBMS=csv replace;
		getnames=yes;
run;

%let dd=dat;


****MCAR: no model is needed****;

proc means data=&dd noprint ;
	var y_mcar_2 y_mcar_5 y_mcar_10 y_mcar_20 y_mcar_30;
	output out=pred_prob mean(y_mcar_2 y_mcar_5 y_mcar_10 y_mcar_20 y_mcar_30)= p1 p2 p3 p4 p5;
	by dat_num;
run;

data ip_mcar;
	merge &dd  pred_prob;
	by dat_num;
	ip_mcar_2=1/(1-p1);
	ip_mcar_5=1/(1-p2);
	ip_mcar_10=1/(1-p3);
	ip_mcar_20=1/(1-p4);
	ip_mcar_30=1/(1-p5);
	drop p1-p5 _TYPE_ _FREQ_;
run;

data Wip_mcar;
	set ip_mcar;
	W_ip_mcar_2=ip_mcar_2*bghhsub_s2;
	W_ip_mcar_5=ip_mcar_5*bghhsub_s2;
	W_ip_mcar_10=ip_mcar_10*bghhsub_s2;
	W_ip_mcar_20=ip_mcar_20*bghhsub_s2;
	W_ip_mcar_30=ip_mcar_30*bghhsub_s2;
run;


%let misslist=mar_v2_2 mar_v2_5 mar_v2_10 mar_v2_20 mar_v2_30 
	mnar_bmi_low_v3_2 mnar_bmi_low_v3_5 mnar_bmi_low_v3_10 mnar_bmi_low_v3_20 mnar_bmi_low_v3_30 
	mnar_gfr_low_v3_2 mnar_gfr_low_v3_5 mnar_gfr_low_v3_10 mnar_gfr_low_v3_20 mnar_gfr_low_v3_30
	mnar_bmi_hi_v3_2 mnar_bmi_hi_v3_5 mnar_bmi_hi_v3_10 mnar_bmi_hi_v3_20 mnar_bmi_hi_v3_30 
	mnar_gfr_hi_v3_2 mnar_gfr_hi_v3_5 mnar_gfr_hi_v3_10 mnar_gfr_hi_v3_20 mnar_gfr_hi_v3_30;

%let p_misslist=p_mar_v2_2 p_mar_v2_5 p_mar_v2_10 p_mar_v2_20 p_mar_v2_30 
	p_mnar_bmi_low_v3_2 p_mnar_bmi_low_v3_5 p_mnar_bmi_low_v3_10 p_mnar_bmi_low_v3_20 p_mnar_bmi_low_v3_30 
	p_mnar_gfr_low_v3_2 p_mnar_gfr_low_v3_5 p_mnar_gfr_low_v3_10 p_mnar_gfr_low_v3_20 p_mnar_gfr_low_v3_30
	P_mnar_bmi_hi_v3_2 p_mnar_bmi_hi_v3_5 p_mnar_bmi_hi_v3_10 p_mnar_bmi_hi_v3_20 p_mnar_bmi_hi_v3_30 
	p_mnar_gfr_hi_v3_2 p_mnar_gfr_hi_v3_5 p_mnar_gfr_hi_v3_10 p_mnar_gfr_hi_v3_20 p_mnar_gfr_hi_v3_30;

%let logit_misslist=logit_mar_v2_2 logit_mar_v2_5 logit_mar_v2_10 logit_mar_v2_20 logit_mar_v2_30 
	logit_mnar_bmi_low_v3_2 logit_mnar_bmi_low_v3_5 logit_mnar_bmi_low_v3_10 logit_mnar_bmi_low_v3_20 logit_mnar_bmi_low_v3_30 
	logit_mnar_gfr_low_v3_2 logit_mnar_gfr_low_v3_5 logit_mnar_gfr_low_v3_10 logit_mnar_gfr_low_v3_20 logit_mnar_gfr_low_v3_30
	logit_mnar_bmi_hi_v3_2 logit_mnar_bmi_hi_v3_5 logit_mnar_bmi_hi_v3_10 logit_mnar_bmi_hi_v3_20 logit_mnar_bmi_hi_v3_30 
	logit_mnar_gfr_hi_v3_2 logit_mnar_gfr_hi_v3_5 logit_mnar_gfr_hi_v3_10 logit_mnar_gfr_hi_v3_20 logit_mnar_gfr_hi_v3_30;

%let ip_misslist=ip_mar_v2_2 ip_mar_v2_5 ip_mar_v2_10 ip_mar_v2_20 ip_mar_v2_30 
	ip_mnar_bmi_low_v3_2 ip_mnar_bmi_low_v3_5 ip_mnar_bmi_low_v3_10 ip_mnar_bmi_low_v3_20 ip_mnar_bmi_low_v3_30 
	ip_mnar_gfr_low_v3_2 ip_mnar_gfr_low_v3_5 ip_mnar_gfr_low_v3_10 ip_mnar_gfr_low_v3_20 ip_mnar_gfr_low_v3_30
	ip_mnar_bmi_hi_v3_2 ip_mnar_bmi_hi_v3_5 ip_mnar_bmi_hi_v3_10 ip_mnar_bmi_hi_v3_20 ip_mnar_bmi_hi_v3_30 
	ip_mnar_gfr_hi_v3_2 ip_mnar_gfr_hi_v3_5 ip_mnar_gfr_hi_v3_10 ip_mnar_gfr_hi_v3_20 ip_mnar_gfr_hi_v3_30;

%let W_ip_misslist=W_ip_mar_v2_2 W_ip_mar_v2_5 W_ip_mar_v2_10 W_ip_mar_v2_20 W_ip_mar_v2_30 
	W_ip_mnar_bmi_low_v3_2 W_ip_mnar_bmi_low_v3_5 W_ip_mnar_bmi_low_v3_10 W_ip_mnar_bmi_low_v3_20 W_ip_mnar_bmi_low_v3_30 
	W_ip_mnar_gfr_low_v3_2 W_ip_mnar_gfr_low_v3_5 W_ip_mnar_gfr_low_v3_10 W_ip_mnar_gfr_low_v3_20 W_ip_mnar_gfr_low_v3_30
	W_ip_mnar_bmi_hi_v3_2 W_ip_mnar_bmi_hi_v3_5 W_ip_mnar_bmi_hi_v3_10 W_ip_mnar_bmi_hi_v3_20 W_ip_mnar_bmi_hi_v3_30 
	W_ip_mnar_gfr_hi_v3_2 W_ip_mnar_gfr_hi_v3_5 W_ip_mnar_gfr_hi_v3_10 W_ip_mnar_gfr_hi_v3_20 W_ip_mnar_gfr_hi_v3_30;



****MNAR*****;
*get predicted observed probability for mar using logistic regression;
*%getlogit(mydat=&dd,miss=mnar_bmi_hi_2);
*%getlogit(mydat=&dd,miss=mnar_bmi_hi_5);
*%getlogit(mydat=&dd,miss=mnar_bmi_hi_10);
*%getlogit(mydat=&dd,miss=mnar_bmi_hi_20);
*%getlogit(mydat=&dd,miss=mnar_bmi_hi_30);
*%getlogit(mydat=&dd,miss=mnar_gfr_hi_2);
*%getlogit(mydat=&dd,miss=mnar_gfr_hi_5);
*%getlogit(mydat=&dd,miss=mnar_gfr_hi_10);
*%getlogit(mydat=&dd,miss=mnar_gfr_hi_20);
*%getlogit(mydat=&dd,miss=mnar_gfr_hi_30);

%macro Logitres();
	%do j=1 %to %sysfunc(countw(&misslist));
	%let miss_term=%scan(&misslist,&j);
		%getlogit(mydat=&dd,miss=&miss_term);
	%end;
%mend;

%Logitres;

 
data data_all;
	merge &dd &logit_misslist;
	drop _LEVEL_;
run;

data data_weights;
	set data_all;
	array p_mnar[25] &p_misslist;
	array ip_mnar[25] &ip_misslist;
	array W_ip_mnar[25] &W_ip_misslist;
	
	do i=1 to 25;
		ip_mnar[i]=1/p_mnar[i];
		W_ip_mnar[i]=ip_mnar[i]*bghhsub_s2;
	end;
	drop &p_misslist i;
run;


proc sort data=Wip_mcar;
	by dat_num subid;
run;

proc sort data=data_weights;
	by dat_num subid;
run;

data data_final;
	merge  data_weights Wip_mcar(keep=subid dat_num ip_mcar_2 ip_mcar_5 ip_mcar_10 ip_mcar_20 ip_mcar_30 W_ip_mcar_2 W_ip_mcar_5 W_ip_mcar_10 W_ip_mcar_20 W_ip_mcar_30);
	by dat_num subid;
run;


proc export data=data_final
	outfile="&outdir.&name._ipwts_&a._&b..csv"
	dbms=csv replace;
run;
%end;

%mend;

%weight(name=widewt_samp_oct2016_nrwts_v3vars); 
