design = svydesign(id=~BGid, strata=~strat, weights=~wts, data=subdat)
# 'wts' refers to the IPW or NRW adjusted sampling weights;
# 'subdat' refers to the dataset in which the missing observations are removed. 
model = svyglm(y_egfr_mcar_30~x6+x7+y1_con_bmi_v4,subset=(y1_bin_gfr==0), family=binomial(link = "logit"), design=design)