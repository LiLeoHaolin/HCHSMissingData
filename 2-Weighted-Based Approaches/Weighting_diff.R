design = svydesign(id=~BGid, strata=~strat, weights=~wts, data=subdat)
# 'wts' refers to the IPW or NRW adjusted sampling weights;
# 'subdat' refers to the dataset in which the missing observations are removed. 
model = svyglm(y_con_mcar_30~x1+x5+x6+y1_con_bmi_v4,family = gaussian(link='identity'),design=design)