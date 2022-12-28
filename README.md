# Computing Codes for the Paper "Comparisons of Statistical Methods for Handling Attrition in a Follow-up Visit with Complex Survey Sampling"
### Jianwen Cai, Donglin Zeng, Haolin Li, Nicole M. Butera, Pedro L. Baldoni, Poulami Maitra, and Li Dong

## Description

In the paper "Comparisons of Statistical Methods for Handling Attrition in a Follow-up Visit with Complex Survey Sampling", we perform extensive simulation studies to compare the performances of various statistical methods of handling attrition in design-based analysis for data from complex sample surveys. In particular, the statistical models considered include linear, logistic, and Poisson regression model; and the missing data methods include inverse probability weighting (IPW), non-response cell weighting (NRCW), multiple imputation (MI), and full information maximum likelihood (FIML) approach. 

For the sake of reproducibility, we summarize the relevant computing codes in this repository. To avoid redundancy, all the computing codes correspond to the over-specified models with MCAR missing mechanism and 30% missingness described in the paper, and the simulation results for correctly specified and under-specified models can be reproduced by removing a particular set of auxiliary variables, and the results for other percentages of missingness and other missing mechanisms can be reproduced by changing the corresponding outcome variables using the following manner: (1) the outcome of change in BMI that has been used in the linear regression model is y_con_bmi_v4, and the outcome variable for the logistic model and Poisson model is y_bin_gfr_low_v3. (2) the missing indicator for MCAR is y_mcar_%; the missing indicator for MCAR is y_mar_v5_%; and the missing indicators for MNAR are y_mnar_bmi_low_v4_% and y_mnar_gfr_low_v3_% for continuous and binary outcome, respectively. 

Note that there are also some changes to the variable names in the manuscript. The x15 in the simulations is the x5 in the manuscript; the x12 in the simulations is the x7 in the manuscript; and the x13 in the simulations is the x8 in the manuscript. 

## Folders 

### 1-Data Generation

In this folder, we summarize the computing codes for generating population and sample data sets and creating IPW and NRW adjusted weights. The names and descriptions of the files are as follows,

* *Generate_Population.r* - The R code for generating the population data set. 
* *Generate_Samples.r* - The R code for generating the sample data sets and creating NRCW adjusted weights. 
* *IPW.sas* - The SAS code creating IPW adjusted weights.

### 2-Weighting-Based Approaches

In this folder, we summarize the computing codes for the weighting-based approaches, which include IPW and NRCW. The names and descriptions of the files are as follows,

* *Weighting_diff.r* - The R code for difference model (linear regression) using IPW or NRCW. 
* *Weighting_rate.r* - The R code for rate of change model (linear regression) using IPW or NRCW. 
* *Weighting_bin.r* - The R code for binary model (logistic regression) using IPW or NRCW. 
* *Weighting_poi.r* - The R code for incidence model (Poisson regression) using IPW or NRCW. 

### 3-Multiple Imputation

In this folder, we summarize the computing codes for MI. The names and descriptions of the files are as follows,

* *MI_diff.sas* - The SAS code for difference model (linear regression) using MI. 
* *MI_rate.sas* - The SAS code for rate of change model (linear regression) using MI. 
* *MI_bin.sas* - The SAS code for binary model (logistic regression) using MI. 
* *MI_poi.r* - The R code for incidence model (Poisson regression) using MI.

### 4-Full Information Maximum Likelihood 

In this folder, we summarize the computing codes for FIML. The names and descriptions of the files are as follows,

* *FIML_diff_full.inp* - The Mplus code for the full difference model (linear regression) using FIML.
* *FIML_diff_reduced.inp* - The Mplus code for the reduced difference model (linear regression) using FIML.
* *FIML_rate_full.inp* - The Mplus code for the full rate of change model (linear regression) using FIML.
* *FIML_rate_reduced.inp* - The Mplus code for the reduced rate of change model (linear regression) using FIML.

## References

Cai, J., Zeng, D., Li, H., Butera, N., Baldoni, P., Maitra, P., & Dong, L.(2021+). Comparisons of Statistical Methods for Handling Attrition in a Follow-up Visit with Complex Survey Sampling. Manuscript Submitted for Publication.


