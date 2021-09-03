# Computing Codes for the Paper "Comparisons of Statistical Methods for Handling Attrition in a Follow-up Visit with Complex Survey Sampling"
### Haolin Li

## Description

In the paper "Comparisons of Statistical Methods for Handling Attrition in a Follow-up Visit with Complex Survey Sampling", we perform extensive simulation studies to compare the performances of various statistical methods of handling attrition in design-based analysis for data from complex sample surveys. In particular, the statistical models considered include linear, logistic, and Poisson regression model; and the missing data methods include inverse probability weighting (IPW), non-response cell weighting (NRW), multiple imputation (MI), and full information maximum likelihood (FIML) approach. Please click [here](https://www.google.com) for the full text of the paper.

For the sake of reproducibility, we summarize the relevant computing codes in this repository. To avoid redundancy, all the computing codes correspond to the over-specified models described in the paper, and the simulation results for correctly specified and under-specified models can be reproduced by removing a particular set of auxiliary variables.

For questions or suggestions please send an email to haolin@live.unc.edu.

## Folders 

### 1-Data Generation

In this folder, we summarize the computing codes for generating population and sample data sets and creating IPW and NRW adjusted weights. The names and descriptions of the files are as follows,

* *train_cc.csv* - Training data set for complete-case analysis.
* *train_mi_1.csv, train_mi_2.csv, train_mi_3.csv, train_mi_4.csv, train_mi_5.csv* - Five training data sets for multiple imputation.
* *test.csv* - Test data set for all types of analysis.
* *train_cc_downsample.csv* - Training data set for down-sampling analysis.

### 2-Weighting-Based Approaches

In this folder, we summarize the computing codes for the weighting-based approaches, which include IPW and NRW. The names and descriptions of the files are as follows,

* *train_cc.csv* - Training data set for complete-case analysis.
* *train_mi_1.csv, train_mi_2.csv, train_mi_3.csv, train_mi_4.csv, train_mi_5.csv* - Five training data sets for multiple imputation.
* *test.csv* - Test data set for all types of analysis.
* *train_cc_downsample.csv* - Training data set for down-sampling analysis.

### 3-Multiple Imputation

In this folder, we summarize the computing codes for MI. The names and descriptions of the files are as follows,

* *MI_diff.sas* - The SAS code for difference model (linear regression) using MI. 
* *MI_rate.sas* - The SAS code for rate of change model (linear regression) using MI. 
* *MI_bin.sas* - The SAS code for binary model (logistic regression) using MI. 

### 4-Full Information Maximum Likelihood 

In this folder, we summarize the computing codes for FIML. The names and descriptions of the files are as follows,

* *FIML_diff_full.inp* - The Mplus code for the full difference model (linear regression) using FIML.
* *FIML_diff_reduced.inp* - The Mplus code for the reduced difference model (linear regression) using FIML.
* *FIML_rate_full.inp* - The Mplus code for the full rate of change model (linear regression) using FIML.
* *FIML_rate_reduced.inp* - The Mplus code for the reduced rate of change model (linear regression) using FIML.



