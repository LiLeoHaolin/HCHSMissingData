# Simulation Codes for the Paper "Comparisons of Statistical Methods for Handling Attrition in a Follow-up Visit with Complex Survey Sampling"
### Haolin Li

## Project Description

Welcome to our UNC-BIOS735 project. In this repository, you will find a variety of scripts and folders.  Let's begin with a description of our research project. Recruiting the right employees is key to business growth and success, which makes staffing an essential business operation. While recruiting companies like LinkedIn and Indeed have helped the business expand their grid of search for new employees at competitive prices, staffing can be expensive. The primary aim of the project is to build statistical and machine learning models to predict whether a candidate wants to look for new employment based on available features. By using the statistical techniques in the course, we will practice the steps to build a statistical model and estimate parameters through maximum likelihood estimation (MLE). While for the machine learning model, a generic random forest (FR) and support vector machine (SVM) model will be built to compare the accuracy of predictions. The second aim is to evaluate the performance of different classifiers through the area under the curve (AUC) of the receiver operator characteristics (ROC) curve, as well as the sensitivity and specificity of the predicted values. The third aim is to practice our skills in building R packages to solve real-world data problems.

In this project, we use a data set offered by Kaggle, a subsidiary of Google Inc. that has publicly available data sets for machine-learning and data-science practitioners. The data set contains 12 feature variables and 19158 observations. Please click [here](https://www.kaggle.com/arashnic/hr-analytics-job-change-of-data-scientists) for a better description of the data. 

## Folders 

### 1-Data

In this folder, we will keep all the data produced from the **2.1 Data Pre-processing Steps** step from the manuscript. This includes;

* *train_cc.csv* - Training data set for complete-case analysis.
* *train_mi_1.csv, train_mi_2.csv, train_mi_3.csv, train_mi_4.csv, train_mi_5.csv* - Five training data sets for multiple imputation.
* *test.csv* - Test data set for all types of analysis.
* *train_cc_downsample.csv* - Training data set for down-sampling analysis.

### 2-Work Flow

This folder will be dedicated to showing the necessary steps with code implementation on how to calculate the desired results. The names and descriptions of the files are as follows,  

* *1_DataPreprocessing* - Illustrate how to pre-process the data from the website. This part of the implementation formulates creates the data files in the **1-Data** folder. 
* *2_Parametric_models* - Show how to conduct the logistic and ridge-logistic regression using our R-package. We assess the overall performance of the parametric models using the complete-case dataset and the multiple imputation. We also conducted sensitivity analysis about the missing data mechanisms by comparing the parameter estimates from complete-case analysis and multiple imputation.
* *3_ML* - Implement the machine learning methods described in the report. 

The order of the files that you will need to run is *1_DataPreprocessing*, *2_Parametric_models*, *3_ML*. 

### 3-Report  

A copy of the initial project proposal (due March 30) and final project report (due May 8) are kept in this folder. The presentation for the report can be found [here](https://docs.google.com/presentation/d/1ps8OVQ3goY8DG6n5b9Uw2LeLj9rf22sMTaY5Rkaxj60/edit?usp=sharing)

### 4-Package 

Our R-package implementation will be kept in this folder. For more information about the functions please refer to the report.

For questions or suggestions please send an email to jesusvaz[at]unc[dot]ad[dot]edu.


