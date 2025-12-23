Dementia Risk Factor Modeling & Diagnostic Reliability

Project Overview
This project investigates the clinical risk factors associated with dementia using a dataset of 997 patients in Greater Manchester. It includes a comprehensive analysis of lifestyle factors (smoking, sleep, exercise) and clinical markers (HDL, BMI) to predict dementia occurrence.

Key Technical Actions
* Statistical Modeling: Implemented a Logistic Regression model using backward stepwise selection (AIC) to identify the most significant predictors.
* Comparative Analysis: Conducted a One-way ANOVA with Tukey HSD Post-Hoc tests to statistically validate differences in HDL levels across smoking groups.
* Diagnostic Evaluation: Built a Confusion Matrix to calculate Sensitivity (92.31%), Specificity (98.10%), and Accuracy (97.79%) of clinical dementia diagnoses.

Critical Insights
* Primary Risk Factors: Identified Age, BMI, and Current Smoking as the most significant predictors of dementia.
* Smoking Impact: Current smokers have over 4 times the odds of dementia compared to non-smokers (OR = 4.28).
* Reversibility: Found that ex-smokers return to near-baseline risk levels, highlighting smoking cessation as a critical health intervention.

Tools & Technologies
* R Programming: (Tidyverse, car, ggplot2, patchwork).
* Statistical Concepts: Odds Ratios, Confidence Intervals, AIC, p-values.
