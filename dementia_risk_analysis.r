

getwd()
install.packages("car")

dim_study <- read.csv("SHDS_Assessment2_data.csv")


#summarise the data

head(dim_study)
sum(is.na(dim_study))
str(dim_study)
tail(dim_study)
as.character(dim_study$ID)
head(dim_study)

#factorize categorical columns
dim_study$Irritability <- factor(dim_study$Irritability, levels = c("0","1"), labels = c("no","yes"))

dim_study$Smoking <- factor(dim_study$Smoking, levels = c("0", "1", "2"), labels = c("non", "ex", "current"))
head(dim_study)

colnames(dim_study)[colnames(dim_study) == "Physical_activity"] <- "Physical_activity_15mins"
head(dim_study)

str(dim_study$Physical_activity_15mins)
as.character(dim_study$Physical_activity_15mins)
dim_study$Physical_activity_15mins <- factor(dim_study$Physical_activity_15mins, levels = c("0","1"), labels = c("less/equal", "greater"))
head(dim_study)


as.character(dim_study$Dementia)
dim_study$Dementia <- factor(dim_study$Dementia, levels = c("0","1"), labels = c("no", "yes"))
dim_study$Dementia_diag <- factor(dim_study$Dementia_diag, levels = c(0,1), labels = c("no", "yes"))

table(dim_study$Dementia)
head(dim_study, 10)
tail(dim_study, 10)

# check for duplicates
sum(duplicated(dim_study))
sum(duplicated(dim_study[ , -1]))

# Check data ranges and summary
range(dim_study$HDL)
range(dim_study$Sleep)
range(dim_study$BMI)
range(dim_study$Age)
table(dim_study$Smoking)
round(prop.table(table(dim_study$Smoking)) * 100, 2)
table(dim_study$Irritability)
round(prop.table(table(dim_study$Irritability)) * 100, 2)
table(dim_study$Physical_activity_15mins)
round(prop.table(table(dim_study$Physical_activity_15mins)) * 100, 2)
table(dim_study$Dementia)
round(prop.table(table(dim_study$Dementia)) * 100, 2)

# Question 2, exploratory analysis 


#Summary Statistics by group

Smoking_status_grouped <- dim_study %>% group_by(Smoking) %>% 
  summarise(
  n = n(), 
  HDL_mean = mean(HDL), 
  sd_HDL = sd(HDL),
  mid_HDL = median(HDL),
  IQR_HDL = IQR(HDL), 
  min_HDL = min(HDL), 
  max_HDL = max(HDL)
)

Smoking_status_grouped

#box_plot visual exploration
boxplot(HDL ~ Smoking,
        data = dim_study,
        main= "HDL by smoking status",
        xlab = "Smoking group",
        ylab = "HDL-Level"
)

# We have:
# One continuous outcome variable (HDL)
# One categorical predictor with 3 independent groups (Smoking)
# Need to compare means across >2 groups

# Appropriate test: One-way ANOVA

# However, ANOVA requires:
#  1. Approximate normality within each group
# 2. Homogeneity of variances


## normality check in each group

# histogram data check
par(mfrow = c(1,3))
hist(dim_study$HDL[dim_study$Smoking == "non"], main= "Non_Smokers", xlab = "HDL")
hist(dim_study$HDL[dim_study$Smoking == "ex"], main= "Ex_Smokers", xlab = "HDL")
hist(dim_study$HDL[dim_study$Smoking == "current"], main= "Current_Smokers", xlab = "HDL")

# Shapiro-Wilk test 
shapiro.test(dim_study$HDL[dim_study$Smoking == "non"])
shapiro.test(dim_study$HDL[dim_study$Smoking == "ex"])
shapiro.test(dim_study$HDL[dim_study$Smoking == "current"])

# “Shapiro-Wilk tests indicated slight departures from normality in the non-smoker 
# (p < 0.001) and ex-smoker (p < 0.001) groups, while the current smoker group 
# showed approximate normality (p = 0.115). However, given the balanced design and 
# ANOVA’s robustness to minor normality violations, 
# one-way ANOVA remains appropriate.”

## Homogeneity test

#Levene"s test
library(car)

leveneTest(HDL ~ Smoking, data = dim_study)

# Rule f Thumb study
tapply(dim_study$HDL, dim_study$Smoking, sd)
 #largest sd / smallest sd =
0.3856749 / 0.3580399

# “Levene’s test indicated homogeneity of variances across smoking groups 
# (F(2, 994) = 0.67, p = 0.514). Additionally, the rule of thumb was satisfied, 
# with the ratio of largest to smallest standard deviation being 1.08 (< 2).
# Therefore, the assumption of equal variances was met.”


## Run Anova test
anova_test <- aov(HDL ~ Smoking, data = dim_study)
summary(anova_test)
#Run Post-Hoc test
TukeyHSD(anova_test)


#“A one-way ANOVA was conducted to compare HDL cholesterol levels 
# across smoking status groups. The results indicated a statistically 
#significant difference between groups, F(2, 994) = 3.16, p = 0.043.”

#“Post-hoc analysis using Tukey’s HSD test revealed that current smokers had 
#significantly lower HDL cholesterol levels compared to non-smokers 
#(mean difference = -0.11, p = 0.048). No significant differences were found 
# between ex-smokers and non-smokers (p = 0.394) or between current smokers and 
#ex-smokers (p = 0.243).”

# Clinical Interpretation:
# This makes biological sense! Current smoking is associated with lower HDL
# (“good cholesterol”), while people who have quit smoking (ex-smokers) 
# show HDL levels that aren’t significantly different from either current 
# smokers or non-smokers, suggesting a potential intermediate recovery effect.


### Question number 3

# examine the data
table(dim_study$Dementia)
prop.table(table(dim_study$Dementia)) * 100

# Univariance analysis

dim_study %>% group_by(Dementia) %>% summarise(
  mean_HDL = mean(HDL),
  mean_Sleep = mean(Sleep),
  mean_BMI = mean(BMI),
  mean_Age = mean(Age)
)

par(mfrow= c(2,2))
boxplot(HDL ~ Dementia, data = dim_study, main = "HDL by Dementia Status",
        xlab = "Dementia", ylab = "HDL (mmol/L)")
boxplot(Sleep ~ Dementia, data = dim_study, main = "Sleep by Dementia Status",
        xlab = "Dementia", ylab = "Sleep (hours)")
boxplot(BMI ~ Dementia, data = dim_study, main = "BMI by Dementia Status",
        xlab = "Dementia", ylab = "BMI (kg/m²)")
boxplot(Age ~ Dementia, data = dim_study, main = "AGE by Dementia Status",
        xlab = "Dementia", ylab = "AGE (years)")

# check for correlation among predictors
cor(dim_study[ , c("HDL", "Sleep", "BMI", "Age")])


model <- glm(Dementia ~ HDL + Sleep + BMI + Age + Irritability + Smoking + 
               Physical_activity_15mins, data = dim_study, family = binomial())

summary(model)

# identify columns with greater correlation using R, 
# the columns with correlation are stored in a variable called sorted_model
sorted_model <- step(model, direction = "backward")


# # calculate odds ratio

summary(sorted_model)

exp(coef(sorted_model))

# "Higher BMI is significantly associated with increased dementia 
# risk. Each 1 kg/m² increase in BMI raises the odds of dementia 
# by 49.5% (OR = 1.50, 95% CI: [your CI here], p < 0.05). 
# This represents a substantial effect - for example, someone with 
# BMI 30 has approximately 7.6 times higher odds of dementia 
# compared to someone with BMI 25, holding age and smoking constant."



# "Age is the strongest risk factor for dementia. Each additional 
# year increases dementia odds by 13.3% (OR = 1.13, 95% CI: [your CI], 
# p < 0.001). 
#This cumulative effect is substantial: a 70-year-old has 
#after adjusting for BMI and smoking status.


# "Ex-smokers showed no significant difference in dementia odds 
# compared to non-smokers (OR = 0.98, 95% CI: [your CI], p > 0.05). 
# This near-neutral effect (OR ≈ 1.0) suggests that individuals 
# who have quit smoking may return to baseline risk levels, 
# though the current study cannot determine the time since cessation."


#"Current smoking is strongly associated with dementia risk. 
#to non-smokers (OR = 4.28, 95% CI: [your CI], p < 0.05), 
# representing a 328% increase in odds.
# This substantial effect, combined with the neutral effect for 
# ex-smokers (OR = 0.98), provides strong evidence that smoking 
# cessation may reduce dementia risk to near-baseline levels. 
# This represents a critical modifiable risk factor for 
# intervention strategies."


summary(sorted_model)

# Get confidence interval
exp(confint(sorted_model))


# Higher BMI is significantly associated with increased dementia 
# risk (OR = 1.50, 95% CI: 1.23-1.83, p < 0.05). Each unit 
# increase in BMI raises the odds of dementia by 50%, with the 
# true effect likely between 23% and 83%. This dose-response 
# relationship supports the link between obesity and cognitive 
# decline.


# Age emerged as a strong predictor of dementia (OR = 1.13, 
# 95% CI: 1.08-1.19, p < 0.001). Each additional year increases 
# dementia odds by 13%, with a narrow confidence interval 
# indicating high precision. Over a decade, this translates to 
# approximately 3.5-fold higher odds in older individuals.


# Current smoking showed the strongest association with dementia 
# (OR = 4.28, 95% CI: 1.66-10.17, p < 0.05). Current smokers have 
# more than four times the odds of dementia compared to non-smokers. 
# While the wide confidence interval reflects some uncertainty, 
# the lower bound (1.66) still indicates substantial risk.
  
# In contrast, ex-smokers showed no significant difference from 
# non-smokers (OR = 0.98, 95% CI: 0.51-1.85, p > 0.05). This 
# finding is particularly encouraging, suggesting that smoking 
# cessation may reduce dementia risk to near-baseline levels.



### create result table for this in my report

AIC(sorted_model)
coef(sorted_model)
exp(coef(sorted_model))
exp(confint(sorted_model))
summary(sorted_model)


### Qustion 4: Investigate how reliable the dementia diagnosis is
table(dim_study$Dementia, dim_study$Dementia_diag)

# from this table above, label the boxes:

TN <- 927 
FP <- 18
FN <- 4
TP <- 48

total <- TN + FP + FN + TP
total
#identify what those names stand for in my report



# Calculate the percentages to see the reliability of the doctor's diagnosis
 # calculate accuracy

accuracy <- (TN + TP) / total
accuracy_percent <- accuracy * 100
round(accuracy_percent, 2)


# calculate sensitivity (How good at finding sick people?)

sensitivity <- TP / (TP + FN)
round((sensitivity * 100), 2)


# calculate specificity (how good at finding healthy people)
specificity <- TN / (TN + FP)
round((sensitivity * 100), 2)

# positive predictive value (if doctor says yes, are they right)
 ppv <- TP / (TP + FP)

 round(ppv * 100, 2)

 
# negative predictive value (how accurate the doctors are to give a negative result)
 npv <- TN / (TN + FN)
round(npv * 100, 2)















