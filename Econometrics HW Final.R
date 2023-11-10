## Due to COVID infection, I did the lab by myself.

## Continued from the last lab

require(plyr)
require(dplyr)
require(tidyverse)
require(haven)

levels_n <- read.csv("C:/Users/Cho/Desktop/Econometrics/IND_levels.csv")
names(levels_n) <- c("New_Level","levels_orig")
acs2021$IND <- as.factor(acs2021$IND)
levels_orig <- levels(acs2021$IND) 
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))

acs2021$public_work <- acs2021$IND 
levels_public <- read.csv("C:/Users/Cho/Desktop/Econometrics/publicwork_recode.csv")
names(levels_public) <- c("levels_orig","New_Level")
levels_new_pub <- join(data.frame(levels_orig),data.frame(levels_public))

levels(acs2021$IND) <- levels_new$New_Level
levels(acs2021$public_work) <- levels_new_pub$New_Level

## Transfer imported data set as numeric data and entitlement.

acs2021$public_work_num <- as.numeric(acs2021$public_work == "work for public, stable")
table(acs2021$public_work,acs2021$public_work_num)

## The subgroup I chose: 22~55 years old African American female group who are fully employed

attach(acs2021)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35) & (female == 1) & (AfAm == 1)
dat_use <- subset(acs2021,use_varb) 
detach(acs2021)
summary(dat_use)

## Null hypothesis: There is not a correlation between being an African American female and proportion of people who are working in public work sector

ols_out1 <- lm(public_work_num ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use)
summary(ols_out1, type = "text")

## p-value: < 2.2e-16 / Our p-value was very small, therefore, I figure there are enough evidence to reject the null hypothesis.

## For the final project, I am going to work on the correlations between individual's income and obesity.
## I am going to read journals posted by National Health Department of the U.S.

pred_vals_ols1 <- predict(ols_out1, dat_use)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))

table(pred = pred_model_ols1, true = dat_use$public_work_num)

## the table shows that the people in public sector would more likely to not be an African American female(False 567 > true 447)

summary(pred_vals_ols1)

## Mean was 0.5555

model_logit1 <- glm(public_work_num ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = acs2021, family = binomial
)
summary(model_logit1)

pred_vals_logit1 <- predict(model_logit1, acs2021, type = "response")
pred_model_logit1 <- (pred_vals_logit1 > 0.5)
table(pred = pred_model_logit1, true = acs2021$public_work_num)

dat_use$PUMA_factor <- as.factor(dat_use$PUMA)
d_pub_work <- data.frame(model.matrix(~ dat_use$public_work_num))
d_educ_hs <- data.frame(model.matrix(~ dat_use$educ_hs))
d_educ_somecoll <- data.frame(model.matrix(~ dat_use$educ_somecoll))
d_educ_college <- data.frame(model.matrix(~ dat_use$educ_college))
d_educ_advdeg <- data.frame(model.matrix(~ dat_use$educ_advdeg))
d_age <- data.frame(model.matrix(~ dat_use$AGE))
d_PUMA <- data.frame(model.matrix(~ dat_use$PUMA_factor))

#Confirmed sum of it is equal to 0
sum( colSums(d_PUMA) == 0)#Put Together
dat_for_analysis_sub <- data.frame(
  d_pub_work[,2], # need [] since model.matrix includes intercept term
  d_educ_hs[,2],
  d_educ_somecoll[,2],
  d_educ_college[,2],
  d_educ_advdeg[,2],
  d_age[,2],
  d_PUMA[,2:145] )

#TraiN data

install.packages("standarize")
require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$pub_work)

restrict_1 <- (runif(NN) < 0.35) # use 10% as training data, ordinarily this would be much bigger but start small
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

fmla_sobj <- reformulate( names(dat_for_analysis_sub[2:151]), response = "pub_work")
sobj <- standardize(fmla_sobj, dat_train, family = binomial)
s_dat_test <- predict(sobj, dat_test)

model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$pub_work)

#HW 9

# Adding Dummy
dat_use$BA_plus <- dat_use$educ_college + dat_use$educ_advdeg

# Linear regression with new dummy
model_lpm_v1 <- lm(public_work_num ~ female + BA_plus + AGE + I(female*BA_plus) + I(AGE * female), data = dat_use)
summary(model_lpm_v1)

# p-value: 5.514e-06

dat_use_female <- subset(dat_use,as.logical(dat_use$female))
dat_use_male <- subset(dat_use,!(dat_use$female))

# 2 part comparison

model_lpm_v1f <- lm(public_work_num ~ BA_plus + AGE, data = dat_use_female)
summary(model_lpm_v1f)

model_lpm_v1m <- lm(public_work_num ~ BA_plus + AGE, data = dat_use_male)
summary(model_lpm_v1m)

# Random Forest

install.packages('randomForest') # For generating random forest model
library(randomForest)
set.seed(54321)
model_randFor <- randomForest(as.factor(pub_work) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)

# look at confusion matrix for this too

pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$pub_work)

install.packages('e1071')
library(e1071)

# summary(tuned_parameters)
# figure best parameters and input into next

svm.model <- svm(as.factor(pub_work) ~ ., data = sobj$data, cost = 1, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$pub_work)