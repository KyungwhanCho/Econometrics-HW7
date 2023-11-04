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

ols_out1 <- lm(public_work_num ~ female + AfAm + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use)
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

model_logit1 <- glm(public_work_num ~ female + AfAm + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = acs2021, family = binomial
)
summary(model_logit1)

pred_vals_logit1 <- predict(model_logit1, acs2021, type = "response")
pred_model_logit1 <- (pred_vals_logit1 > 0.5)
table(pred = pred_model_logit1, true = acs2021$public_work_num)

## False 77348 > True 5888 indicating the ratio of African American female working in public sector against others.