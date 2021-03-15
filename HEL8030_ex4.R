# HEL-8030 Applied Linear Regression - Spring 2021
# Kajsa Møllersen (kajsa.mollersen@uit.no) March 2021

## from the exercises by Tonje Braathen
# my comments
#* solutions by Tonje

# set the path 
setwd("")

# rm(list = ls()) clear workspace

options(digits=3) 

## Exercise 4: ANCOVA
## Dataset BLDPRES. 
data0 = read.table("bldpres.txt",header = TRUE)

bldprs = data0
dim(bldprs) # individuals
colnames(bldprs) # variables

## We now wish to study if the systolic blood pressure differs among the three socioeconomic 
## status groups (SES) while controlling for body weight

## a) Compute the unadjusted differences in SYS between the SES-groups. 

## Use a multiple regression model with two dummy variables and assign the lowest SES-level as 
## reference group. 
bldprs$dummy2 <- as.numeric(bldprs$ses == 2)
bldprs$dummy3 <- as.numeric(bldprs$ses == 3)
lm_ss_dummy <- lm(sys~dummy2+dummy3, data = bldprs)
summary(lm_ss_dummy)

# This is a more elegant solution

is.factor(bldprs$ses) # checking data type
bldprs$ses = as.factor(bldprs$ses) # want it to be factor
levels(bldprs$ses) # having a look at the levels

levels(bldprs$ses) = c("low", "medium", "high") # rename them

## Are there significant differences in SYS between the SES-groups?  

lm_ss <- lm(sys~ses, data = bldprs)
print("Compared to group: ") 
levels(bldprs$ses)[1]
smr = summary(lm_ss) # compares to the first level

pval = smr$coefficients[2:3,"Pr(>|t|)"]
# nice presentation of p-values
pval = as.matrix(pval)
colnames(pval) = levels(bldprs$ses)[1]
pval 

# if you want to compare to the medium level instead
bldprs$ses = relevel(bldprs$ses, "medium")
lm_ss <- lm(sys~ses, data = bldprs)
print("Compared to group: ") 
levels(bldprs$ses)[1]
smr = summary(lm_ss) # compares to level medium

pval = smr$coefficients[2:3,"Pr(>|t|)"]
# nice presentation of p-values
pval = as.matrix(pval)
colnames(pval) = levels(bldprs$ses)[1]
pval 

## Report estimates for unadjusted differences, p-values and 95 % confidence intervals between 
## the middle and lowest and between the highest and lowest SES-level. 
#* Unadjusted difference in SYS between middle and low class: -1.449 (p=0.49). 95% CI: (-5.58;2.68)
#* Unadjusted difference in SYS between upper and low class: 8.037 (p=0.002). 95% CI: (2.93;13.14)

bldprs$ses = relevel(bldprs$ses, "low") # reverse
lm_ss <- lm(sys~ses, data = bldprs)

print("Compared to group: ") 
levels(bldprs$ses)[1]
coef(lm_ss)
confint(lm_ss, level=0.95)

## b) Check if a one-way ANOVA gives the same p-value as in a) when testing for significant 
## differences in expected SYS between the SES-groups
anv_ss = anova(lm_ss, data = bldprs) # get a warning because it is not nestes
anv_ss 

## c) Use one-way ANOVA (as in b) or a multiple regression (as in a) to decide if there are 
# significant differences in expected body weight between the SES-groups.

lm_ws <- lm(weight~ses, data = bldprs)
summary(lm_ws)

anv_ws = anova(lm_ws, data = bldprs)
anv_ws

## d) In order to do an ANCOVA with weight as control variable we have to first examine if 
## the differences in expected systolic blood pressure between the SES-groups are independent of 
## body weight. Check this assumption by including suitable interaction variables in the model. 
## Give your conclusion.
#* The p value for the change in (partial) F is non-significant (p=0.822)

lm_ssw_int = lm(sys ~ ses + weight + ses*weight, data = bldprs)
summary(lm_ssw_int)
anova(lm_ssw_int)

## e) In order to adjust the analysis in part a) for weight the variable WEIGHT may be included in 
## the regression model. Does the over-all test confirm significant differences in expected 
## systolic blood pressure between the SES-groups controlled for weight? 
#* difference in SYS between the three ses groups, adjusted for body weight (p=0.013).

lm_sws = lm(sys ~ weight + ses, data = bldprs)
anova(lm_sws)

## Compute the adjusted differences between the middle and lowest and between the highest and 
## lowest SES-level
# * Adjusted difference between middle and low class:(-3.18, p=0.14), 
# and between high and low class (4.58, p=0.11). 
summary(lm_sws)




