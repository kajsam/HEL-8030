# HEL-8030 Applied Linear Regression - Spring 2021
# Kajsa Møllersen (kajsa.mollersen@uit.no) March 2021

## from the exercises by Tonje Braathen
# my comments
#* solutions from Tonje

setwd("/Users/kam025/Desktop/HEL8030R/")
options(digits=3)

## Exercise 2: Simple linear regression
## Data are in the file BLDPRES. Firstly you shall study the relation between AGE and SYS.
## In the parts a-d, do the analyses just for women (SEX = 1) and in e for men (SEX = 0).

data0 = read.table("bldpres.txt", header = T) # load the data file

bldprs = data0 # make a copy, so the original file is unaltered

dim(bldprs) # number of individuals and variables 
colnames(bldprs) # variable names

## a) Select women and make a scatter plot of SYS and AGE. Include the regression line in the 
## plot.
idx = which(bldprs$sex == 1) # index of the women
bldprs_w = bldprs[idx,] # data set of women only

plot(bldprs_w$age, bldprs_w$sys) # make the plot
lm_women_sa <- lm(sys~age, data=bldprs_w) # linear model 
abline(lm_women_sa) # draw the line

## b) Do a simple regression analysis with SYS as dependent and AGE as
## independent variable.

smr = summary(lm_women_sa)
print(smr)

## Report the estimated regression line.
intrcpt = smr$coefficients["(Intercept)","Estimate"] # the estimated intercept
beta_age = smr$coefficients["age","Estimate"] # the estimated age coeff

## Report the 95 % confidence interval for the regression parameter for the
## variable AGE.

confint(lm_women_sa, "age", level=0.95)

## Does age contribute significantly to explain the variation in systolic blood
## pressure?

p_age = smr$coefficients["age","Pr(>|t|)"]

## c) Locate in the output and report:
## The estimate for the standard deviation of the residuals.
#* SRES = 'Standard error of the estimate' = 16.2
#* R-square = 0.189, i.e. 18.9 % of the variation in SYS can be explained by AGE

smr$sigma

## The proportion of total variance in systolic blood pressure explained by age.

smr$r.squared

## d) Examine if the model assumptions is fulfilled in a residual analysis:

## Make histogram of the residuals, and appraise it. Are the residuals (approximately) 
## normally distributed?

res = lm_women_sa$residuals # the residuals are already calculated
hist(res, breaks=20, prob=F) 

## Make a scatter plot of the residuals and AGE (or the predicted values).
## Does the variance appear to be independent of AGE?

plot(bldprs_w$age, lm_women_sa$res)

## e) Do the regression analysis for men with the same variables as in a-d.
idx = which(bldprs$sex == 0) # index of the men
bldprs_m = bldprs[idx,] # data set of men only

## Report the slope of the regression line and compare with the result from
## the analysis in women.

#* The slope for men = 0.244 (lower than in women)


plot(bldprs_m$age, bldprs_m$sys) # make the plot
lm_men_sa <- lm(sys~age, data=bldprs_m) # linear model 

## Draw both regression lines in the same scatter plot.
abline(lm_men_sa)
abline(lm_women_sa, col= "gray") # draw the line

