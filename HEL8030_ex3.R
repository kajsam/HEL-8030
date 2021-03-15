# HEL-8030 Applied Linear Regression - Spring 2021
# Kajsa Møllersen (kajsa.mollersen@uit.no) March 2021

## from the exercises by Tonje Braathen
# my comments
#* solutions by Tonje

# set the path 
setwd("/Users/kam025/Desktop/HEL8030R/")

## Exercise 3: Multiple regression
## Dataset BLDPRES. 
## We wish to study the relation between diastolic blood pressure (DIAS) and age (AGE), body weight 
## (WEIGHT) and pulse frequency (PULSE) in a multiple regression

data0 = read.table("bldpres.txt", header = T)

bldprs = data0

head(bldprs) # have a quick look 
dim(bldprs) # number of observations and variables
colnames(bldprs) # variable names

## a) Begin by analyzing the simple relations between diastolic blood pressure and each of the 
## predictors. 

## Make a scatter plot of DIAS as Y-variable and AGE as X-variable. 

plot(bldprs$age, bldprs$dias)

## Include the regression line in the plot.

abline(coef(lm(dias~age, data=bldprs)))

## Do the regression analysis with DIAS as dependent variable and AGE as predictor. 

lm_da = lm(dias~age, data=bldprs)
smr_da = summary(lm_da)

## Do the same analysis (plot and regression) for WEIGHT and PULSE as predictors instead of AGE.

## b) Answer for each model the following: 
## Report the estimates of the regression parameters and the corresponding p-values
## Give your interpretation of the regression coefficients
## What proportion of the total variance of diastolic blood pressure is explained by each predictor 
## separately? 
#* AGE: b1=0.080 (p=0.032). R-square = 0.014
#* WEIGHT: b1=0.276 (p<0.0005). R-square = 0.098
#* PULSE: b1=0.111 (p=0.021). R-square = 0.016

coef = smr_da$coefficients["age","Estimate"] # estimated coefficient
pval = smr_da$coefficients["age","Pr(>|t|)"] # p-value of coefficient
rsq = smr_da$r.squared

sprintf("Estimated coefficient: %.3f", coef)

## c) Compute the Pearson correlation coefficient between the predictors

# repeat for all pairs
sprintf("age - weight: %.2f", cor(bldprs$age, bldprs$weight, method="pearson"))
test = cor.test(bldprs$age, bldprs$weight, method="pearson")
sprintf("p-value: %.3f",test$p.value)

# instead,  print a table of pearson coeffs
idx = match(c("age","weight","pulse"),names(bldprs)) # finding the specifiec columns
options(digits=2)
cor(bldprs[,idx], method="pearson")

# make a matrix of all pairs
idx_pair = combn(idx,2)
k = dim(idx_pair)[2] # number of combinations

# calculate p-value for each pair
options(digits=3)
p_value = numeric(k) # vector of zeros with the correct length
for (i in 1:k ){ # this is a for-loop
  test = cor.test(bldprs[,idx_pair[1,i]], bldprs[,idx_pair[2,i]], method="pearson")
  print(colnames(bldprs[,idx_pair[,i]]))
  print(test$p.value)
  p_value[i] = test$p.value
}

## d) Do a multiple regression analysis with DIAS as dependent variable and AGE, 
## WEIGHT and PULSE as predictors.

lm_dawp = lm(dias~age+weight+pulse, data=bldprs)
smr = summary(lm_dawp)

## Find 95 % confidence intervals for the regression coefficients 

confint(lm_dawp, level=0.95)

## and make a histogram of the residuals
par(mfrow=c(2,1)) # two plots in same window
hist(lm_dawp$residuals, breaks=20, prob=TRUE, 
     xlab="residuals", ylim=c(0, 0.07), 
     main="normal curve over histogram")
# and normal plot for the residuals
mu = mean(lm_dawp$residuals) # calculate the mean
std = sqrt(var(lm_dawp$residuals)) # and the standard deviation
# it is also found here: smr$sigma
curve(dnorm(x, mean=mu, sd=std), 
      col="darkblue", add=TRUE) # lwd=2, thicker line

# bonus: qq-plots
qqnorm(lm_dawp$residuals)
qqline(lm_dawp$residuals)

## e) Interpret the estimates for the regression coefficients. 
## Notice that the estimated regression coefficients is pretty close to the simple coefficients 
## you found in part b. Give an explanation of this.

smr$coefficients[,"Estimate"]

## f) Formulate a null hypothesis and an alternative hypothesis for the F-test in the ANOVA-table 
## in the output. What is the conclusion of the test? 
#* The null hypothesis is rejected (F=16.25, p<0.0005)
smr
smr$fstatistic # for the model as a whole

anv_dawp = anova(lm_dawp) # variable by variable, as entered

## g) What proportion of the total variance of diastolic blood pressure is explained by age, 
## weight and pulse frequency together? Compare this with the results in the simple regressions
#* R-square = 0.131, slightly greater than the sum of the R-squares from the simple models.
print(anv_dawp$"Sum Sq")
TotVar = sum(anv_dawp$"Sum Sq")
print(TotVar)
Prop = sum(anv_dawp$"Sum Sq"[1:3])/TotVar
sprintf("Proportion of total variance: %.3f", Prop)

# it is available directly from the linear model
smr$r.squared

# h) Appraise the histogram and decide if the normality assumption is met
