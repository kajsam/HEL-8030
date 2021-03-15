# HEL-8030 Applied Linear Regression - Spring 2021
# Kajsa Møllersen (kajsa.mollersen@uit.no) March 2021

## Cheat sheet
# This is an R cheat sheet. Here you will find the functions necessary to solve the 
# exercises. 

# type ?function, e.g, ?plot in the console to find out what each function does

# dev.off() comes in handy for errors while plotting
# rm(list = ls()) clear workspace (erase old variables)

## Exercise 1: Correlation between two continuous variables

setwd("/Users/Student/Documents/HEL8030R/") # set the path
data0 <- read.table("data.txt", header = T) # header = true if there are column names

options(digits=2) # it is an annoying amount of digits in the output. besides, it does not make
# sense mathematically or statistically to use the default amount of digits here. 

dim(data0) # number of observations and variables
colnames(data0) # variable names

data0$col3 # will give you that specific column

plot(x-axis, y-axis) # scatter plot

pairs(~col1+col2+col6+col11, data=data0) # multiple scatter plots

par(mfrow=c(2,1)) # two plots in same window

cor(data0, method="pearson") # pairswise correlations, alternative: method="spearman"

pear_cor = cor(data0, method="pearson") # store your output as a variable
pear_cor["variable1","variable7"] # accessing a specific entry by row name and column name

print("Headline") # print some headlines ones in a while so the output is easier to read
print(pear_cor) # print value of variable

max(data0$col1) # the maximum value
idx_max = which.max(data0$col1) # this gives you the index (the place) of the maximum value

data_max = data0 # do not manipulate directly in the original data, make a new one
data_max$col1[idx_max] = 66 # replace a specific index of a variable with a new value

## Exercise 2: Simple linear regression

lin_mod = lm(dep_var~indep_variable, data=data0) # linear model 

abline(lin_mod) # draw a line
abline(lin_mod2, col= "gray") # draw a line with a different color

smr = summary(lin_mod) # a summary of the model with its different parts

str(smr) # tells you all the things that smr contains
intrcpt = smr$coefficients["(Intercept)","Estimate"] # the estimated intercept
beta_age = smr$coefficients["var","Estimate"] # the estimated coeff for "var"

confint(lin_mod, "var", level=1-alpha) # conf int of the estimated coeff for "var"

hist(res, breaks=20, prob=FALSE)  # histogram. 'breaks' lets you decide how many bins. play
# around a bit and see if you change opinion by changing the number of bins. 'prob' (TRUE/FALSE)
# indicated whether the y-axis will display the number of observations in each bin, or the 
# proportion of observations in each bin 

## Exercise 3: Multiple regression

head(data0) # let's you take a quick look 

sprintf("The estimated value is : %.3f", var) # nice output, '%.3f' means 3 decimal digits

idx = match(c("colname1","colname5"),names(data0)) # finding the columns with these names

combn(idx_vec, m) # all possible combinations of the elements of idx_vec, taken m at a time

output_vec = numeric(k) # vector of zeros with the correct length
for (i in 1:k ){ # this is a for-loop, very useful when repeating the same thing
  output_vec[i] = function(input[i])
}

mult_lin_mod = lm(output~var1+var2+var3, data=data0) # multiple linear regression

hist(res, breaks=20, prob=TRUE, 
     xlab="residuals", ylim=c(0, 0.07), 
     main="normal curve over histogram")
# and normal plot for the residuals
mu = mean(res)
std = sqrt(var(res))
curve(dnorm(x, mean=mu, sd=std), 
      col="darkblue", add=TRUE) # lwd=2, thicker line

## Exercise 4: ANCOVA

is.factor(var4) # check if a variable is a factor (a "group"-variable)
as.factor(var4) # make it a factor
levels(var4) # check the levels (the "groups")

# everything will be compared to the first level
relevel(data$var4, "medium") # to compare to another level instead

anova(lin_mod) # gives the same output as summary(lin_mod)

lm(ouput ~ var1 + var2 + var1*var2, data = data0) # interaction term

## Exercise 5: ANOVA - ANCOVA

library(car)
leveneTest(sys ~ pulse + ses, data = bldprs) # Levene will not test for the model that includes pulse
leveneTest(sys ~ ses, data = bldprs) # but this is not the full picture

# Ever thought about what "controlling/adjusting for variable X"/"confounder" meant? Here it is:
lm_s_adj = lm(sys ~ pulse, data = bldprs) # make a model with the control variable
bldprs$res_adj = lm_s_adj$residuals # access the residuals

lm_res = lm(res_adj ~ ses, data = bldprs) # make a model with the residuals as outcome
summary(lm_res)
summary(lm_sps) # see?

leveneTest(res_adj ~ ses, data = bldprs) # this is the full picture

## Exercise 6: Regression diagnostics

predict(lin_mod) # predicted values

cooks.distance(lin_mod) # identify outliers

library(car)
vif(lin_mod) # multicollinearity

