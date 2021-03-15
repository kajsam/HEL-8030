# HEL 8030  Exercise 6
# by Kajsa Møllersen (kajsa.mollersen@uit.no) March 2021

## from the exercises by Tonje Braathen
# my comments
#* spss solution

# set the path 
setwd("/Users/kam025/Desktop/HEL8030R/")

options(digits=3) 

## Exercise 6: Regression diagnostics
## Dataset CYSTFIBR

data0 <- read.table("cystfibr.txt",header = TRUE)

cystfbr = data0
head(cystfbr)# have a quick look 
dim(cystfbr) # individuals
colnames(cystfbr) # variables

## a) For the model μfev1 = β0 + β1∙age + β2∙sex + β3∙bmp + β4∙frc, 

lm0 = lm(fev1  ~ age + sex + bmp + frc, data = cystfbr)

## assess the model assumptions:  
## i.   Make a residual plot (residuals and predicted values)

res0 = lm0$residuals
pred0 = predict(lm0)

plot(pred0, res0)

## ii. Make a normal plot and a histogram of the residuals

hist(res0, breaks=5, prob=TRUE, 
     xlab="residuals", ylim=c(0, 0.2), 
     main="normal curve over histogram")
# and normal plot for the residuals
mu = mean(res0)
std = sqrt(var(res0))
curve(dnorm(x, mean=mu, sd=std), 
      col="darkblue", add=TRUE) # lwd=2, thicker line

qqnorm(res0)
qqline(res0)

## iii. Identify possible outliers

cooksd <- cooks.distance(lm0)

max(cooksd)/mean(cooksd)

cystfbr[which.max(cooksd),c("fev1","age","sex","bmp","frc")]
par(mfrow=c(2,2))
hist(cystfbr$age)
hist(cystfbr$sex)
hist(cystfbr$bmp)
hist(cystfbr$frc)

## iv. Consider the amount of multicollinearity

library(car)
vif(lm0)

## Is there any potential problems regarding the model assumptions or influence 
## of outliers?

## b) Dataset BLDPRES 
## For the model μsys = β0 + β1∙age + β2∙weight + β3∙pulse + β4∙X1 + β5∙X2, 
## assess the model assumptions:  

## v.   Make a residual plot (residuals and predicted values)
## vi.   Make a normal plot and a histogram of the residuals
## vii.   Identify possible outliers
## viii.   Consider the amount of multicollinearity
