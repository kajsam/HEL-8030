# HEL-8030 Applied Linear Regression - Spring 2021
# Kajsa Møllersen (kajsa.mollersen@uit.no) March 2021

## from the exercises by Tonje Braathen
# my comments

## Exercise 1: Correlation between two continuous variables
## The dataset CYSTFIBR contains data for 25 patients with cystic fibrosis 
## (from Altman. “Practical Statistics for Medical Research”, p. 338).

setwd("/Users/kam025/Desktop/HEL8030R/")
data <- read.table("cystfibr.txt", header = T) # header = true if there are column names

data # have a quick look 
dim(data) # number of observations and variables
colnames(data) # variable names

## a) Make a scatter plot of FEV1 and BMP. 
# plot(x-axis, y-axis) Commonly, the output (dependent variable) is on the y-axis, and in this 
# example, my interpretation is that the expiration valume is a function of body mass, not the 
# not the other way around. Often there is no clear interpretation.

plot(data$bmp, data$fev1) 

abline(coef(lm(fev1~bmp, data=data))) # bonus question: draw the regression line

## Give your comments of the plot. Is it reasonable to use Pearson’s correlation 
## coefficient in this case?

## Make a scatter plot for all the variables FEV1, BMP, FRC, and RV. 

pairs(~fev1+bmp+frc+rv, data=data)

## Do the same considerations regarding use of Pearson’s correlation coefficient.

## b) Compute the simple correlations between FEV1, BMP, FRC, and RV. Do it with both Pearson and 
## Spearman.

options(digits=2) # it is an annoying amount of digits in the output. besides, it does not make
# sense mathematically or statistically to use the default amount of digits here. 

print("Correlations") # print some headlines ones in a while so the output is easier to read

print("Pearson")
pcor = cor(data, method="pearson")
print(pcor)
print("Spearman")
scor = cor(data, method="spearman")
print(scor)

## c) Make a simple scatter plot of FEV1 and HEIGHT. 
plot(data$height, data$fev1)

## d) Compute the Pearson- and Spearman-correlations between FEV1 and HEIGHT
print("Correlations between fev1 and height")
# text and output on the same line:
sprintf("Pearson: %f", cor(data$fev1, data$height, method="pearson")) 
pcor["fev1","height"] # same as above, just accessing the table
sprintf("Spearman: %f", cor(data$fev1, data$height, method="spearman"))
scor["fev1","height"]

## e) In order to illustrate the influence of a single observation on the Pearson-correlation 
## coefficients you shall exchange the values in the observation with the highest FEV1 with 
## FEV1 = 75 and HEIGHT = 200. 

max(data$fev1) # this gives you the maximum value
which.max(data$fev1) # this gives you the index (the place) of the maximum value

data_max = data # do not manipulate directly in the original data, make a new one
data_max$fev1[which.max(data$fev1)] = 75
data_max$height[which.max(data$height)] = 200

## Make a scatter plot 
par(mfrow=c(2,1)) # two plots in same window
plot(data$height, data$fev1) # for comparison
abline(coef(lm(fev1~height, data=data))) # bonus question: draw the regression line
plot(data_max$height, data_max$fev1)
abline(coef(lm(fev1~height, data=data)),col="gray") # for comparison
abline(coef(lm(fev1~height, data=data_max))) # bonus question: draw the regression line

## and compute Pearson- and Spearman-correlations between FEV1 and HEIGHT. 
sprintf("New Pearson: %f", cor(data_max$fev1, data_max$height, method="pearson"))
sprintf("New Spearman: %f", cor(data_max$fev1, data_max$height, method="spearman"))

## Describe the alterations that take place.


