# HEL 8030  Exercise 5
# by Kajsa Møllersen (kajsa.mollersen@uit.no) March 2021

## from the exercises by Tonje Braathen
# my comments
#* solutions from Tonje

# set the path 
setwd("")

# use this command to clear your workspace before a new exercise
# do not use it within a script! code is for sharing, and suddenly someone else's workspace got cleared
# rm(list = ls())

options(digits=3) 

## Exercise Exercise 5: ANOVA - ANCOVA
## Dataset BLDPRES.
data0 <- read.table("bldpres.txt",header = TRUE)

bldprs = data0

# Do some simple investigation
typeof(bldprs) # what type of object is this
str(bldprs) # what structure does it have 

head(bldprs) # displays the first entries
dim(bldprs) # number of observations and variables
colnames(bldprs) # names of variables

## We now wish to study if the systolic blood pressure differs among the three socioeconomic status groups 
## (SES) while controlling for differences in pulse frequency.

# There is a simple way to handle categorical variables in R
is.factor(bldprs$ses) # checking if it is a factor
bldprs$ses = as.factor(bldprs$ses) # want it to be factor
levels(bldprs$ses) # having a look at the levels
levels(bldprs$ses) = c("low", "medium", "high") # rename them

## a) Examine if there is an interaction between SES and PULSE. 
#*p = 0.088 for the interaction terms

lm_sps_int = lm(sys ~ pulse + ses + ses*pulse, data = bldprs)
str(lm_sps_int) # wow! look at this, all the different things that are already in this object
anv = anova(lm_sps_int)
anv["pulse:ses","Pr(>F)"] # p-value for interaction term

# apparently, we remove the interaction term

## b) Do the covariance analysis. 

# Create the linear model
lm_sps = lm(sys ~ pulse + ses, data = bldprs)
summary(lm_sps)

# Check the assumption of Gaussian distributed residuals
res = resid(lm_sps) # this function returns the residuals
res = lm_sps$residuals # but we already have them

hist(res, breaks=20, prob=TRUE, 
     xlab="residuals", ylim=c(0, 0.04), 
     main="normal curve over histogram")
# and normal plot for the residuals
mu = mean(res)
std = sqrt(var(res))
curve(dnorm(x, mean=mu, sd=std), 
      col="darkblue", add=TRUE) # lwd=2, thicker line

qqnorm(res)
qqline(res)

# Check for equal variances

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

## Does the analysis confirm significant differences in expected systolic blood pressure between the three 
## socioeconomic status groups controlled for pulse frequency? 
#* Upper and lower SES differ significantly (p = 0.002)

# Do the analysis
smr = summary(lm_sps) 
pvalue_ses = smr$coefficients[c("sesmedium","seshigh"),"Pr(>|t|)"]
pvalue_ses = as.data.frame(pvalue_ses)
colnames(pvalue_ses) = levels(bldprs$ses)[1]
pvalue_ses

# Do the ses2 vs ses3 comparison
bldprs$ses = relevel(bldprs$ses, ref = "medium")
smr = summary(lm(sys ~ pulse + ses, data = bldprs))
smr$coefficients["seshigh","Pr(>|t|)"]

bldprs$ses = relevel(bldprs$ses, ref = "low") # reverse

## c) Repeat b) as an ANOVA.
#* p = 0.002 for the overall association with SES, controlled for puls
anv_sps = anova(lm_sps)

anv_sps["ses","Pr(>F)"]

## d) Include SEX as a second factor and run a two-way ANOVA, controlling for PULSE

bldprs$sex = as.factor(bldprs$sex) # want it to be factor
levels(bldprs$sex) # having a look at the levels
levels(bldprs$sex) = c("men", "women") # rename them

# Check out the intersections betwen sex and pulse
lm_sex_int = lm(sys ~ pulse + ses + sex + pulse*sex, data = bldprs)
anova(lm_sex_int)

# Check out the intersections betwen ses and sex
lm_sex_int = lm(sys ~ pulse + ses + sex + ses*sex, data = bldprs)
anova(lm_sex_int)

lm_spss = lm(sys ~ pulse + ses + sex, data = bldprs)

anova(lm_spss)

