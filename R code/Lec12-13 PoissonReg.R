### Wave Damage Data (Page 205 of GLM)
#The data concern a type of damage caused by waves to the forward section of certain cargo-carrying vessels. For the purpose of setting standards for hull construction, one needs to know the risk of damage associated with the three classifying factors shown below:
#
#           Ship Type: A-E
#Year of Construction: 1960-64, 65-69, 70-74, 75-79
# Period of operation: 1960-74, 75-79
#
#Two other variables are Aggregated months of service and Number of damage accidents.
#
#The question of interest is how the number of damage accidents depends on the other four variables.

## formulate the data into a data.frame
month=c(127,63, 1095,1095,1512, 3353, 0,2244, 44882, 17176, 28609, 20370,  7064, 13099,0, 7117,1179,552,781,676,783,1948,0,274,251,105,
        288,192,349,1208, 0,2051,45,0,789,437,1157, 2161, 0,542)
damage=c(0 , 0  ,3  ,4  ,6, 18,  0, 11, 39, 29, 58, 53, 12, 44,  0, 18,  1,  1,  0,  1,  6,  2,  0,  1,  0,  0,  0,  0,
         2, 11,  0,  4,  0,  0,  7,  7,  5, 12,  0,  1)
ship <- rep(LETTERS[1:5], rep(8,5)) # 5 groups
year <- rep(rep(c("60-64", "65-69", "70-74","75-79"), rep(2,4)), 5) # 4 groups
period <- rep(c("60-74", "75-79"), 20) # 2 groups
wave <- data.frame(ship, year, period, month, damage)

#   ship  year period month damage
#1     A 60-64  60-74   127      0
#2     A 60-64  75-79    63      0
#3     A 65-69  60-74  1095      3
#4     A 65-69  75-79  1095      4
#5     A 70-74  60-74  1512      6

# NOTE: log(month) = offset,
# ALso rule out these 0 values in month, it's n = 34.  


# fit Poisson log linear model
wave.glm1 <- glm(damage~ship+year+period+offset(log(month)), family=poisson, data= wave, subset = month>0)
summary(wave.glm1) 
# n = 34, 
# Ship: A, B, C, D, E;(5-1)
# p = 5-1 + 4-1 + 2-1 + 1 = 9 (category: category -1)
# df = 34 - 9 = 25;
# We have the offset function for the month, log added required. 
# subset = month > 0 for controlling the values > 0

### check interactions 
wave.glm2 <- glm(damage~ship*year+period+offset(log(month)), family = poisson(link=log), data=wave, subset=month>0)
summary(wave.glm2)
# how to calc df?  
# 34 - (19 + 2) = 13
# 3 * 4 = 12, log(offset) is not related to the df.
# 34 - [12 +  (5-1) + (4 -1) + (2 - 1) + 1] = 13

## deviance analysis (ignoring the over dispersion)
test.stat = wave.glm1$deviance - wave.glm2$deviance
df = 25 - 13 
pval = 1 - pchisq(test.stat, df = df) # chisq test
pval # rej, go with the bigger model






##############################################################################
### estimate the dispersion parameter (from the additive model)
# the traditional way of calc constant dispersion parameter
res.p1= residuals(wave.glm1,type = 'pearson', data= wave, subset=month>0)  
# exactly the same as pearson residual for wave.glm3
G1=sum(res.p1^2) # calc dispersion param based on full model
phi=G1/(34-9) ## Estimate the dispersion parameter;
phi # 1.69
wave.glm1$deviance/wave.glm1$df.residual # 1.55

summary(wave.glm1, dispersion = phi)
# an equivalent way of estimating dispersion parameter and fit over-dispersed poisson regression
wave.glm3 <- glm(damage~ship+year+period+offset(log(month)), family=quasi(link=log,variance=mu), data=wave, subset=month>0)
## specify this is a quasi, link and variance;
summary(wave.glm3) 
# original se * phi = SE, changes the z values and p-value in the model 
# estimates stay the same


# test over-dispersion (half normal plot)
plot(qnorm((34+1:34+0.5)/(2*34+1.125)),sort(abs(res.p1)),xlab='Expected Half-Normal Order Stats',ylab='Ordered Abs Pearson Residuals')
abline(a=0,b=1)
abline(a=0,b=sqrt(phi),lty=2)   
# Conclusion: 
# This because of the outliers, but not the overdispersion that leads to lack of fit.
# This one captured a few points at the end, most of points fall around the reference line.
# Perhaps remove the outliers;



# deviance analysis
test.stat=wave.glm1$deviance-wave.glm2$deviance # deviance (from original model fitting)
df= 25-13
res.p = residuals(wave.glm2,type='pearson')  
res.p 
G=sum(res.p^2) # calc dispersion param based on larger model
phi=G/13
F.stat=test.stat/(df*phi)
pval=1-pf(F.stat,df,13)
pval # not rej, go with the smaller model




### New way for fitting the model:overdispersion ##
library(MASS)
newwave = wave[wave$month>0,]
# negative binomial regression
wave.nb = glm.nb(damage~ship+year+period+offset(log(month)),data=newwave)
summary(wave.nb) # dispersion param theta will be estimated simultaneously 
# Conclusion: the result (huge theta value: 52549.57) indicates no over-dispersion, 
# that corresponds to the results we have seen in the half-normal plot; single-double digit number 
# may indicate that there is dispersion in our model.





##################################################
# Analyze fish data for Zero-inflated Poisson regression
# By Gen Li 12/25/2017
#install.packages("pscl")

zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
zinb= zinb[,c(3,4,5,8)]
head(zinb)
## The count depends on the child

library(pscl)
m1 <- zeroinfl(count ~ child + camper | persons, data = zinb) 
# child and camper for poisson, persons for binary
summary(m1)

# Assumptions:
# more people in the group, more likely to fish
# more children, fewer fish; those who camp tend to catch more fish

# Interpretation: (Conditional on fishing = 0 / 1)
# This poisson model given the event, calculate the event rate.
# One more child in you group, on average you will have -1.0428 fewer fish given you fish.
# The log odds ratio for not fishing is -0.5643 if you have one more person in your group.
# Two process both give you the zeros. True zero: binary, you do not fish; 
# psedo zero: you fish, but you didn't get any fish, captured in the poisson model;
# The log rate of fish given there is no child and you do not camp is 1.59789.

#######################################################
# use predict to get prob. of True 0. 
pr <- predict(m1,type ="zero")  # pi(do not fish)
mu <- predict(m1,type = "count") # given fish, how many caught

# The prob. of getting 0 comes from two parts: binary and poisson process:
zip <- pr + (1 - pr)*exp(-mu) # total prob of catch 0 fish 
