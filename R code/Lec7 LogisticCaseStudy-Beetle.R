# Analyze Beetle Mortality Data
# By Gen Li 8/19/2016


# load data
library(ggplot2)
Dose = c(49.05689, 52.99074, 56.91150, 60.84151, 64.75898, 68.69103, 72.61060, 76.54203)
log10Dose = log10(Dose)
num = c(59, 60, 62, 56, 63, 59, 62, 60) 
killed = c(6, 13, 18, 28, 52, 53, 61, 60)
data = data.frame(log10Dose,num,killed)

# data visualization
ggplot(data, aes(x = log10Dose, y =  killed/num)) +
  geom_point()

# data preparation
x = data$log10Dose
y = data$killed
m = data$num
resp = cbind(y, m - y)   #### The event is being killed


### (1)fit logistic model
glm_logit = glm(resp ~ x, family = binomial(link = 'logit'))

#glm_probit=glm(resp~x, family=binomial(link='probit'))
#glm_clog=glm(resp~x, family=binomial(link='cloglog')) # asymmetric
summary(glm_logit) # wald test of coefficients

# Interpretation:
# beta_0: log10(1)= 0, for the intercept; when log10dose is 1, the log odds ratio of being killed is -60.717.
# beta_1: The log odds ratio of beetles being killed is 34.3 per unit change of log10 dose.
# 2 parameters, df = 8 -2 = 6; 
# Null deviance: 7 + 1 = 8 

#(2) surv=1, death=0, resp = response
resp = cbind(m - y, y) 
glm_logit = glm(resp ~ x, family = binomial(link = 'logit'))

## Flip of sign:
summary(glm_logit) # wald test of coefficients

#(3) But the flip would not hold for log-log link.
glm_clog = glm(resp~x, family=binomial(link = 'cloglog'))
summary(glm_clog)

#(4) probit, sign flipped 
glm_probit = glm(resp~x, family=binomial(link='probit'))
summary(glm_probit)

### logistic model with ungrouped data
new.x = c(rep(x,y), rep(x, m - y))
new.resp = c(rep(1,sum(y)), rep(0,sum(m-y)))
newdata = data.frame(new.resp,new.x)
dim(newdata)
head(newdata)
glm_logit1 = glm(new.resp~new.x, family = binomial(link='logit'),data=newdata)
summary(glm_logit1)

## results are the same.
## Same coefficients and estimation results, but there are some changes:
# Null deviance: 480 df, not 7 anymore.
# Residual dieviance: 479 df, not 6 anymore.


######################################
# Goodness of fit/residuals (check ?residuals.glm to learn more about residuals)
## (1) Match the grouped model fit 
beta0 = coef(glm_logit)[1]
beta1 = coef(glm_logit)[2]
beta_0 = x * beta1
pihat = fitted(glm_logit)

## pearson-chi-square residual 
G.res = (y - m * pihat) / sqrt(m * pihat * (1 - pihat)) # manually
residuals(glm_logit, type = "pearson") # pearson chisq 
sum(residuals(glm_logit,type = 'pearson')^2) # pearson chisq 

## deviance (or obtain from summary(glm_logit)) 
dev = sum(residuals(glm_logit,type = 'deviance')^2)
dev 
# compare with chisq(model: 8-2 = 6)
pval = 1 - pchisq(dev,6);pval 
# the p-value is 0.08 > 0.05, fit is ok, fails to reject

## (2) Match the ungrouped model fit 
# check Hosmer-Lemeshow for ungrouped data
library(ResourceSelection)
# fitted: returns \hat{pi}
hl <- hoslem.test(glm_logit1$y, fitted(glm_logit1), g = 10) 
hl # Again, p-value is 0.29 > 0.05, the fit is ok, fails to reject


# Now, we turn into the inference part
######################################
# Confidence interval 
# CI for beta
vcov(glm_logit) 
# variance-covariance matrix of beta MLE (fisher information inverse)

beta = glm_logit$coefficients[2]
se = sqrt(vcov(glm_logit)[2,2]) # (same as in summary(glm_logit))
beta + c(qnorm(0.025), 0, -qnorm(0.025)) * se

# CI for odds ratio: exp(beta); tranfer back. 
exp(beta + c(qnorm(0.025),0,-qnorm(0.025)) * se)
 
# CI for x\beta
out = predict(glm_logit, se.fit = TRUE);out 

# predict: returns x\hat{beta}
# ?predict.glm # check other options; can also return \hat{pi}
out = predict(glm_logit, data.frame(x = c(1.7)), se.fit = TRUE)
CIval = out$fit[1] +  c(qnorm(0.025),0,-qnorm(0.025))*out$se.fit[1] # 95% CI  for eta, based on delta method

# CI for pi
predict(glm_logit, data.frame(x = 1.7), se.fit = TRUE,type ='response') # predict: \hat{pi}
exp(CIval)/(1 +  exp(CIval)) # 95% CI for pi
#
# # Note: do NOT use the following to get CI for pi (b/c g^-1(eta) deviates from Gaussian)
# out= predict(glm_logit, data.frame(x=c(1.7)), se.fit=TRUE, type='response')
# wrong.CIval=out$fit+c(qnorm(0.025),0,-qnorm(0.025))*out$se.fit
# wrong.CIval


# `````` Example```````#
# LD50 est and CI
beta0= glm_logit$coefficients[1]
beta1= glm_logit$coefficients[2]
betacov = vcov(glm_logit) # inverse fisher information
x0fit = -beta0/beta1
x0fit # Used for cross validation 
10^x0fit # point estimate of LD50
varx0 = betacov[1,1]/(beta1^2) + betacov[2,2]*(beta0^2)/(beta1^4)- 
  2*betacov[1,2]*beta0/(beta1^3)
c(x0fit,sqrt(varx0)) # point est and se
10^(x0fit + c(qnorm(0.025),-qnorm(0.025))*sqrt(varx0)) # 95% CI for LD50




