# Fit logistic models


# EX1: Show/No-show
data1 = read.table('./Lagtime_1.csv',header=TRUE,sep=',')
View(data1)
## the data is ungrouped; 
## also using lag time to predict show up or not;

table(data1[,1]) # 521 show, 125 no-show;
## [ ,-1] uses the fact that a data.frame is a two dimensional array, so when you do dataframe
## [, -1] you get the sub-array that does not include the first column.
dim(data1) # 645*2
names(data1)
levels(data1$Internal.Status) 
# if resp is a factor, the first level is treated as 0 and second level is treated as 1

data1$Internal.Status1 <-
  relevel(data1$Internal.Status, "NOSHOW") # switch level order: relabelling

fit = glm(Internal.Status~Appointment.Lag,
          family = binomial(link = 'logit'), 
          data = data1)

summary(fit) 
exp(fit$coefficients)[2] 
# odds ratio of no-show with one day increase in lag 
# GoF (ungrouped data) check Hosmer-Lemeshow for ungrouped data
library(ResourceSelection)
hoslem.test(fit$y, fitted(fit), g = 10)  # fitted: returns \hat{pi}
# fails to reject, fit is ok p = 0.1179 > 0.05

# 95% CI for beta
CI1=fit$coefficients + kronecker(t(c(0,qnorm(0.025),-qnorm(0.025))),t(t(sqrt(diag(vcov(fit))))))
out=cbind(exp(CI1)[-1,,drop=FALSE],coef(summary(fit))[-1,4,drop=FALSE])
colnames(out)=c('OR','95% CI','95% CI','p-value')
rownames(out)= c('Lag')
out


# EX2: Pub
data2 = read.table('./MedEd Stats.csv',header=TRUE,sep=',')
sum(data2$timeoff) # 26 timeoff, 162 no timeoff

# fit model
# (#pr, #total-#pr) ~ timeoff
resp=cbind(data2$Urology.Publication,data2$Total.Publications-data2$Urology.Publication)
pred=data2$timeoff
fit=glm(resp~pred,family=binomial(link='logit'))
summary(fit)
exp(fit$coefficients)[2] # odds ratio of urology pub with timeoff vs no-timeoff
# GoF
#sum(residuals(fit,type='pearson')^2) # pearson chisq 
#dev=sum(residuals(fit,type='deviance')^2);dev # deviance (or obtain from summary(glm_logit)) 
## compare with chisq(188-2)
#pval=1-pchisq(dev,186);pval # fit is not good, later will see why (over dispersion)


# equivalently
uropub=xtabs(data2$Urology.Publication~data2$timeoff)
allpub=xtabs(data2$Total.Publications~data2$timeoff)
resp1=cbind(uropub,allpub-uropub)
fit=glm(resp1~c(0,1),family=binomial(link='logit'))
summary(fit) 
exp(fit$coefficients)[2] 



# 95% CI
CI1=fit$coefficients + kronecker(t(c(0,qnorm(0.025),-qnorm(0.025))),t(t(sqrt(diag(vcov(fit))))))
out=cbind(exp(CI1)[-1,,drop=FALSE],coef(summary(fit))[-1,4,drop=FALSE])
colnames(out)=c('OR','95% CI','95% CI','p-value')
rownames(out)=c('timeoff')
out 
## p=0.008, OR=1.31(1.07,1.59) (odds of PR pub in timeoff vs no timeoff)

