# Analyze malignant melanoma Data (two-way contingency table)
# By Gen Li 12/25/2017

freq=c(22,16,19,11,2,54,33,17,10,115,73,28)
tumor=factor(rep(c('Hutchinson', 'Superficial','Nodular','Indeterminate'),3))
site=factor(rep(c('HeadNeck','Trunk','Extremities'),c(4,4,4)))
melanoma=data.frame(freq,tumor,site)
tbl=xtabs(freq~tumor+site)
tbl



#### fit Poisson model
model1 <- glm(freq~tumor+site, family=poisson, data=melanoma)
summary(model1)
D=model1$deviance
D # 51.8
G=sum(residuals(model1, type="pearson")^2) 
G # 65.8, same as chisq test stat
1-pchisq(G,6) ### returns a very significant p-value
# chisq test
chisq.test(tbl)

#### fit multinomial model
n=sum(freq)
LL1=dmultinom(freq, prob=freq/n,log=TRUE) # log lik of full model
#
out=aggregate(freq,by=list(site),FUN=sum)
pj=out[,2]/n
names(pj)=out[,1]
pj
out=aggregate(freq,by=list(tumor),FUN=sum)
pi=out[,2]/n
names(pi)=out[,1]
pi
prob=(pi[match(melanoma[,2],names(pi))]*pj[match(melanoma[,3],names(pj))])
LL2=dmultinom(freq,prob=prob,log=TRUE)
#
LR=2*(LL1-LL2)
LR # 51.8, same as Dev in model1


#### fit product multinomial model (rows being obs)
library(nnet)
model2 <- multinom(tbl~1)
pihat=predict(model2,type='p')
pihat
# deviance (do not use deviance from summary(model2))
m=rowSums(tbl)
D.stat=sum(2*tbl*log(tbl/(m*pihat)))
D.stat # 51.8, same as Dev in model1







