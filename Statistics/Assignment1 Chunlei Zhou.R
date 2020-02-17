#Assignment 1 Chunlei Zhou
#Q1
library(ISLR)
library(ggplot2)
#a
ds = OJ
ds$StoreID = as.numeric(ds$StoreID)
boxplot(ds$LoyalCH~ds$Purchase,xlab='Purchase',ylab='LoyalCH')
#The median of LoyalCH for CH is higher than that of MM.
wilcox.test(ds$LoyalCH ~ ds$Purchase)
#The result shows a significant difference.


#b
boxplot(ds$LoyalCH~ds$StoreID,xlab='StoreID',ylab='LoyalCH')
fit1 <- aov(ds$LoyalCH ~ ds$StoreID)
summary(fit1)
#Result shows that LoyalCH varies with SotreID.

#c
TukeyHSD(aov(ds$LoyalCH ~ as.factor(ds$StoreID)), oredered = TRUE)
#There is no significant difference between the loyalty score of the store #1 and the store #2, since the adjusted p-value is larger than 0.05. Except for the pair mentioned above, the loyalty score is significantly different between all other pairs of stores.
#The difference of the significantly different pairs is ranging from 0 to 0.5, with store #4 and store #3 having the largest difference.

#d
qqnorm(fit1$residuals)
qqline(fit1$residuals)
#The residuals are distributed approxiately as a normal distribution.
#We only need to verify that the residuals have a normal distribution to conclude that the assumption of linear model that the error term (estimated by residuals) belong to normal distribution

#e
#ii
p = rep(NA,100000)
for (i in c(1:100000)){
  id = sample(1:length(fit1$residuals),size=500,replace=TRUE)
  y.boot = fit1$residuals[id]
  fit.boot = lm(y.boot ~ ds$StoreID[id])
  tstore = summary(fit.boot)
  p[i] = pf(tstore$fstatistic[1],df1=1,df2=498,lower.tail = FALSE)}
p
hist(p,nclass=25)
sum(p<0.1)/length(p)
sum(p<0.05)/length(p)
sum(p<0.01)/length(p)
sum(p<0.001)/length(p)
sapply(p, function(x){return((x-0.1)/sqrt(0.1*(1-0.1)*1/100000))})
sapply(p, function(x){return((x-0.05)/sqrt(0.05*(1-0.05)*1/100000))})
sapply(p, function(x){return((x-0.01)/sqrt(0.01*(1-0.01)*1/100000))})
sapply(p, function(x){return((x-0.001)/sqrt(0.001*(1-0.001)*1/100000))})

#iii
sqrt(0.1*(1-0.1)*1/100000)
#The result shows that the bootstrap is quite accurate.

#f
fit2 <- aov(ds$LoyalCH ~ as.factor(ds$StoreID))
y = fit2$fitted.values[ds$StoreID == 7]
y_trans = 1/(1-y)
pt=rep(0,100000)
for(i in c(1:100000)){
index=sample(c(1:length(y_trans)),size=214,replace = TRUE)
y.boot=y_trans[index]
fit.boot=lm(y.boot~ds$StoreID[index])
temp=summary(fit.boot)
pt[i]=pf(temp$fstatistic[1] ,df1 = 1,df2 = 212,lower.tail = F)}
pt
hist(pt)
#The observed significant level would be accurate.

#g
library("MASS", lib.loc="C:/Program Files/R/R-3.5.2/library")
y.obj=boxcox(fit2)
pt=rep(0,100000)
for(i in c(1:100000)){
index=sample(c(1:length(y.obj$y)),size=106,replace = TRUE)
y.boot=y.obj$y[index]
fit.boot=lm(y.boot~ds$StoreID[index])
temp=summary(fit.boot)
pt[i]=pf(temp$fstatistic[1] ,df1 = 1,df2 = 53,lower.tail = F)}
pt

#Q2
ds$PriceDiff <- ds$SalePriceMM - ds$SalePriceCH
#a
lm1 <- lm(ds$LoyalCH~ds$PriceDiff)
summary(lm1)
#LoyalCH varies with PriceDiff.
#When PriceDiff == 0, LoyalCH == 0.54847. For every one dollar's increase in the PriceDiff, the LoyalCH will increase by 0.11819.

#b
lm2 <- lm(ds$LoyalCH~as.factor(ds$StoreID))
summary(lm2)
#LoyalCH varies with StoreID. It shows more details than the previous ANOVA model.

#c
ds$StoreID <-as.factor(ds$StoreID)
lm3 <- lm(ds$LoyalCH~ds$PriceDiff+ds$StoreID)
lm4 <- lm(ds$LoyalCH~ds$PriceDiff*ds$StoreID)
#Model 1
matplot(ds$PriceDiff,ds$LoyalCH,pch=20)
abline(lm1)
#Model 2
matplot(ds$StoreID,ds$LoyalCH,pch=20)
#I am confused about the hind. I am not sure what kind of data frame I should use.
#I tried to use subset to draw five line for each model, but my code has an error " object of type 'closure' is not subsettable"
#I will work on other questions first and to see if I can find the bug of my code later.

#d
SETable=data.frame(model=c('lm1','lm2','lm3','lm4'),
SSE=c(sum(lm1$residuals**2),sum(lm2$residuals**2),sum(lm3$residuals**2),sum(lm4$residuals**2)),
df=c(lm1$df.residual,lm2$df.residual,lm3$df.residual,lm4$df.residual))
SETable
#Model 1,3,4 are nested. Model 2,3,4 are nested.
#Model 4 is the best. It slightly improved Model 3.
#Model 3 slightly improved Model 2.
#Model 3 improved Model 1.

anova(lm1)
anova(lm2)
anova(lm3)
anova(lm4)
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)

#e
lm21 = lm(LoyalCH ~ as.factor(StoreID) + I(PriceDiff*(StoreID == 1)), data = ds)
lm22 = lm(LoyalCH ~ as.factor(StoreID) + I(PriceDiff*(StoreID == 2)), data = ds)
lm23 = lm(LoyalCH ~ as.factor(StoreID) + I(PriceDiff*(StoreID == 3)), data = ds)
lm24 = lm(LoyalCH ~ as.factor(StoreID) + I(PriceDiff*(StoreID == 4)), data = ds)
lm27 = lm(LoyalCH ~ as.factor(StoreID) + I(PriceDiff*(StoreID == 7)), data = ds)
anova(lm2,lm21)
anova(lm2,lm22)
anova(lm2,lm23)
anova(lm2,lm24)
anova(lm2,lm27)
#Got a warning message:
#In anova.lmlist(object, ...) :models with response ‘"LoyalCH"’ removed because response differs from model 1

#f
p.adjust(2.2e-16, method = "bonferroni", n = length(p))
pairwise.t.test(ds$PriceDiff,ds$StoreID,p.adjust= "bonferroni")
#In Store 7, LoyalCH varies with PriceDiff.

#Q3
#d
set.seed(12345)
x = (1:100)/100
y = rnorm(100, mean=1+5*x^2, sd=1)
plot(x,y)
#i
cov(poly(x,3,raw=F))

#ii
fit1=lm(y~poly(x,3,raw=F))
fit2=lm(y~poly(x,3,raw=T))
summary(fit1)
summary(fit1)
#Though the two models have the same F-statictics, the second model do not have any significant coefficient.
#The reason is there is collinearity.

#iii
matplot(x,y,pch=1)
matpoints(x,fit1$fitted.values,pch = 2,col='red')
matpoints(x,fit2$fitted.values,pch=23,col='green')
fit1$fitted.values

#iiii
summary(lm(y~poly(x,2,raw=T)))