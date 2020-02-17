######## Assignment 5 Chunlei Zhou

###Q2
library(MASS)
data(Cars93)
help(Cars93)
log.price = log(Cars93$Price)
log.mgp = log(Cars93$MPG.city)
plot(log.price,log.mgp)
library(splines)

# We have 6 models. Wanted: SSE, AIC, BIC, and df for each model.
fit1 = lm(log.mgp~log.price)
fit2=lm(log.mgp~log.price + I(log.price^2))
fit3=lm(log.mgp~I(log.price^-1))
fit4=lm(log.mgp~I(log.price^-2))
fit5=lm(log.mgp~bs(log.price,knots = 2.5,degree=1))
fit6=lm(log.mgp~bs(log.price,knots = c(2.5,3.0),degree=1))
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

n = dim(Cars93)[1]

##(a) Construct a table to list SSE, AIC, BIC, and DF for each model.

table_info=function(model){
  SSE=sum(model$residuals**2)
  k=length(model$coefficients)
  return(c(SSE,n*log(SSE/n)+2*k,n*log(SSE/n)+log(n)*k,k))
}

M =rep(0,4)
library(assignPOP)
table_a <- matrix(rep(0,24),nrow = 6, ncol = 4)
for(i in 1:6){
  M=table_info(get (paste0 ("fit", i)))
  assign(paste('Model',i,sep = ''),M)
  table_a[i,] = (get (paste0 ("Model", i)))
}

colnames(table_a) <- c('SSE', 'AIC','BIC','DF')
rownames(table_a) <- c('Model1', 'Model2','Model3','Model4','Model5','Model6')
table_a


##(b) Use log-likelihood to calculate AIC and BIC
table_info_b=function(model){
  SSE=sum(model$residuals**2)
  k=length(model$coefficients)
  ll=logLik(model)
  return(c(SSE,-2*ll+2*k,-2*ll+log(n)*k,k))
}

Mb = rep(0,4)
table_b <- matrix(rep(0,24),nrow = 6, ncol = 4)

for(i in 1:6){
  Mb=table_info_b(get (paste0 ("fit", i)))
  assign(paste('Model',i,sep = ''),Mb)
  table_b[i,] = (get (paste0 ("Model", i)))
}

colnames(table_b) <- c('SSE', 'AIC','BIC','DF')
rownames(table_b) <- c('Model1', 'Model2','Model3','Model4','Model5','Model6')
table_b

##(c)
table_info_c=function(model){
  SSE=sum(model$residuals**2)
  k=length(model$coefficients)
  AIC = AIC(model)
  BIC = BIC(model)
  return(c(SSE,AIC,BIC,k))
}

Mc = rep(0,4)
table_c <- matrix(rep(0,24),nrow = 6, ncol = 4)

for(i in 1:6){
  Mc=table_info_c(get (paste0 ("fit", i)))
  assign(paste('Model',i,sep = ''),Mc)
  table_c[i,] = (get (paste0 ("Model", i)))
}

colnames(table_c) <- c('SSE', 'AIC','BIC','DF')
rownames(table_c) <- c('Model1', 'Model2','Model3','Model4','Model5','Model6')
table_c

##(d) Standardize AIC and BIC scores
s_table_a = table_a
s_table_b = table_b
s_table_c = table_c
for (i in 1:6){
  s_table_a[i,2] = table_a[i,2]-min(table_a[,2])
  s_table_a[i,3] = table_a[i,3]-min(table_a[,3])
  s_table_b[i,2] = table_b[i,2]-min(table_b[,2])
  s_table_b[i,3] = table_b[i,3]-min(table_b[,3])
  s_table_c[i,2] = table_c[i,2]-min(table_c[,2])
  s_table_c[i,3] = table_c[i,3]-min(table_c[,3])
}

s_table_a
s_table_b
s_table_c

##(e)
par(mfrow=c(3,2))

for (i in 1:6){
  model <- get (paste0 ("fit", i))
  xgrid = seq(min(log.price),max(log.price),0.05)
  ypred <- predict(model,newdata = data.frame(log.price=xgrid))
  plot(log.price, log.mgp, main = paste('Model ', i, 'AIC: ', round(s_table_c[i, 2], 2), 'BIC: ', round(s_table_c[i, 3], 2)))
  lines(xgrid, ypred,col=3,lwd=3,pty='s')
}


###Q3
install.packages("wooldridge")
library(wooldridge)
data("lawsch85")

### Select variables by column index
vari = c(1,4,5,7,9,15,20,21)
### Use log-transform for response variable
y = log10(lawsch85$salary)
### Feature matrix
x = as.matrix(lawsch85[,vari])
### Create data frame and remove missing values
yx = data.frame(y,x)
yx = na.omit(yx)
### Extract separate response and feature matrix
x = as.matrix(yx[,-1])
y = yx[,1]
### Names of variables used
names(yx)

##(a)
par(mfrow=c(1,1))

ssto = var( yx$y ) * (nrow(yx)-1)
for (i in 1:8){
  m = lm(y~poly(x[,i],2))
  sse = sum(m$residuals**2)
  r2 = 1-(sse/ssto)
  assign(paste('Predictor',i,sep = ''),m)
  print(paste('R-square for predicor =', names(yx)[i+1], 'is', r2))
}
fit.total = lm(y~x)
summary(fit.total)
plot(fit.total$fitted.values,fit.total$residuals)
lines(fit.total$fitted.values,
      rep(mean(fit.total$residuals),length(fit.total$fitted.values)),col='green')

##(b)
x_new <- c()
for (i in 2:9){
  x_new <- as.matrix(cbind(x_new, unlist(poly(yx[, i], 2))))
}
xs <- as.matrix(unlist(x_new))
colnames(xs) <- c("rank", "rank2","LSAT","LSAT2", 
                  "GPA", "GPA2", "faculty", "faculty2", 
                  "clsize", "clsize2", "studfac", "studfac2", 
                  "llibvol", "llibvol2", "lcost", "lcosr2" )
apply(xs, 2, var)

fit.frame = data.frame(y,xs)

fit.total2 = lm(y~.,data=fit.frame)
summary(fit.total2)
plot(fit.total2$fitted.values,fit.total2$residuals)
lines(fit.total2$fitted.values,
      rep(mean(fit.total2$residuals),length(fit.total2$fitted.values)),col='green')

##(c)
# Fit a LASSO model based on (b).
# Use cross-validation as implemented in cv.glmnet. 
install.packages("glmnet")
library (glmnet)

fit.lasso.cv = cv.glmnet(xs, y, alpha=1)

# What variables are included in the 1se solution?
fit.lasso.cv$lambda.1se
coef.1se.lasso = coef(fit.lasso.cv, s = fit.lasso.cv$lambda.1se)
coef.1se.lasso[,1][which(coef.1se.lasso[,1] !=0)]


##(d)
# Fit a ridge regression model. 
# Use cross-validation as implemented in cv.glmnet. 
fit.ridge.cv = cv.glmnet(xs, y, alpha=0)
coef.1se.ridge = coef(fit.ridge.cv, s = fit.ridge.cv$lambda.1se)
coef.1se.ridge[,1][which(coef.1se.ridge[,1] !=0)]

# Rank the features in order of the absolute value of the coefficients.
abs.coef.1se.ridge = abs(coef.1se.ridge[,1][which(coef.1se.ridge[,1] !=0)])
rank(-abs.coef.1se.ridge)
# What are the ranks of those coefficients selected by the LASSO model?
abs.coef.1se.lasso = abs(coef.1se.lasso[,1][which(coef.1se.lasso[,1] !=0)]) 
rank(-abs.coef.1se.lasso)

##(e)
library(MASS)
fit.stepaicf <- stepAIC(fit.total2,direction='forward')
fit.stepaicb <- stepAIC(fit.total2,direction='backward')
fit.stepaica <- stepAIC(fit.total2,direction='both')

install.packages("meifly")
library(meifly)
fit.allsub<- fitall(y, data.frame(xs),method = "lm")

allsub.aic = t(sapply(fit.allsub, extractAIC))
allsub.bestmodelbyaic = fit.allsub[order(allsub.aic[,2])[1]][[1]]

coef.stepaicf = abs(coef(fit.stepaicf)[which(coef(fit.stepaicf) !=0)])
coef.stepaicb = abs(coef(fit.stepaicb)[which(coef(fit.stepaicb) !=0)])
coef.allsub = abs(coef(allsub.bestmodelbyaic)[which(coef(allsub.bestmodelbyaic) !=0)])

summary(fit.stepaicf)
summary(fit.stepaicb)
summary(allsub.bestmodelbyaic)

rank(-coef.stepaicf)
rank(-coef.stepaicb)
rank(-coef.allsub)

st1 = system.time(fit.stepaicf<-stepAIC(fit.total2,direction='forward'))
st2 = system.time(fit.stepaicb <- stepAIC(fit.total2,direction='backward'))
st3 = system.time(fit.allsub<- fitall(y, data.frame(xs),method = "lm"))
st4 = system.time(fit.lasso.cv <- cv.glmnet(xs, y, alpha=1))

st1
st2
st3
st4

system.time(allsub.bestmodelbyaic<- fitall(y, data.frame(xs),method = "lm")[order(allsub.aic[,2])[1]][[1]])


##(f)
plot(lawsch85$rank, lawsch85$salary, xlab="Rank", ylab="Salary")

lasso_1se = glmnet(xs,y,alpha = 1, lambda=fit.lasso.cv$lambda.1se)
log.salary=predict(lasso_1se,xs)
org.salary = 10^log.salary

salary.allsubset = 10^allsub.bestmodelbyaic$fitted.values

points(x[,1],org.salary,col=3)
points(x[,1],salary.allsubset,col=2)
legend('topright', c('Original','all subset AIC','Lasso.1se'),col= c(1,2,3),pch = c(1))
