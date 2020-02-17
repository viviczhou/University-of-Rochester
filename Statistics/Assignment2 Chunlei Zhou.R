#Assignment 2 Chunlei Zhou
#Q2
#(b)
#(i)
library(survival)
par(mfrow=c(1,2))
t.points = c(0,105,107.5,110,115,120)
p.surv = c(1,7/8,5/7,1,1/3,0)
km.curve = cumprod(p.surv)
plot(t.points, km.curve, type ='s')
points(t.points[c(4,5)], km.curve[c(4,5)], pch=3)
#(ii)
z = c(105,107.5,107.5,107.5,110,115,115,120)
ev = c(1,1,1,0,0,1,1,1)
plot(survfit(Surv(z,ev)~1), conf.int = FALSE, mark.time = TRUE)
##The two curves is almost the same.

#Q3
library(MASS)
#help(VA)
#head(VA)
#summary(VA)
# Just to get some knowledge regarding the data frame VA.

#(b)
par(mfrow = c(1,2))
plot(survfit(Surv(stime)~cell, data = VA), fun = 'cumhaz',lty = 1, col = 1:4, conf.int = T, xlab = 'Survival Time', ylab = 'Cumulative Hazard')
#The cumulative hazard rate estimates assumption is reasonable.

#(c)
modelA = coxph(Surv(stime)~cell,data=VA)
modelB = coxph(Surv(stime)~cell+Karn, data=VA)
modelC = coxph(Surv(stime)~cell*Karn, data=VA)
anova(modelB,modelA)
anova(modelC,modelB)
#modelB is a significant improvement over modelA.
#modelC is not a significant improvement over modelB.

#(d)
cell1 <- subset(VA,VA$cell==1)
cell2 <- subset(VA,VA$cell==2)
cell3 <- subset(VA,VA$cell==3)
cell4 <- subset(VA,VA$cell==4)

time.grid=seq(1,1001,1)
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(1,1001)),Karn=rep(quantile(cell1$Karn,0.25),1001))
predc1=predict(modelB,newdata=new.x,type="expected")
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(1,1001)),Karn=rep(quantile(cell1$Karn,0.75),1001))
predc2=predict(modelB,newdata=new.x,type="expected")
plot(survfit(Surv(stime,status)~cell, data=cell1),conf.int=F)
lines(time.grid,exp(-predc1),col=2)
lines(time.grid,exp(-predc2),col=3)
title(paste('cell = 1'))
legend('bottomright',legend=c("KM","Q1","Q3"),col=1:3,lty=1)

time.grid=seq(1,1001,1)
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(2,1001)),Karn=rep(quantile(cell2$Karn,0.25),1001))
predc1=predict(modelB,newdata=new.x,type="expected")
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(2,1001)),Karn=rep(quantile(cell2$Karn,0.75),1001))
predc2=predict(modelB,newdata=new.x,type="expected")
plot(survfit(Surv(stime,status)~cell, data=cell2),conf.int=F)
lines(time.grid,exp(-predc1),col=2)
lines(time.grid,exp(-predc2),col=3)
title(paste('cell = 2'))
legend('bottomright',legend=c("KM","Q1","Q3"),col=1:3,lty=1)

time.grid=seq(1,1001,1)
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(3,1001)),Karn=rep(quantile(cell3$Karn,0.25),1001))
predc1=predict(modelB,newdata=new.x,type="expected")
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(3,1001)),Karn=rep(quantile(cell3$Karn,0.75),1001))
predc2=predict(modelB,newdata=new.x,type="expected")
plot(survfit(Surv(stime,status)~cell, data=cell3),conf.int=F)
lines(time.grid,exp(-predc1),col=2)
lines(time.grid,exp(-predc2),col=3)
title(paste('cell = 3'))
legend('bottomright',legend=c("KM","Q1","Q3"),col=1:3,lty=1)

time.grid=seq(1,1001,1)
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(4,1001)),Karn=rep(quantile(cell4$Karn,0.25),1001))
predc1=predict(modelB,newdata=new.x,type="expected")
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(4,1001)),Karn=rep(quantile(cell4$Karn,0.75),1001))
predc2=predict(modelB,newdata=new.x,type="expected")
plot(survfit(Surv(stime,status)~cell, data=cell4),conf.int=F)
lines(time.grid,exp(-predc1),col=2)
lines(time.grid,exp(-predc2),col=3)
title(paste('cell = 4'))
legend('bottomright',legend=c("KM","Q1","Q3"),col=1:3,lty=1)

time.grid=seq(1,1001,1)
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(1,1001)),Karn=rep(quantile(cell1$Karn,0.25),1001))
predc1=predict(modelC,newdata=new.x,type="expected")
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(1,1001)),Karn=rep(quantile(cell1$Karn,0.75),1001))
predc2=predict(modelC,newdata=new.x,type="expected")
plot(survfit(Surv(stime,status)~cell, data=cell1),conf.int=F)
lines(time.grid,exp(-predc1),col=2)
lines(time.grid,exp(-predc2),col=3)
title(paste('cell = 1'))
legend('bottomright',legend=c("KM","Q1","Q3"),col=1:3,lty=1)

time.grid=seq(1,1001,1)
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(2,1001)),Karn=rep(quantile(cell2$Karn,0.25),1001))
predc1=predict(modelC,newdata=new.x,type="expected")
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(2,1001)),Karn=rep(quantile(cell2$Karn,0.75),1001))
predc2=predict(modelC,newdata=new.x,type="expected")
plot(survfit(Surv(stime,status)~cell, data=cell2),conf.int=F)
lines(time.grid,exp(-predc1),col=2)
lines(time.grid,exp(-predc2),col=3)
title(paste('cell = 2'))
legend('bottomright',legend=c("KM","Q1","Q3"),col=1:3,lty=1)

time.grid=seq(1,1001,1)
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(3,1001)),Karn=rep(quantile(cell3$Karn,0.25),1001))
predc1=predict(modelC,newdata=new.x,type="expected")
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(3,1001)),Karn=rep(quantile(cell3$Karn,0.75),1001))
predc2=predict(modelC,newdata=new.x,type="expected")
plot(survfit(Surv(stime,status)~cell, data=cell3),conf.int=F)
lines(time.grid,exp(-predc1),col=2)
lines(time.grid,exp(-predc2),col=3)
title(paste('cell = 3'))
legend('bottomright',legend=c("KM","Q1","Q3"),col=1:3,lty=1)

time.grid=seq(1,1001,1)
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(4,1001)),Karn=rep(quantile(cell4$Karn,0.25),1001))
predc1=predict(modelC,newdata=new.x,type="expected")
new.x=data.frame(stime=time.grid,status=rep(1,1001),cell=as.factor(rep(4,1001)),Karn=rep(quantile(cell4$Karn,0.75),1001))
predc2=predict(modelC,newdata=new.x,type="expected")
plot(survfit(Surv(stime,status)~cell, data=cell4),conf.int=F)
lines(time.grid,exp(-predc1),col=2)
lines(time.grid,exp(-predc2),col=3)
title(paste('cell = 4'))
legend('bottomright',legend=c("KM","Q1","Q3"),col=1:3,lty=1)

#Q4
#(b)
xgrid=seq(-0.7,1.0,0.01)
ygrid=seq(-0.1,1.6,0.01)
density=function(theta1,theta2){
    prior = 1/(1+0.7)*(1.8+0.1)
    z1=1/0.926
    z2=1/0.943
    z3=1/0.787
    u1=sqrt((theta1+0.5)^2+(theta2-0)^2)
    u2=sqrt((theta1-0.42)^2+(theta2-2)^2)
    u3=sqrt((theta1-1.5)^2+(theta2-1.27)^2)
    f_z1=dweibull(z1,shape=8.25,scale=u1)
    f_z2=dweibull(z2,shape=8.25,scale=u2)
    f_z3=dweibull(z3,shape=8.25,scale=u3)
    p_density=f_z1*f_z2*f_z3
    post_density=p_density*prior
    return(post_density)
}
persp(x=xgrid,y=ygrid,z=outer(xgrid,ygrid,density),theta=0, phi=30,col=3)

#(c)
which(density==0,arr.ind=T)

#(d)
for (j in c(100,1000)){
  ntrace=100000
  theta.trace=matrix(NA,ntrace,2)
  theta.old=c(mean(density),0)
  set.seed(234)
  for (i in 1:ntrace){
    theta.new=theta.old+runif(2,-1/10,1/10)
    a=exp(density(theta.new)-density(theta.old))
    if (runif(1)<=a){theta.old=theta.new}
    theta.trace[i,]=theta.old
  }
}

#(e)
#(i)
library(graphics)
theta=c(theta1,theta2)
smoothScatter(theta,xlin=c(-1,1.5),ylim=c(-0.5,2.1))
abline(h=0)
abline(v=0)

#(ii)
contour(xgrid,ygrid,exp(density),add=T)