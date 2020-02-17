#Q1
install.packages('wooldridge')
library(wooldridge)
data('alcohol')

#(c)
yabuse <- factor(alcohol$abuse)
model1 <- glm (yabuse~mothalc+fathalc,data=alcohol,family='binomial')
model3 <- glm (yabuse~mothalc*fathalc,data=alcohol,family='binomial')
exp(cbind(coef(model1),confint.default(model1)))
summary(model3)

#Q3
library(MASS)
library(class)
Pima.all = rbind(Pima.tr, Pima.te)
summary(Pima.all)
help(Pima.tr)

#(a)
t_wilcoxon <- function(x){
  a=x[Pima.all$type=='No']
  b=x[Pima.all$type=='Yes']
  result=wilcox.test(a,b)
  return(result$p.value)}
for (i in colnames(Pima.all)){
  if (i != 'type'){print(paste(i,': P-value =',t_wilcoxon(Pima.all[,paste(i)])))}
}

#(b)
Pima.all$npreg <- log(Pima.all$npreg+1)
Pima.all$glu <- log(Pima.all$glu)
Pima.all$bp <- log(Pima.all$bp)
Pima.all$skin <- log(Pima.all$skin)
Pima.all$bmi <- log(Pima.all$bmi)
Pima.all$ped = log(Pima.all$ped)
Pima.all$age <- log(Pima.all$age)

#(c)
css = function(ct){
  CE = (ct[2,1]+ct[1,2])/sum(ct)
  sens = ct[2,2]/(ct[2,2]+ct[1,2])
  spec =ct[1,1]/(ct[1,1]+ct[2,1])
  return (c(CE,sens,spec))}

#(d)
##(i)
lda.fit = lda(type~., data=Pima.all,CV=T)
qda.fit = qda(type~., data=Pima.all,CV=T)
#### KNN classifier
train <- Pima.all[,1:length(colnames(Pima.all))-1]
tcl <- Pima.all$type
cev=rep(0,length(seq(1,35,2)))
cnt=1
for(i in seq(1,35,2)){
  knn.fit = knn.cv(train,tcl,k=i)
  cev[cnt]=css(table(knn.fit,tcl))[1]
  cnt=cnt+1
}
plot(cev)
#the K that minimizes CE = 1+2*(which.min(cev)-1
print(1+2*(which.min(cev)-1))
min(cev)

##(ii)
css((table(knn.cv(train,tcl,k=25),tcl)))
#LDA
lda.pre=predict(lda.fit)
css(table(lda.pre$class,tcl))
#QDA
qda.pre=predict(qda.fit)
css(table(qda.pre$class,tcl))

#(e)
##(i)
fit = glm(type=='Yes'~.,data = Pima.all,family = 'binomial')

##(ii)
pr = fit$fitted.values
boxplot(pr~Pima.all$type)
title('Estimated probability of type=="yes"')

# ROC curve
install.packages('ROCR')
library(ROCR)
pred <- prediction(pr, Pima.all$type )
par(mfrow=c(1,1))
perf <- performance(pred,"tpr","fpr")
plot(perf)

#for (i in seq(0,1,0.01)){
#  pred.type = pr>i
#  confusion.table = table(pred.type, Pima.all$type)
#  confusion.table
#  sens1 = confusion.table[2,2]/sum(confusion.table[,2])
#  spec1 = confusion.table[1,1]/sum(confusion.table[,1])
#  ppv1 = confusion.table[2,2]/sum(confusion.table[2,])
#  npv1 = confusion.table[1,1]/sum(confusion.table[1,])
#  c(sens1,spec1,ppv1,npv1)
#  points(1-spec1,sens1,col='red',pch=20)
#}

####Code I wrote when I have misunderstanding of the question... sad...
#pred.type = pr > 0.35
#confusion.table = table(pred.type, Pima.all$type)
#confusion.table
#Based on the confusion table, I can get the value I need.
#sens1 = confusion.table[2,2]/sum(confusion.table[,2])
#spec1 = confusion.table[1,1]/sum(confusion.table[,1])
#ppv1 = confusion.table[2,2]/sum(confusion.table[2,])
#npv1 = confusion.table[1,1]/sum(confusion.table[1,])
#c(sens1,spec1,ppv1,npv1)
#now try to set the threshold as 0.4
#pred.type = pr > 0.4
#confusion.table = table(pred.type, Pima.all$type)
#confusion.table
#sens2 = confusion.table[2,2]/sum(confusion.table[,2])
#spec2 = confusion.table[1,1]/sum(confusion.table[,1])
#ppv2 = confusion.table[2,2]/sum(confusion.table[2,])
#npv2 = confusion.table[1,1]/sum(confusion.table[1,])
#c(sens2,spec2,ppv2,npv2)

#points(1-spec1,sens1,col='red',pch=20)
#points(1-spec2,sens2,col='red',pch=20)

##(iii)
#recall the three classifier
lda.fit = lda(type~., data=Pima.all,CV=T)
qda.fit = qda(type~., data=Pima.all,CV=T)
knn.fit = knn.cv(train,tcl,k=25)
##sens and spec for knn model
ds1=css(table(knn.fit,Pima.all$type))
##sens and spec for lda model
confusion.table1 = table(predict(lda.fit), Pima.all$type)
ds2=css(confusion.table1)
##sens and spec for qda model
confusion.table2 = table(predict(qda.fit), Pima.all$type)
ds3=css(confusion.table2)
plot(perf)
points(1-ds1[3],ds1[2],col='green',pch=20)
points(1-ds2[3],ds2[2],col='blue',pch=20)
points(1-ds3[3],ds3[2],col='yellow',pch=20)
legend(0.5,0.5,legend=c('knn','lda','qda'),col=c('green','blue','yellow'),pch=20)

##(iv)
#fit.prior=function(prior){
#  l.fit = lda(type~., data=Pima.all,CV = T,prior = c(prior,1-prior))
#  q.fit = qda(type~., data=Pima.all,CV = T,prior = c(prior,1-prior))
#  ctl = table(l.fit$class, Pima.all$type)
#  dsl=css(ctl)
#  ctq = table(q.fit$class, Pima.all$type)
#  dsq=css(ctq)
#  points(1-dsl[3],dsl[2],col='red',pch=20)
#  points(1-dsq[3],dsq[2],col='green',pch=20)}
#plot(perf)
#for (i in seq(0,1,0.01)){fit.prior(i)}

### Another method

sensl=rep(0,length(seq(0.01,1,0.01)))
sepcl=rep(0,length(seq(0.01,1,0.01)))
sensq=rep(0,length(seq(0.01,1,0.01)))
sepcq=rep(0,length(seq(0.01,1,0.01)))

fit.priorl=function(priorl){
 l.fit = lda(type~., data=Pima.all,CV = T,prior = c(priorl,1-priorl))}
fit.priorq=function(priorq){
 q.fit = qda(type~., data=Pima.all,CV = T,prior = c(priorq,1-priorq))}
c=1
for(i in seq(0.01,1,0.01)){
 modell=fit.priorl(i)
 ctl = table(modell$class, Pima.all$type)
 dsl=css(ctl)
 modelq=fit.priorq(i)
 ctq = table(modelq$class, Pima.all$type)
 dsq=css(ctq)
 sensl[c]=dsl[2]
 sepcl[c]=1-dsl[3]
 sensq[c]=dsq[2]
 sepcq[c]=1-dsq[3]
 c=c+1
}
lines(sepcl,sensl,col='red')
lines(sepcq,sensq,col='blue')
legend(0.5,0.5,legend=c('LDA','QDA','LR'),col=c('red','blue','black'),lty=1)


#Q4
##(a)
install.packages("randomForest")
library(randomForest)
fit.rf = randomForest(type~.,data=Pima.all)
pred.class = predict(fit.rf,type='response')
cssv=css(table(pred.class,Pima.all$type))

##(b)
#pr.rf = fit.rf$predicted
pred.rf <- prediction(predict(fit.rf,type='prob')[,2],Pima.all$type)
par(mfrow=c(1,1))
perf.rf <- performance(pred.rf,"tpr","fpr")
plot(perf.rf)
#points(1-cssv[3],cssv[2],col='red',pch=20)
plot(perf, add=T, col='green')

pr.lda = lda.fit$posterior
pred.lda <- prediction(pr.lda[,2],Pima.all$type)
perf.lda <- performance(pred.lda,"tpr","fpr")
plot(perf.lda, add=T, col='red')
pr.qda = qda.fit$posterior
pred.qda <- prediction(pr.qda[,2],Pima.all$type)
perf.qda <- performance(pred.qda,"tpr","fpr")
plot(perf.qda, add=T, col='blue')

legend(0.5,0.5,legend=c('LR','LDA','QDA','RF'),col=c('green','red','blue','black'),lty=1)

#points(1-ds2[3],ds2[2],col='blue',pch=20)
#points(1-ds3[3],ds3[2],col='yellow',pch=20)
