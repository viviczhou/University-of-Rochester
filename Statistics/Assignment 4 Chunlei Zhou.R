####Assignment 4 Chunlei Zhou

###Q1
gem = read.table(file = 'GSE364n50.csv',sep=',',row.names=1,header=F)
row.names(gem)
library(stats)

##(a)
##### Create matrix using prcomp function.
##### Using gene expressions as a feature set.
##### Using centering, but not scaling.

gem$group <- NA
gem$group[1:30] <- 'matastasis'
gem$group[31:50] <- 'non-matastasis'
gem1 = gem[,-which(names(gem)%in%c('group'))]
prc = prcomp(gem1, center=T,scale.=F)
plot(prc)

##### Construct a QDA classifier for metastasis class
##### Use the first four PC as features.
##### CV = T
##### prior=c(0.5,0.5)
library(MASS)
fit.qda = qda(gem$group~prc$x[,1:4],CV=T,prior = c(0.5,0.5))
summary(fit.qda)

#(ii)
post = fit.qda$posterior
pmax = rep(0,length(seq(1,50,1)))

### Waste code...
#c=1
#for(i in seq(1,50,1)){
#  if (post[i,1]>post[i,2]){
#    pmax[c]=post[i,1]}
#  else{
#    pmax[c]=post[i,2]}
#  c=c+1}

for (i in seq(1,50,1)){
  pmax[i] = max(post[i,])}
pmax

## Count pmax<0.75 (11) and pmax>=0.75 (39) first. 
## Then build array to store values.
as.data.frame(table(pmax))
## Then use loop to calculate cr.
correct = rep(0,length(seq(1,50,1)))
correct2 = rep(0,length(seq(1,50,1)))
c=1
for(i in seq(1,50,1)){
  if (i<=30){
    if (pmax[i] >= 0.75){
      if (pmax[i]==post[i,1]){
        correct[c]=1}
      else{
        correct[c]=0}}
    else{
      if (pmax[i]==post[i,1]){
        correct2[c]=1}
      else{
        correct2[c]=0}}}
  else{
    if (pmax[i]>=0.75){
      if (pmax[i]==post[i,2]){
        correct[c]=1}
      else{
        correct[c]=0}}
    else{
     if (pmax[i]==post[i,1]){
        correct2[c]=0}
      else{
        correct2[c]=1}}}
  c=c+1}
cr = sum(correct)/39 #0.8974359
cr2= sum(correct2)/11 #0.6363636

cor = correct + correct2
a <- as.data.frame(table(pmax))
a$correct <- cor[1:49]
a$correct <- corr[1:49]
a[nrow(a) + 1,] = list('1','2','0')
a$correct <- as.numeric(a$correct)

## Wilcoxon rank-sum test
pc = pmax[fit.qda$class == gem$group]
pnc = pmax[fit.qda$class != gem$group]
wilcox.test(pc,pnc)

## P-value: 0.01336

#(iii)

## Useless code...
#corr = rep(0,length(seq(1,50,1)))
#c=1
#for(i in seq(1,50,1)){
#  if (i<=30){
#    if (pmax[i]==post[i,1]){
#        corr[c]=1}
#      else{
#        corr[c]=0}}
#  if (i>30){
#    if (pmax[i]==post[i,1]){
#      corr[c]=0}
#    else{
#      corr[c]=1}
#    }
#  c=c+1}
#sum(corr)

my.pch = as.numeric(fit.qda$class == gem$group)
pairs(prc$x[,1:4],col=c(rep('red',30),rep('black',20)),pch=c(1,3[my.pch]))

##(b)
# Use cross-validation to determine the # of PC to include in the classifier.

# Method 1
positive = (gem$group == 'matastasis')
er1 = c()
for (i in seq(1,18,1)){
  new.qda = qda(matrix(prc$x[,1:i],nrow=50,ncol=i),grouping=gem$group)
  new.predict = predict(new.qda,method='looCV')$class
  pred = (new.predict =='matastasis')
  er1 = c(er1,sum(pred != positive)/length(positive))
}

# Method 2
er2 = c()
for (i in seq(1,18)){
  error = 0
  for (j in 1:50){
    gemi = gem1[j,]
    true = gem$group[j]
    pca.new = prcomp(gem1[-j,])
    matrix(pca.new$x[,1:i],nrow=49,ncol=i)
    new.qda.2 = qda(matrix(pca.new$x[,1:i],nrow=49,ncol=i),grouping = gem$group[-j])
    pca.test = predict(pca.new,gemi)
    result = predict(new.qda.2,pca.test[1:i])$class
    if (true != result){
      error = error+1}}
  er2 = c(er2,error/nrow(gem1))}

matplot(x=seq(1,18),y=cbind(er1,er2),pch=c(20,22),col=c(2,3),cex=0.8,xlab='K PCs',ylab='error rate', main = 'LOOCV for Method 1 & Method 2')
legend('bottomleft',legend=c('error rate 1','error rate 2'),col=c(2,3),pch=c(20,22))

### Q2
##(a)
install.packages("partitionComparison")
library(partitionComparison)

pp = new("Partition", c(1,1,2,2))
qq = new("Partition", c(1,1,2,3))
mutualInformation(pp,qq)

##(b)
data("iris")
summary(iris)
head(iris)
hfit = hclust(dist(iris[,1:4]),method='average')
plot(hfit,labels=as.integer(iris$Species))

##(c)
#Naive code...
#for (i in 2:10) {
#  mm = table(iris$Species,cutree(hfit,k=i))}
#install.packages('factoextra')
#library(factoextra)
#mindis = rep(0,10)
#for (i in 1:10){
#  print(paste('K =', i))
#  print(get_dist(mm[,i], method = "euclidean", stand = F))
#  mindis[i] = min(get_dist(mm[,i], method = "euclidean", stand = F))}
#plot(mindis)

dis_matrix = as.matrix(dist(iris[,1:4]))
average_dis = function(c1,c2,dis_matrix){
  total = length(c1)*length(c2)
  dis_sum = 0
  for (i in c1){
    for (j in c2){
      dis_sum = dis_sum + dis_matrix[i,j]}}
  return(dis_sum/total)}

install.packages("assignPOP")
library(assignPOP)

#magic_for()
min_dis = rep(0,length(seq(2,10)))
ct=1
for (i in 2:10){
  mm = table(iris$Species,cutree(hfit,k=i))
  gr = cutree(hfit,k=i)
  nn = matrix(c(rep(99,i^2)),nrow=i,ncol=i)
  for (a in 1:i-1){
    for (b in a+1:i){
      if (b <= i){
        c1 = which(gr==a)
        c2 = which(gr==b)
        nn[a,b] = average_dis(c1,c2,dis_matrix)
        nn[b,a] = average_dis(c1,c2,dis_matrix)}}
    assign(paste('cluster_distance_',i,sep = ''),nn)}
  print(paste('The smallest distance for K =', i, 'is', min(get (paste0 ("cluster_distance_", i)))))
  min_dis[ct] = min(get (paste0 ("cluster_distance_", i)))
  ct = ct+1}



#install.packages("devtools") # if you have not installed "devtools" package
#devtools::install_github("hoxo-m/magicfor")
#library(magicfor)

#magic_result_as_dataframe()

## There must be a way to print out the result in the loop. Not the magic function...

##(d)

iqr =c()
for (i in 2:10){
  truth = new('Partition', c(rep(1,50),rep(2,50),rep(3,50)))
  ha = mutualInformation(truth,truth)
  grp = cutree(hfit,k=i)
  cluster = new('Partition',grp)
  mi = mutualInformation(truth,cluster)
  hb = mutualInformation(cluster,cluster)
  iqr[i-1] = (mi/max(ha,hb))
}

##(e)
par(mfrow=c(1,1))
plot(min_dis,xlab='K',ylab='Minimum Distance Between Clusters',pch=20)
plot(iqr,xlab='K',ylab='I(Q,R)',pch=20)

### Q3
###### Load data
data(UScereal)
summary(UScereal)
head(UScereal)
rownames(UScereal)

### Define new feature matrix
df <- subset(UScereal, select = -c(mfr,shelf,vitamins))
fm = prcomp(df, center=T,scale.=F)

### Log transformation f(x)=log10(x+1)
df$calories <- log10(df$calories+1)
df$protein <- log10(df$protein+1)
df$fat <- log10(df$fat+1)
df$sodium <- log10(df$sodium+1)
df$fibre <- log10(df$fibre+1)
df$carbo <- log10(df$carbo+1)
df$sugars <- log10(df$sugars+1)
df$potassium <- log10(df$potassium+1)

##(a)
### K-means
r2 = rep(0, 15)
ss_within = rep(0,15)
for (i in 1:15) {
  fit = kmeans(df,centers=i,nstart=100)
  r2[i] = fit$betweenss/fit$totss
  ss_within[i] = fit$withinss
}
plot(r2,ylim=c(0,1),xlab='K',ylab='R-squared',type='b',pch=20)
plot(ss_within,xlab='K',ylab='SSwithin',type='b',pch=20)

##(b)
for (i in seq(1,15,1)){
  if (r2[i+1] - r2[i] <= 0.1){
    print(paste('K=',i,', R-squared=',r2[i]))
    return(r2[i])
  }
}
## So the true number of clusters is 3. (K=3)
## Create a separate list of brand names for each cluster when K=3.
fit.suit = kmeans(df,centers=3,nstart=100)
fit.suit$size
# size of cluster 1, 2, 3 are 35, 5, 25 respectively.

# Wrong code...
#bn_for_c1 = list()
#bn_for_c2 = list()
#bn_for_c3 = list()
#for (i in seq(1,65)){
#  if (fit.suit$cluster[i]==1){
#    bn_for_c1[i]=fit.suit$cluster[i]}
#  if(fit.suit$cluster[i]==2){
#    bn_for_c2[i]=fit.suit$cluster[i]}
#  if(fit.suit$cluster[i]==3){
#    bn_for_c3[i]=fit.suit$cluster[i]}}

### create list
bn1 <- list(row.names(df[fit.suit$cluster==1,]))
bn2 <- list(row.names(df[fit.suit$cluster==2,]))
bn3 <- list(row.names(df[fit.suit$cluster==3,]))
