# libraries
rm(list=ls())
library(data.table)
library(ggplot2)
library(lmtest)
library(forecast)
library(partykit)

# data generation
n <- 500
set.seed(75080)

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z+50
y   <- -100*z+1100+50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z+80
y   <- -80*z+1200+50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z+30
y   <- -120*z+1000+50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

dtable <- merge(dt1,dt2,all=TRUE)
dtable <- merge(dtable,dt3,all=TRUE)

# kmeans.wss function
kmeans.wss <- function(data,kmax,seed=75080) {
  wss <- rep(NA,kmax)
  for (i in 1:kmax) { 
    set.seed(seed)
    wss[i] <- kmeans(data,centers=i,nstart=10,iter.max=50)$tot.withinss
  }
  return(wss)
}

# hclust.wss function
hclust.wss <- function(data,kmax) {
  wss <- rep(NA,kmax)
  for (i in 1:kmax) {
    clust <- cutree(hclust(dist(data)),i)
    means <- data[,lapply(.SD,mean),by=clust]
    diffs <- data-means[clust,2:(ncol(data)+1)]
    wss[i]<- sum(diffs^2)
  }
  return(wss)
}

# plot.wss function
plot.wss <- function(wss) {
  plot(1:NROW(wss),wss,type='b',xlab='Number of Clusters',ylab='Total WSS')
}

# eratio.wss function
eratio.wss <- function(wss) {
  n <- NROW(wss)
  dss <- -diff(wss)
  dss <- c(wss[1]/log(n),dss)
  erat <- dss[1:(n-1)]/dss[2:n]
  gss <- log(1+dss/wss)
  grat <- gss[1:(n-1)]/gss[2:n]
  return(c(which.max(erat),which.max(grat)))
}

## Question 2 - pooled & within models
dtable$group <- as.factor(dtable$group)
pooled <- lm(sat~income,data=dtable)           # pooled model
within <- lm(sat~income+group-1,data=dtable)   # within model
model1 <- lm(sat~income,data=dtable[group==1]) # group1 model
model2 <- lm(sat~income,data=dtable[group==1]) # group2 model
model3 <- lm(sat~income,data=dtable[group==1]) # group3 model

summary(model1) # sat = 2095.34 - 19.85*income
summary(model2) # sat = 2505.12 - 16.33*income
summary(model3) # sat = 1722.48 - 24.03*income
summary(pooled) # sat = 950.891 + 2.792*income
summary(within) # sat = 000.000 - 20.17*income + 2111.26*group1 + 2812.18*group2 + 1605.30*group3

## Question 3 - recursive partition
rpart1 <- ctree(sat~income,data=dtable) # recursive partition with income
rpart2 <- ctree(sat~group,data=dtable)  # recursive partition with groups
rpart3 <- ctree(sat~income+group,data=dtable) # with both income & groups

plot(rpart1) # inner nodes = 08 and terminal nodes = 09
plot(rpart2) # inner nodes = 02 and terminal nodes = 03
plot(rpart3) # inner nodes = 29 and terminal nodes = 30

## Question 4 - glmtree model
tree <- glmtree(sat~income|group,data=dtable) # glmtree model
plot(tree) # got same results obtained in fixed effects model

## Question 5 - kmeans without scaling
xtable <- data.table('sat'=dtable$sat,'income'=dtable$income) # extracting sat and income variables
xmeans <- dtable[,lapply(.SD,mean),by=group,.SDcols=2:3] # finding actual centers of both variables

kwss <- kmeans.wss(xtable,kmax=10)
plot.wss(kwss) # Elbow plot of Total WSS
eratio.wss(kwss) # Optimal clusters is 2

kmodel <- kmeans(xtable,centers=2,nstart=10,iter.max=50)
kgroup <- kmodel$cluster
table(kgroup) # Cluster Size
kmodel$centers # Centers for K-means
xmeans # Actual centers of variables
table(kgroup,dtable$group)/500 # rough split: group1 = 60/40, group2 = 90/10, group3 = 25/75

## Question 6 - hierarchical cluster without scaling
hwss <- hclust.wss(xtable,kmax=10)
plot.wss(hwss) # Elbow plot of Total WSS
eratio.wss(hwss) # Optimal Clusters is 4

hmodel <- hclust(dist(xtable))
hgroup <- cutree(hmodel,4)
table(hgroup) # Cluster Size
table(hgroup,dtable$group)/500 # rough split: group1 = 45/35/15/5, group2 = 55/5/40/0, group3 = 20/50/5/25

## Question 7 - new models with clusters as groups
xtable$kgroup <- as.factor(kgroup) # Adding kgroup
xtable$hgroup <- as.factor(hgroup) # Adding hgroup
table(xtable$kgroup,xtable$hgroup) # Two-Way table

pooled_k <- lm(sat~income,data=xtable)          # K-means pooled
within_k <- lm(sat~income+kgroup-1,data=xtable) # K-means within
pooled_h <- lm(sat~income,data=xtable)          # H-clust pooled
within_h <- lm(sat~income+hgroup-1,data=xtable) # H-clust within

summary(pooled_k) # sat = 950.891 + 2.792*income
summary(pooled_h) # sat = 950.891 + 2.792*income
summary(within_k) # sat = 000.000 + 0.735*income + 1155*kgroup1 + 934*kgroup2
summary(within_h) # sat = 000.000 + 0.139*income + 1140*hgroup1 + 997*hgroup2 + 1272*hgroup3 + 826*hgroup4

## Question 8 - kmeans using only income
kwss1 <- kmeans.wss(xtable$income,kmax=10)
plot.wss(kwss1) # Elbow plot of Total WSS
eratio.wss(kwss1) # Optimal clusters is 3

kmodel1 <- kmeans(xtable,centers=3,nstart=10,iter.max=50)
table(kmodel1$cluster) # Cluster Size
kmodel1$centers # Centers for K-means
xmeans # Actual centers for variables
table(kmodel1$cluster,dtable$group)/500 # accuracy: cluster1 = 48%, cluster2 = 71%, cluster3 = 47%

## Question 9 - kmeans with scaling
kwss2 <- kmeans.wss(xtable[,.(sat,income)],kmax=10)
plot.wss(kwss2) # Elbow plot of Total WSS
eratio.wss(kwss2) # Optimal clusters is 3

kmodel2 <- kmeans(scale(xtable[,.(sat,income)]),centers=3,nstart=10,iter.max=50)
table(kmodel2$cluster) # Cluster Size
kmodel2$centers # Centers for K-means
xmeans # Actual centers for variables
table(kmodel2$cluster,dtable$group)/500 # accuracy: cluster1 = 74%, cluster2 = 100%, cluster3 = 66%


