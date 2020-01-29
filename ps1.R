setwd("E:/UTD/BUAN 6356/Assignments/ProblemSet1/Database")

library(data.table)
library(ggplot2)
library(RSQLite)
library(DBI)

con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)

wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}

# Question 1
wage1 <- wpull('wage1')
summary(wage1$educ)
summary(wage1$wage)
ggplot(wage1,aes(x=wage))+geom_density()+geom_vline(aes(xintercept=mean(wage1$wage)))
cpi1976 <- 100*5.06/8.85
cpi1976
cpi2010 <- 100*19.04/8.90
cpi2010
wage2010 <- mean(wage1$wage)*cpi2010/cpi1976
wage2010
table(wage1$female)

# Question 2
meap01 <- wpull('meap01')
summary(meap01)
sum(meap01$math4==100)
100*mean(meap01$math4==100)
nrow(meap01[math4==50])
cor(meap01$math4,meap01$read4)
sd(meap01$exppp)
expppA <- 6000
expppB <- 5500
100*(expppA-expppB)/expppB
100*(log(expppA)-log(expppB))

# Question 3
d401k <- wpull('401k')
summary(d401k)
model3 <- lm(prate~mrate,data=d401k)
summary(model3)
predict(model3,data.frame(mrate=3.5))

# Question 4
ceosal2 <- wpull('ceosal2')
summary(ceosal2)
nrow(ceosal2[ceoten==0])
summary(lm(log(salary)~ceoten,data=ceosal2))

# Question 5
wage2 <- wpull('wage2')
summary(wage2$wage)
summary(wage2$IQ)
sd(wage2$IQ)
model5.1 <- lm(wage~IQ,data=wage2)
summary(model5.1)
15*coef(model5.1)[2]
model5.2 <- lm(log(wage)~IQ,data=wage2)
summary(model5.2)
100*(exp(15*coef(model5.2)[2])-1)

# Question 6
meap93 <- wpull('meap93')
summary(lm(math10~expend,data=meap93))
summary(lm(math10~log(expend),data=meap93))
summary(meap93$math10)

# Question 7
hprice1 <- wpull('hprice1')
head(hprice1)
model7 <- lm(price~sqrft+bdrms,data=hprice1)
summary(model7)
1*coef(model7)[3]
140*coef(model7)[2]+1*coef(model7)[3]
predict(model7)[1]
price1 <- predict(model7,data.frame(sqrft=hprice1$sqrft[1],bdrms=hprice1$bdrms[1]))
price1
hprice1$price[1]-price1
residuals(model7)[1]

# Question 8
ceosal2 <- wpull('ceosal2')
summary(lm(log(salary)~log(sales)+log(mktval),data=ceosal2))
summary(lm(log(salary)~log(sales)+log(mktval)+profits,data=ceosal2))
summary(lm(log(salary)~log(sales)+log(mktval)+profits+ceoten,data=ceosal2))
cor(log(ceosal2$mktval),ceosal2$profits)

# Question 9
attend <- wpull('attend')
summary(attend)
model9 <- lm(atndrte~priGPA+ACT,data=attend)
summary(model9)
predict(model9,data.frame(priGPA=3.65,ACT=20))
attend[priGPA==3.65&ACT==20]
atndrteA <- predict(model9,data.frame(priGPA=3.1,ACT=21))
atndrteB <- predict(model9,data.frame(priGPA=2.1,ACT=26))
atndrteA
atndrteB
atndrteA-atndrteB

# Question 10
htv <- wpull('htv')
summary(htv)
100*nrow(htv[educ==12])/nrow(htv)
summary(lm(educ~motheduc+fatheduc,data=htv))
summary(lm(educ~motheduc+fatheduc+abil,data=htv))
model10 <- lm(educ~motheduc+fatheduc+abil+I(abil^2),data=htv)
summary(model10)
beta0 <- coef(model10)[1]
beta1 <- coef(model10)[2]
beta2 <- coef(model10)[3]
beta3 <- coef(model10)[4]
beta4 <- coef(model10)[5]
100*nrow(htv[abil<(-0.5*beta3/beta4)])/nrow(htv)
neweduc <- beta0+beta1*mean(htv$motheduc)+beta2*mean(htv$fatheduc)+beta3*htv$abil+beta4*(htv$abil)^2
plot(htv$abil,htv$educ,type='p',col="red",xlab='ability',ylab='education',main='ability vs. education')
lines(htv$abil,neweduc,type='p',col="green")
