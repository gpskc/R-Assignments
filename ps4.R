setwd("E:/UTD/BUAN 6356/Assignments/Database")

rm(list=ls())
library(data.table)
library(ggplot2)
library(RSQLite)
library(DBI)
library(broom)
library(sandwich)
library(lmtest)
library(margins)
library(tseries)
library(forecast)
library(plm)

con <- dbConnect(SQLite(),'woolridge2.db')
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

tidyg <- function(model,vc,conf.int=FALSE,conf.level=0.95){
  tmodel <- tidy(model,conf.int,conf.level)
  tmodel$std.error <- sqrt(diag(vc))
  tmodel$statistic <- tmodel$estimate/tmodel$std.error
  tmodel$p.value <- 2*pnorm(-abs(tmodel$statistic))
  if(conf.int){
    tmodel$conf.low <-  tmodel$estimate-
      qnorm(1-(1-conf.level)/2)*tmodel$std.error
    tmodel$conf.high <- tmodel$estimate+
      qnorm(1-(1-conf.level)/2)*tmodel$std.error
  }
  return(tmodel)
}
tidyw <- function(model,...){
  return(tidyg(model,sandwich::vcovHC(model),...))
}
tidynw <- function(model,...){
  return(tidyg(model,sandwich::NeweyWest(model),...))
}
tidyhac <- function(model,...){
  warning('n must be much greater than T')
  return(tidyg(model,plm::vcovHC(model),...))
}

## Question 1
hprice1 <- wpull('hprice1')
model1 <- lm(price~(assess+bdrms+lotsize+sqrft)^3
                  +(assess+bdrms+lotsize+sqrft)^2
                  +(assess+bdrms+lotsize+sqrft)+colonial
                  +colonial:(assess+bdrms+lotsize+sqrft)^3
                  +colonial:(assess+bdrms+lotsize+sqrft)^2
                  +colonial:(assess+bdrms+lotsize+sqrft),data=hprice1)
best_model1 <- step(model1,k=log(nrow(hprice1)))
AIC(best_model1) # 910.4328
BIC(best_model1) # 925.2968
summary(best_model1)
summary(model)

## Question 2
gpa2 <- wpull('gpa2')
model2 <- lm(colgpa~(sat+tothrs+verbmath+hsize+hsrank+hsperc)^2
                    +sat+tothrs+verbmath+hsize+hsrank+hsperc
                    +athlete+female+white+black,data=gpa2)
best_model2 <- step(model2,k=log(nrow(gpa2)))
AIC(best_model2) # 6590.858
BIC(best_model2) # 6679.446
summary(best_model2)

## Question 3
mlb1 <- wpull('mlb1')
model3<- lm(log(salary)~teamsal+years+yrsallst+pcinc+allstar+gamesyr+hrunsyr+atbatsyr+rbisyr+sbasesyr+runsyr
                       +games+atbats+runs+hits+doubles+triples+hruns+rbis+bavg+bb+so+sbases+fldperc+slugavg
                       +nl+frstbase+scndbase+shrtstop+thrdbase+outfield+catcher+hispan+black,data=mlb1)
best_model3 <- step(model3,k=log(nrow(mlb1)))
AIC(best_model3) # 732.2394
BIC(best_model3) # 767.0376
summary(best_model3)

## Question 4
rental <- wpull('rental')
str(rental)
pdrental <- pdata.frame(rental,index=c('city','year'))
pdrental
pdrental$y90 <- as.numeric(pdrental$year==90)
pdrental$pctstu <- 100*(pdrental$enroll/pdrental$pop)
model4 <- plm(log(rent)~y90+log(pop)+log(avginc)+pctstu,model="pooling",data=pdrental)
summary(model4)
tidyhac(model4)
summary(plm(log(rent)~y90+log(pop)+log(avginc)+pctstu,model="fd",data=pdrental))
summary(plm(log(rent)~y90+log(pop)+log(avginc)+pctstu,model="within",data=pdrental))

## Question 5
murder <- wpull('murder')
murder
pdmurder <- pdata.frame(murder,index=c('id','year'))
pdmurder$exec
murder_9093 <- subset(pdmurder,year==90|year==93)
model5 <- plm(mrdrte~as.factor(year)+exec+unem,model="pooling",data=murder_9093)
summary(model5)
summary(plm(mrdrte~as.factor(year)+exec+unem,model="within",data=murder_9093))
tidyw(model5)
subset(murder,year==93)[order(-exec),c("state","exec")][1:2]
model5a <- plm(mrdrte~as.factor(year)+exec+unem,model="fd",data=subset(murder_9093,state!='TX'))
tidy(model5a)
tidyhac(model5a)
summary(plm(mrdrte~as.factor(year)+exec+unem,model="within",data=pdmurder))

## Question 6
airfare <- wpull('airfare')
pdairfare <- pdata.frame(airfare,index=c('id','year'))
model6 <- plm(log(fare)~as.factor(year)+bmktshr+log(dist)+I(log(dist)^2),model="pooling",data=pdairfare)
summary(model6)
model6a <- lm(log(fare)~as.factor(year)+bmktshr+log(dist)+I(log(dist)^2),data=airfare)
tidy(model6a,conf.int=TRUE)
tidyw(model6a,conf.int=TRUE)
exp(-coef(model6)[6]/(2*coef(model6)[7])) # 79.50877
range(airfare$dist)
summary(plm(log(fare)~as.factor(year)+bmktshr+log(dist)+I(log(dist)^2),model="within",data=pdairfare))

## Question 7
loanapp <- wpull('loanapp')
model7 <- glm(approve~white,family=binomial(),data=loanapp)
summary(model7)
predict(model7,data.table(white=c(1,0)),type='response') # 90.84% for whites and 70.78% for nonwhites 
predict(lm(approve~white,data=loanapp),data.table(white=c(1,0)))
model7a <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist
                      +pubrec+mortlat1+mortlat2+vr,family=binomial(),data=loanapp)
summary(model7a)

## Question 8
alcohol <- wpull('alcohol')
table(alcohol$employ)[2]/(table(alcohol$employ)[1]+table(alcohol$employ)[2])
table(alcohol$abuse)[2]/(table(alcohol$abuse)[1]+table(alcohol$abuse)[2])
model8 <- lm(employ~abuse,data=alcohol)
tidy(model8)
tidyw(model8)
model8a <- glm(employ~abuse,family=binomial(),data=alcohol)
summary(model8a)
predict(model8,data.frame(abuse=0))
predict(model8,data.frame(abuse=1))
predict(model8a,data.frame(abuse=0),type='response')
predict(model8a,data.frame(abuse=1),type='response')
model8b <- lm(employ~abuse+age+I(age^2)+educ+I(educ^2)+married+famsize+white+northeast+midwest+south
                    +centcity+outercity+qrt1+qrt2+qrt3,data=alcohol)
tidy(model8b)
model8c <- glm(employ~abuse+age+I(age^2)+educ+I(educ^2)+married+famsize+white+northeast+midwest+south
                     +centcity+outercity+qrt1+qrt2+qrt3,family=binomial(),data=alcohol)
summary(model8c)
coef(model8c)[2]/sqrt(diag(vcov(model8c)))[2]
model8d <- lm(employ~exhealth+vghealth+goodhealth+fairhealth
                     +abuse+age+I(age^2)+educ+I(educ^2)+married+famsize+white+northeast+midwest+south
                     +centcity+outercity+qrt1+qrt2+qrt3,data=alcohol)
tidy(model8d)
plot(alcohol$abuse,model8c$resid,xlab='abuse',ylab='residual')
cor(alcohol$abuse,alcohol$fathalc)
cor(alcohol$abuse,alcohol$mothalc)

## Question 9
fertil1 <- wpull('fertil1')
fertil1
fertil1$y74 <- as.numeric(fertil1$year == 74)
fertil1$y76 <- as.numeric(fertil1$year == 76)
fertil1$y78 <- as.numeric(fertil1$year == 78)
fertil1$y80 <- as.numeric(fertil1$year == 80)
fertil1$y82 <- as.numeric(fertil1$year == 82)
fertil1$y84 <- as.numeric(fertil1$year == 84)
model9 <- glm(kids~educ+age+I(educ^2)+black+east+northcen+west+farm+othrural+town+smcity
                  +y74+y76+y78+y80+y82+y84,family=poisson(),data=fertil1)
summary(model9)
cor(fertil1$kids,model9$fitted)^2
model9a <- lm(kids~educ+age+I(educ^2)+black+east+northcen+west+farm+othrural+town+smcity
                  +y74+y76+y78+y80+y82+y84,data=fertil1)
summary(model9a)




