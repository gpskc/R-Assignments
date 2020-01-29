setwd("E:/UTD/BUAN 6356/Assignments/Database")

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

rep.kpss <- function(series,max.reps=5,level=0.95,trend=TRUE){
  for(i in 0:max.reps){
    suppressWarnings(pval <- tseries::kpss.test(series)$p.value)
    if(pval >= 1-level){return(c(i,0,pval))}
    if(trend){
      suppressWarnings(pval <- tseries::kpss.test(series,null="Trend")$p.value)
      if(pval >= 1-level){return(c(i,1,pval))}
    }
    series <- diff(series)
  }
}

# Question 1
mlb1 <- wpull('mlb1')
model11 <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar
             +frstbase+scndbase+thrdbase+shrtstop+catcher,data=mlb1)
summary(model11)
beta13 <- coef(model11)[14]
exp(beta13)-1
model12 <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar,data=mlb1)
anova(model12,model11)

# Question 2
gpa2 <- wpull('gpa2')
model21 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete,data=gpa2)
summary(model21)
model22 <- lm(colgpa~hsize+I(hsize^2)+hsperc+female+athlete,data=gpa2)
summary(model22)
model23 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete+female:athlete,data=gpa2)
summary(model23)
model24 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete+female:sat,data=gpa2)
summary(model24)

# Question 3
loanapp <- wpull('loanapp')
model31 <- lm(approve~white,data=loanapp)
summary(model31)
model32 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch
              +cosign+chist+pubrec+mortlat1+mortlat2+vr,data=loanapp)
summary(model32)
model33 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch
              +cosign+chist+pubrec+mortlat1+mortlat2+vr+white:obrat,data=loanapp)
summary(model33)
bk <- coef(model33)[2]+coef(model33)[17]*32
tc <- qt(0.975,df.residual(model33))
se <- sqrt(diag(vcov(model33))[2]+1024*diag(vcov(model33))[17]+64*vcov(model33)[2,17])
bk-tc*se
bk+tc*se

# Question 4
hprice1 <- wpull('hprice1')
model41 <- lm(price~lotsize+sqrft+bdrms,data=hprice1)
sqrt(diag(vcov(model41)))
sqrt(diag(vcovHC(model41)))
model42 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice1)
sqrt(diag(vcov(model42)))
sqrt(diag(vcovHC(model42)))
summary(lm(model41$resid^2~model41$fitted+I(model41$fitted^2)))
summary(lm(model42$resid^2~model42$fitted+I(model42$fitted^2)))

# Question 5
gpa1 <- wpull('gpa1')
model51 <- lm(colGPA~hsGPA+ACT+skipped+PC,data=gpa1)
summary(model51)
model51$resid
model51$fitted
model52 <- lm(model51$resid^2~model51$fitted+I(model51$fitted^2))
summary(model52)
min(fitted(model52))
model53 <- lm(colGPA~hsGPA+ACT+skipped+PC,weights=1/fitted(model52),data=gpa1)
summary(model53)
sqrt(diag(vcovHC(model53)))

# Question 6
ps36 <- fread('ps36.csv')
ts.plot(ps36$bitcoin)
ts.plot(ps36$sp500)
ts.plot(ps36$gold)
ts.plot(ps36$forex)
ts.plot(ps36$oil)
summary(lm(bitcoin~sp500+gold+forex+oil,data=ps36))
kpss.test(ps36$bitcoin)
kpss.test(diff(ps36$bitcoin))
kpss.test(ps36$sp500)
kpss.test(diff(ps36$sp500))
kpss.test(ps36$gold)
kpss.test(diff(ps36$gold))
kpss.test(ps36$forex)
kpss.test(diff(ps36$forex))
kpss.test(ps36$oil)
kpss.test(diff(ps36$oil))
summary(lm(diff(bitcoin)~diff(sp500)+diff(gold)+diff(forex)+diff(oil),data=ps36))
ps3617 <- fread('ps3617.csv')
acf(diff(ps3617$bitcoin))
pacf(diff(ps3617$bitcoin))



