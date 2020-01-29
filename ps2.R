setwd("E:/UTD/BUAN 6356/Assignments/Database")

library(data.table)
library(ggplot2)
library(RSQLite)
library(DBI)
library(broom)
library(sandwich)
library(lmtest)

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
vote1 <- wpull('vote1')
summary(lm(voteA~log(expendA)+log(expendB)+prtystrA,data=vote1))
summary(lm(voteA~log(expendA)+I(log(expendB)-log(expendA))+prtystrA,data=vote1))

# Question 2
lawsch85 <- wpull('lawsch85')
summary(lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank,data=lawsch85))
summary(lm(log(salary)~log(libvol)+log(cost)+rank,data=lawsch85))$r.squared
summary(lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank+clsize+faculty,data=lawsch85))$r.squared

# Question 3
hprice1 <- wpull('hprice1')
model3 <- summary(lm(log(price)~sqrft+bdrms,data=hprice1))
150*coef(model3)[2]+coef(model3)[3]
model3a <- lm(log(price)~I(sqrft-150*bdrms)+bdrms,data=hprice1)
tidy(model3a,conf.int=TRUE,conf.level=0.95)

# Question 4
wage2 <- wpull('wage2')
summary(lm(log(wage)~educ+exper+tenure,data=wage2))
summary(lm(log(wage)~educ+exper+I(exper+tenure),data=wage2))

# Question 5
t401ksubs <- wpull('401ksubs')
nrow(t401ksubs[fsize==1])
model5 <- lm(nettfa~inc+age,data=t401ksubs[fsize==1])
summary(model5)
tstat <- (coef(model5)[3]-1)/sqrt(diag(vcov(model5)))[3]
pt(-abs(tstat),df=2014)
summary(lm(nettfa~inc,data=t401ksubs[fsize==1]))

# Question 6
kielmc <- wpull('kielmc')
summary(lm(log(price)~log(dist),data=kielmc[year==1981]))
summary(lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age,data=kielmc[year==1981]))
summary(lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age
           +I(log(intst^2)),data=kielmc[year==1981]))
summary(lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age
           +I(log(intst^2))+I(log(dist^2)),data=kielmc[year==1981]))

# Question 7
wage1 <- wpull('wage1')
model7 <- lm(log(wage)~educ+exper+I(exper^2),data=wage1)
summary(model7)
coef(model7)[3]+2*coef(model7)[4]*5
coef(model7)[3]+2*coef(model7)[4]*20
exper0 <- coef(model7)[3]/(-2*coef(model7)[4])
exper0
nrow(wage1[exper>=exper0])

# Question 8
wage2 <- wpull('wage2')
summary(lm(log(wage)~educ+exper+I(educ*exper),data=wage2))
model8a <- lm(log(wage)~educ+exper+I(educ*(exper-10)),data=wage2)
tidy(model8a,conf.int=TRUE,conf.level=0.95)

# Question 9
gpa2 <- wpull('gpa2')
model9 <- lm(sat~hsize+I(hsize^2),data=gpa2)
summary(model9)
hsize0 <- coef(model9)[2]/(-2*coef(model9)[3])
hsize0
model9a <- lm(log(sat)~hsize+I(hsize^2),data=gpa2)
summary(model9a)
hsize0l <- coef(model9a)[2]/(-2*coef(model9a)[3])
hsize0l

# Question 10
hprice1 <- wpull('hprice1')
model10 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice1)
summary(model10)
exp(predict(model10,data.frame(lotsize=20000,sqrft=2500,bdrms=4)))
summary(lm(price~lotsize+sqrft+bdrms,data=hprice1))$r.squared


