#Install Packages Needed

library(asaur) #Library asaur
library(survival)
attach(pharmacoSmoking) #Default dataframe
str(pharmacoSmoking) #To organize and determine the variables
head(pharmacoSmoking)
names(pharmacoSmoking)

# Summary of variables

summary (pharmacoSmoking) 
summary (yearsSmoking)
summary (grp)
summary (age)
summary (race)
summary (yearsSmoking)
summary (gender) 

# Kaplan-Meier Estimator

fit <- survfit(Surv(ttr, relapse)~1)
fit.grp <- survfit(Surv(ttr, relapse)~grp) 
summary(fit, censor=T)
summary(fit.grp)
plot(fit.grp, col=1:2, lwd=2, xlab="time to relapse", ylab="Kaplan-Meier survival estimates")
legend("topright",c("combination", "patchonly"), col=1:2, lty=1, bty="n")

fit.agegroup4 <- survfit(Surv(ttr, relapse)~ageGroup4) 
summary(fit.agegroup4)
plot(fit.agegroup4, col=1:4, lwd=2, xlab="time to relapse", ylab="Kaplan-Meier survival estimates")
legend("topright",c("21-34", "35-49", "50-64", "65+"), col=1:4, lty=1, bty="n")

fit.ageg2<-survfit(Surv(ttr,relapse)~ageGroup2)
plot(fit.ageg2, col=1:2, lwd=2, xlab="time to relapse", ylab="Kaplan Meier suvival estimates")
legend("topright", c("21-49", "50+"), col=1:2, lty = 1, bty = "n")
survdiff(formula=Surv(ttr,relapse)~ageGroup2)
survfit(formula=Surv(ttr,relapse)~ageGroup2)

fit.emp<-survfit(Surv(ttr,relapse)~employment)
summary(fit, censor=T)
summary(fit.emp)
plot(fit.emp, col=1:2:3, lwd=2, xlab = "time to relapse", ylab = "Kaplan-Meier survival estimates")
legend("topright", c("ft", "other", "pt"), col=1:2:3, lty=1, bty="n")

# Logrank Test - check p-value

survdiff(Surv(ttr, relapse)~grp) #significant
survdiff(Surv(ttr,relapse)~ageGroup2) #significant
survdiff(Surv(ttr,relapse)~employment) #not significant
survdiff(Surv(ttr,relapse)~race) #not significant
survdiff(Surv(ttr,relapse)~gender) #not significant
survdiff(Surv(ttr,relapse)~ageGroup4) #significant
survdiff(Surv(ttr,relapse)~age) #not significant
survdiff(Surv(ttr,relapse)~yearsSmoking) #not significant
survdiff(Surv(ttr,relapse)~levelSmoking) #not significant
survdiff(Surv(ttr,relapse)~priorAttempts) #significant
survdiff(Surv(ttr,relapse)~longestNoSmoke) #not significant
logrank.test<-survdiff(Surv(ttr,relapse)~grp)
cbind(logrank.test$obs,logrank.test$exp,logrank.test$var)
qchisq(0.95,1)
qchisq(0.95,2)

# Median of variables

survfit(formula = Surv(ttr,relapse)~grp)
survfit(formula=Surv(ttr,relapse)~employment)
survfit(formula=Surv(ttr,relapse)~ageGroup2)


# Cox Model and Wald Test

fit.full<-coxph(Surv(ttr, relapse)~grp+age+gender+race+employment+yearsSmoking+levelSmoking+priorAttempts+longestNoSmoke) #model with all covariates
ll.full<-as.numeric(logLik(fit.full))
fit.full
fit.rm.ls<-coxph(Surv(ttr,relapse)~grp+age+gender+race+employment+yearsSmoking+priorAttempts+longestNoSmoke) #model after removing levelSmoking
ll.rm.ls<-as.numeric(logLik(fit.rm.ls))
fit.rm.ls
fit.rm.g<-coxph(Surv(ttr,relapse)~grp+age+race+employment+yearsSmoking+priorAttempts+longestNoSmoke) #model after removing gender
ll.rm.g<-as.numeric(logLik(fit.rm.g))
fit.rm.g
fit.rm.pa<-coxph(Surv(ttr,relapse)~grp+age+race+employment+yearsSmoking+longestNoSmoke) #model after removing priorAttempts
ll.rm.pa<-as.numeric(logLik(fit.rm.pa))
fit.rm.pa
fit.rm.lns<-coxph(Surv(ttr,relapse)~grp+age+race+employment+yearsSmoking) #model after removing longestNoSmoke
ll.rm.lns<-as.numeric(logLik(fit.rm.lns))
fit.rm.lns
fit.rm.ys<-coxph(Surv(ttr,relapse)~grp+age+race+employment) #model after removing yearsSmoking
ll.rm.ys<-as.numeric(logLik(fit.rm.ys))
fit.rm.ys
fit.rm.r<-coxph(Surv(ttr,relapse)~grp+age+employment) #model after removing race / final model
ll.rm.r<-as.numeric(logLik(fit.rm.r))
fit.rm.r

bz.final <- basehaz(fit.rm.r, centered=FALSE)
# finds baseline Culm hazard
plot(bz.final$hazard~bz.final$time, xlab="time to relapse", ylab="Baseline Cumulative Hazard")

st.final<-exp(-bz.final$hazard)
# calculates the baseline survival function based on the Cox model
plot(st.final~bz.final$time, xlab="time to relapse", ylab="Basline Survival")

# Likelihood Ratio Test

pchisq(2*(ll.full-ll.rm.ls), df=1, lower.tail=F)
pchisq(2*(ll.rm.ls-ll.rm.g), df=1, lower.tail=F)
pchisq(2*(ll.rm.g-ll.rm.pa), df=1, lower.tail=F)
pchisq(2*(ll.rm.pa-ll.rm.lns), df=1,lower.tail=F)
pchisq(2*(ll.rm.lns-ll.rm.ys), df=1, lower.tail=F)
pchisq(2*(ll.rm.ys-ll.rm.r), df=1, lower.tail=F)

# Test For Proportional Hazards

cox.zph(fit.full)
cox.zph(fit.rm.r)


#Weibull on Cox model (ageGroup2)

result.survreg.age <- survreg(Surv(ttr,relapse) ~ageGroup2, dist = "weibull")
summary(result.survreg.age)
result.coxph.age <-coxph(Surv(ttr,relapse) ~ ageGroup2)
summary(result.coxph.age)
mu0.hat <- result.survreg.age$coef[1]
sigma.hat <- result.survreg.age$scale
alpha.hat <- 1/sigma.hat
lambda0.hat <- exp(-mu0.hat)
t.vec <- 0:182
surv0.vec <- 1 - pweibull(t.vec, shape = alpha.hat, scale = 1/lambda0.hat)
gamma.hat <- result.survreg.age$coef[2]
surv1.vec <- surv0.vec^(exp(-gamma.hat/sigma.hat))
coxph.surv.est <- survfit(result.coxph.age, newdata = data.frame(list(grp=c("50+", "21-49"))))
plot(coxph.surv.est, col=c("black", "red"), lwd=3, xlab = "time to relapse", ylab = "Survival Probability")
legend("topright",c("50+", "21-49"), col=1:2, lty=1, bty="n")
lines(surv0.vec ~ t.vec, col="red", lwd=2)
lines(surv1.vec ~ t.vec, lwd=2)     

#Weibull (Group)

result.survreg.grp <- survreg(Surv(ttr,relapse) ~grp, dist = "weibull")
summary(result.survreg.grp)
result.coxph.grp <-coxph(Surv(ttr,relapse) ~ grp)
summary(result.coxph.grp)
mu0.hat <- result.survreg.grp$coef[1]
sigma.hat <- result.survreg.grp$scale
alpha.hat <- 1/sigma.hat
lambda0.hat <- exp(-mu0.hat)
t.vec <- 0:182
surv0.vec <- 1 - pweibull(t.vec, shape = alpha.hat, scale = 1/lambda0.hat)
gamma.hat <- result.survreg.grp$coef[2]
surv1.vec <- surv0.vec^(exp(-gamma.hat/sigma.hat))
coxph.surv.est <- survfit(result.coxph.grp, newdata = data.frame(list(grp=c("combination", "patchOnly"))))
plot(coxph.surv.est, col=1:2, lwd=3, xlab = "time to relapse", ylab = "Survival Probability")
legend("topright",c("combination", "patch only"), col=1:2, lty=1, bty="n")
lines(surv0.vec ~ t.vec, lwd=2)
lines(surv1.vec ~ t.vec, col="red", lwd=2)   
