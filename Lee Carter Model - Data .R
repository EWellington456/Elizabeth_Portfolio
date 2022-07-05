#installing the packages

install.packages("demography")
install.packages("forecast")
install.packages("lifecontingencies")
install.packages("StMoMo")
install.packages("gnm")
install.packages("bootstrap")

#loading the packages

library(demography)
library(forecast)
library(lifecontingencies)
library(StMoMo)
library(gnm)
library(bootstrap)
#loading the UK mortality data into R

UKdata <- hmd.mx(country = "GBR_NP", username ="ew18422@essex.ac.uk",
                 password = "elizzie123")
UKdata

load(file="mortalityDatasets.RData")


#Mortality rate against age of males, females and both aged 0 - 100
par(mfrow=c(1,3))
plot(UKdata, series = "male", datatype = "rate", main = "Male rates")

plot(UKdata, series = "female", datatype = "rate", main = "Female rates")

plot(UKdata, series = "total", datatype = "rate", main = "Total rates")

#Mortality rate against the year 1922 - 2018 for males, females and both
plot(UKdata, series = "male", plot.type = "time", datatype = "rate", xlab = "years", main = "male rates")

plot(UKdata, series ="female", plot.type = "time", datatype = "rate", xlab = "years", main = "female rates")

plot(UKdata, series = "total", plot.type = "time", datatype = "rate", xlab = "years", main = "total rates")

#fitting the lee carter model for the UK

UKLcaM<-lca(UKdata, series="male", max.age=100)
UKLcaF<-lca(UKdata, series="female", max.age=100)
UKLcaT<-lca(UKdata, series="total", max.age=100)

#plots of each parameter UK Lee Carter Model 
par(mfrow=c(1,3))

#ax
plot(UKLcaT$ax, main="ax", xlab="Age",ylab="ax",type="l")
lines(x=UKLcaF$age, y=UKLcaF$ax, main="ax", col="red")
lines(x=UKLcaM$age, y=UKLcaM$ax, main="ax", col="blue")
legend("topleft" , c("Male","Female","Total"),cex=0.8,col=c("blue","red","black"),lty=1);
#bx
plot(UKLcaT$bx, main="bx", xlab="Age",ylab="bx",type="l")
lines(x=UKLcaF$age, y=UKLcaF$bx, main="bx", col="red")
lines(x=UKLcaM$age, y=UKLcaM$bx, main="bx", col="blue")
legend("topright" , c("Male","Female","Total"),
       cex=0.8,col=c("blue","red","black"),lty=1);
#kt
plot(UKLcaT$kt, main="kt", xlab="Year",ylab="kt",type="l")
lines(x=UKLcaF$year, y=UKLcaF$kt, main="kt", col="red")
lines(x=UKLcaM$year, y=UKLcaM$kt, main="kt", col="blue")
legend("topright" , c("Male","Female","Total"), 
       cex=0.8,col=c("blue","red","black"),lty=1);


#forecasting the Lee Carter model for the UK
forecast_maleUK <- forecast(UKLcaM, h = 50)
forecast_femaleUK <- forecast(UKLcaF, h = 50)
forecast_totalUK <- forecast(UKLcaT, h = 50)

#plot of the forecast UK
par(mfrow = c(1,3))
plot(forecast_maleUK$kt.f, main = "Male")
plot(forecast_femaleUK$kt.f, main = "female")
plot(forecast_totalUK$kt.f, main = "total")


#past and forecasted rates binded in the same matirx UK

ratesMUK<-cbind(UKdata$rate$male[1:100,],forecast_maleUK$rate$male[1:100,])
ratesFUK<-cbind(UKdata$rate$female[1:100,],forecast_femaleUK$rate$female[1:100,])
ratesTUK<-cbind(UKdata$rate$total[1:100,],forecast_totalUK$rate$total[1:100,])

par(mfrow=c(1,1)) 
plot(seq(min(UKdata$year),max(UKdata$year)+50),ratesF[65,],
     col="red",xlab="Years",ylab="Death Rates",type="l") 
lines(seq(min(UKdata$year),max(UKdata$year)+50),ratesM[65,],
      col="blue",xlab="Years",ylab="Death Rates") 
lines(seq(min(UKdata$year),max(UKdata$year)+50),ratesT[65,],
      col="black",xlab="Years",ylab="Death Rates") 
legend("topright", c("Male","Female","Total"),cex=0.8,col=c("blue","red","black"),lty=1);



#loading the Korea mortality data into R 

Koreadata <- hmd.mx(country = "KOR", username ="ew18422@essex.ac.uk",
                    password = "elizzie123")
Koreadata


#Mortality rate against age of males, females and both aged 0 - 100
par(mfrow=c(1,3))
plot(Koreadata, series = "male", datatype = "rate", main = "Male rates")

plot(Koreadata, series = "female", datatype = "rate", main = "Female rates")

plot(Koreadata, series = "total", datatype = "rate", main = "Total rates")



plot(Koreadata, series = "male", plot.type = "time", datatype = "rate", xlab = "years", main = "male rates")

plot(Koreadata, series ="female", plot.type = "time", datatype = "rate", xlab = "years", main = "female rates")

plot(Koreadata, series = "total", plot.type = "time", datatype = "rate", xlab = "years", main = "total rates")


#fitting the Lee Carter model for Korea
KoreaLcaM<-lca(Koreadata, series="male", max.age=100)
KoreaLcaF<-lca(Koreadata, series="female", max.age=100)
KoreaLcaT<-lca(Koreadata, series="total", max.age=100)

#plots for each parameter Korea
par(mfrow=c(1,3))

#ax Korea
plot(KoreaLcaT$ax, main="ax", xlab="Age",ylab="ax",type="l")
lines(x=KoreaLcaF$age, y=SwitLcaF$ax, main="ax", col="red")
lines(x=KoreaLcaM$age, y=SwitLcaM$ax, main="ax", col="blue")
legend("topleft" , c("Male","Female","Total"),cex=0.8,col=c("blue","red","black"),lty=1);

#bx Korea
plot(KoreaLcaT$bx, main="bx", xlab="Age",ylab="bx",type="l")
lines(x=KoreaLcaF$age, y=KoreaLcaF$bx, main="bx", col="red")
lines(x=KoreaLcaM$age, y=KoreaLcaM$bx, main="bx", col="blue")
legend("topright" , c("Male","Female","Total"),
       cex=0.8,col=c("blue","red","black"),lty=1);

#kt Korea
plot(KoreaLcaT$kt, main="kt", xlab="Year",ylab="kt",type="l")
lines(x=KoreaLcaF$year, y=KoreaLcaF$kt, main="kt", col="red")
lines(x=KoreaLcaM$year, y=KoreaLcaM$kt, main="kt", col="blue")
legend("topright" , c("Male","Female","Total"), 
       cex=0.8,col=c("blue","red","black"),lty=1);

#forecasting the model 
forecast_male_Korea <- forecast(KoreaLcaM, h = 50)
forecast_female_Korea <- forecast(KoreaLcaF, h = 50)
forecast_total_Korea <- forecast(KoreaLcaT, h = 50)

#plot of the forecast Korea
par(mfrow = c(1,3))
plot(forecast_male_Korea$kt.f, main = "Male")
plot(forecast_female_Korea$kt.f, main = "female")
plot(forecast_total_Korea$kt.f, main = "total")

#past and forecasted rates binded in the same matirx Korea

ratesM<-cbind(Koreadata$rate$male[1:100,],forecast_male_Korea$rate$male[1:100,])
ratesF<-cbind(Koreadata$rate$female[1:100,],forecast_female_Korea$rate$female[1:100,])
ratesT<-cbind(Koreadata$rate$total[1:100,],forecast_total_Korea$rate$total[1:100,])

par(mfrow=c(1,1)) 
plot(seq(min(Koreadata$year),max(Koreadata$year)+50),ratesF[65,],
     col="red",xlab="Years",ylab="Death Rates",type="l") 
lines(seq(min(Koreadata$year),max(Koreadata$year)+50),ratesM[65,],
      col="blue",xlab="Years",ylab="Death Rates") 
lines(seq(min(Koreadata$year),max(Koreadata$year)+50),ratesT[65,],
      col="black",xlab="Years",ylab="Death Rates") 
legend("topright", c("Male","Female","Total"),cex=0.8,col=c("blue","red","black"),lty=1);


#using booth maindonald smith method to forecast

#fitting the model 
UKBmsM<-bms(UKdata, series="male", max.age=100, breakmethod = "bms")
UKBmsF<-bms(UKdata, series="female", max.age=100, breakmethod = "bms")
UKBmsT<-bms(UKdata, series="total", max.age=100, breakmethod = "bms")

#forecasting the model
forbms_male_UK <- forecast(UKBmsM, minperiod = 20, h = 50)
forbms_female_UK <- forecast(UKBmsF, minperiod = 20, h = 50)
forbms_total_UK <- forecast(UKBmsT, minperiod = 20, h = 50)

par(mfrow = c(1,3))
plot(forbms_male_UK$kt.f, main = "Male")
plot(forbms_female_UK$kt.f, main = "female")
plot(forbms_total_UK$kt.f, main = "total")

#using the Lee Miller method to forecast 

#fitting the model
UK_LeeMiller_M <- lca(UKdata, series="male", adjust = "e0", years = 1950:max(UKdata$year))
UK_LeeMiller_F <- lca(UKdata, series="female", adjust = "e0", years = 1950:max(UKdata$year))
UK_LeeMiller_T <- lca(UKdata, series="total", adjust = "e0", years = 1950:max(UKdata$year))

#forecasting the model
forLM_male_UK <- forecast(UK_LeeMiller_M, h = 50, jumpchoice = "actual")
forLM_female_UK <- forecast(UK_LeeMiller_F, h = 50, jumpchoice = "actual")
forLM_total_UK <- forecast(UK_LeeMiller_T, h = 50, jumpchoice = "actual")

par(mfrow = c(1,3))
plot(forLM_male_UK$kt.f, main = "Male")
plot(forLM_female_UK$kt.f, main = "female")
plot(forLM_total_UK$kt.f, main = "total")



USING ST MOMO


#installing the packages

install.packages("demography")
install.packages("forecast")
install.packages("lifecontingencies")
install.packages("StMoMo")
install.packages("gnm")
install.packages("bootstrap")

#loading the packages

library(demography)
library(forecast)
library(lifecontingencies)
library(StMoMo)
library(gnm)
library(bootstrap)
#loading the UK mortality data into R

UKdata <- hmd.mx(country = "GBR_NP", username ="ew18422@essex.ac.uk",
                 password = "elizzie123")
UKdata




UKStMoMOmale <- StMoMoData(UKdata,series = "male")
UKStMoMOfemale <- StMoMoData(UKdata, series = "female")
UKStMoMOtotal <- StMoMoData(UKdata, series = "total")
summary(UKStMoMOmale)
summary(UKStMoMOfemale)
summary(UKStMoMOtotal)

#Defining the Lee Carter Model with constraints already there
LeeCarter <- lc()
LeeCarter

#Fitting the model of gender and ages
#for male

UKmale <- StMoMoData(UKdata, series = "male")
ages.fit <- 55:110
years.fit <- 1970:2016

LCfitm <- fit(LeeCarter, data = UKmale, ages.fit = ages.fit,
              years.fit = years.fit)
LCfitm

plot(LCfitm)


#residuals of model for male
Leecarter_residuals_male <- residuals(LCfitm)
Leecarter_residuals_male

plot(Leecarter_residuals_male, type = "colourmap", reslim = c(-3.5, 3.5))

#for female
UKfemale <- StMoMoData(UKdata, series = "female")
ages.fit <- 55:110
years.fit <- 1970:2016

LCfitf <- fit(LeeCarter, data = UKfemale, ages.fit = ages.fit,
              years.fit = years.fit)
LCfitf

plot(LCfitf, nCol = 2) 

#residuals of model for female
Leecarter_residuals_female <- residuals(LCfitf)
Leecarter_residuals_female


#residuals of model for male
Leecarter_residuals_male <- residuals(LCfitm)
Leecarter_residuals_male

plot(Leecarter_residuals_male, type = "colourmap", reslim = c(-3.5, 3.5))

#for female
UKfemale <- StMoMoData(UKdata, series = "female")
ages.fit <- 55:110
years.fit <- 1970:2016

LCfitf <- fit(LeeCarter, data = UKfemale, ages.fit = ages.fit,
              years.fit = years.fit)
LCfitf

plot(LCfitf, nCol = 2) 

#residuals of model for female
Leecarter_residuals_female <- residuals(LCfitf)
Leecarter_residuals_female
#residuals of model for male
Leecarter_residuals_male <- residuals(LCfitm)
Leecarter_residuals_male

plot(Leecarter_residuals_male, type = "colourmap", reslim = c(-3.5, 3.5))

#for female
UKfemale <- StMoMoData(UKdata, series = "female")
ages.fit <- 55:110
years.fit <- 1970:2016

LCfitf <- fit(LeeCarter, data = UKfemale, ages.fit = ages.fit,
              years.fit = years.fit)
LCfitf

plot(LCfitf, nCol = 2) 

#residuals of model for female
Leecarter_residuals_female <- residuals(LCfitf)
Leecarter_residuals_female

#residuals of model for male
Leecarter_residuals_male <- residuals(LCfitm)
Leecarter_residuals_male

plot(Leecarter_residuals_male, type = "colourmap", reslim = c(-3.5, 3.5))

#for female
UKfemale <- StMoMoData(UKdata, series = "female")
ages.fit <- 55:110
years.fit <- 1970:2016

LCfitf <- fit(LeeCarter, data = UKfemale, ages.fit = ages.fit,
              years.fit = years.fit)
LCfitf

plot(LCfitf, nCol = 2) 

#residuals of model for female
Leecarter_residuals_female <- residuals(LCfitf)
Leecarter_residuals_female


#forecasting the model/mortality rates
#male
LeeCartermalefor <- forecast(LCfitm, h = 50)
LeeCartermalefor

plot(LeeCartermalefor, only.kt = TRUE)

#female
LeeCarterfemalefor <- forecast(LCfitf, h = 50)
LeeCarterfemalefor

plot(LeeCarterfemalefor, only.kt = TRUE)

#total
LeeCartertotalfor <- forecast(LCfittotal, h = 50)
LeeCartertotalfor

plot(LeeCartertotalfor, only.kt = TRUE)

#graph of comparing the forecast before and after

#simulating trajectories

#female
LeeCarterSimfemale <- simulate(LCfitf, nsim = 500, h = 50)
LeeCarterSimfemale

#male
LeeCarterSimMale  <- simulate(LCfitm, nsim = 500, h = 50 )
LeeCarterSimMale

#total
LeeCarterSimtotal <- simulte(LCfittotal, nsim = 500, h = 50)
LeeCarterSimtotal





#parameter uncertainty and bootstrapping

LCbootUKMale <- bootstrap(LCfitm, nBoot = 5000, type = "semiparametric")
LCbootUKMale
plot(LCbootUKMale)
LCSimUKMale <- simulate(LCbootUKMale, nsim = 5000, h = 24)
LCSimUKMale

#forecasting the model/mortality rates
#male
LeeCartermalefor <- forecast(LCfitm, h = 50)
LeeCartermalefor

plot(LeeCartermalefor, only.kt = TRUE)

#female
LeeCarterfemalefor <- forecast(LCfitf, h = 50)
LeeCarterfemalefor

plot(LeeCarterfemalefor, only.kt = TRUE)

#total
LeeCartertotalfor <- forecast(LCfittotal, h = 50)
LeeCartertotalfor

plot(LeeCartertotalfor, only.kt = TRUE)

#graph of comparing the forecast before and after

#simulating trajectories

#female
LeeCarterSimfemale <- simulate(LCfitf, nsim = 500, h = 50)
LeeCarterSimfemale

#male
LeeCarterSimMale  <- simulate(LCfitm, nsim = 500, h = 50 )
LeeCarterSimMale

#total
LeeCarterSimtotal <- simulte(LCfittotal, nsim = 500, h = 50)
LeeCarterSimtotal





#parameter uncertainty and bootstrapping

LCbootUKMale <- bootstrap(LCfitm, nBoot = 5000, type = "semiparametric")
LCbootUKMale
plot(LCbootUKMale)
LCSimUKMale <- simulate(LCbootUKMale, nsim = 5000, h = 24)
LCSimUKMale
