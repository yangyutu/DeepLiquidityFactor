rm(list = ls())
library(anytime)
library(Hmisc)
setwd("E:\\Dropbox\\financeResearch\\factorReturnBenchmark")


PSdata <- read.csv("PSLiquidityFactor.csv")
PSdata$DATE <- anydate(PSdata$DATE)
names(PSdata)[1] <- "date"

# remove rows with missing values
PSdata <- PSdata[!(PSdata$PS_VWF == -99),]

# load market return
#get the market return and bond return
FFfactors <- read.csv("FamaFrenchMonthlyData.csv")
FFfactors$dateff<- anydate(FFfactors$dateff)
names(FFfactors)[1] <- "date"
mergeData <- merge(PSdata, FFfactors, by="date")
mktrf_cum <- cumprod(mergeData$mktrf + mergeData$rf +1) 

cum_liq <- cumprod(mergeData$PS_VWF+1)
par(mfrow = c(1,1))
plot(mergeData$date, cum_liq,type='l',ylim=c(0,100))
lines(mergeData$date, mktrf_cum, type='l')


