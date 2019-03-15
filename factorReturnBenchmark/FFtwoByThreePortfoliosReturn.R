rm(list = ls())
library(anytime)
library(Hmisc)
setwd("E:\\Dropbox\\financeResearch\\factorReturnBenchmark")

FFPorts <- read.csv("FFtwoByThreePortfolios.csv")
FFPorts$date<- anydate(FFPorts$date)
names(FFPorts)[1] <- "date"


png("FFPortVWReturnHist1926_2018.png", width = 600, height = 900)
par(mfrow = c(3,2))
hist(FFPorts$smlo_vwret*100,20,xlab = "smlo monthly return(%)")
hist(FFPorts$smme_vwret*100,20,xlab = "smme monthly return(%)")
hist(FFPorts$smhi_vwret*100,20,xlab = "smhi monthly return(%)")
hist(FFPorts$bilo_vwret*100,20,xlab = "bilo monthly return(%)")
hist(FFPorts$bime_vwret*100,20,xlab = "bime monthly return(%)")
hist(FFPorts$bihi_vwret*100,20,xlab = "bihi monthly return(%)")

dev.off()

png("FFPortsCumReturnTime1926_2018.png", width = 600, height = 600)
par(mfrow = c(1,1))
smlo_cum <- cumprod(FFPorts$smlo_vwret+1)
smme_cum <- cumprod(FFPorts$smme_vwret+1)
smhi_cum <- cumprod(FFPorts$smhi_vwret+1)
bilo_cum <- cumprod(FFPorts$bilo_vwret+1)
bime_cum <- cumprod(FFPorts$bime_vwret+1)
bihi_cum <- cumprod(FFPorts$bihi_vwret+1)

#get the market return and bond return
FFfactors <- read.csv("FamaFrenchMonthlyData.csv")
FFfactors$dateff<- anydate(FFfactors$dateff)
names(FFfactors)[1] <- "date"
mktrf_cum <- cumprod(FFfactors$mktrf + FFfactors$rf +1) 
rf_cum <- cumprod(FFfactors$rf+1) 


colorSet <- rainbow(8)

plot(FFPorts$date, smlo_cum*100,ylab = "portfolio cumulative return(%)",type='l',ylim = c(0, 35800000),col=colorSet[1])
lines(FFPorts$date, smme_cum*100,type='l',col=colorSet[2])
lines(FFPorts$date, smhi_cum*100,,type='l',col=colorSet[3])
lines(FFPorts$date, bilo_cum*100,type='l',col=colorSet[4])
lines(FFPorts$date, bime_cum*100,type='l',col=colorSet[5])
lines(FFPorts$date, bihi_cum*100,,type='l',col=colorSet[6])
lines(FFfactors$date, mktrf_cum*100,type='l',col=colorSet[7])
lines(FFfactors$date, rf_cum*100,,type='l',col=colorSet[8])


legend(-500, 27550000, legend=c("smlo", "smme","smhi","bilo","bime","bihi","market","shortTermbond"),lty=1,col = colorSet)
dev.off()

png("FFPortsCumReturnTime1926_2018_Large.png", width = 600, height = 600)
par(mfrow = c(1,1))



colorSet <- rainbow(8)
smlo_cum[smlo_cum < 0] <- 0.1
smme_cum[smme_cum < 0] <- 0.1
smhi_cum[smhi_cum < 0] <- 0.1
bilo_cum[bilo_cum < 0] <- 0.1
bime_cum[bime_cum < 0] <- 0.1
bihi_cum[bihi_cum < 0] <- 0.1



plot(FFPorts$date, smlo_cum*100,ylab = "portfolio cumulative return(%)",type='l',ylim = c(1000, 35800000),col=colorSet[1],xlim=as.Date(c("2000-01-01","2020-01-01")),log="y")
lines(FFPorts$date, smme_cum*100,type='l',col=colorSet[2])
lines(FFPorts$date, smhi_cum*100,,type='l',col=colorSet[3])
lines(FFPorts$date, bilo_cum*100,type='l',col=colorSet[4])
lines(FFPorts$date, bime_cum*100,type='l',col=colorSet[5])
lines(FFPorts$date, bihi_cum*100,,type='l',col=colorSet[6])
lines(FFfactors$date, mktrf_cum*100,type='l',col=colorSet[7])
lines(FFfactors$date, rf_cum*100,,type='l',col=colorSet[8])


legend("bottom", legend=c("smlo", "smme","smhi","bilo","bime","bihi","market","shortTermbond"),lty=1,col = colorSet)
dev.off()


### now calculate the rolling mean and standard deviation of returns
library(zoo)

prodMean <- function(x){
  prod <- prod(1+x)
  n <- length(x)
  res <- prod^(1/n) - 1
  return(res)
}

rollCal <- function(x, width){
  rollMean <- rollapply(x, width = width, FUN=prodMean, align = "right")
  rollSD <- rollapply(x, width = width, FUN=sd, align = "right")
  rollSharpe <- rollMean/rollSD
  res <- data.frame(rollMean=rollMean, rollSD=rollSD, rollSharpe=rollSharpe)
  return(res)
}
width <- 60
time <- FFPorts$date[width:length(FFPorts$date)]
smloRoll <- rollCal(FFPorts$smlo_vwret, width)
smmeRoll <- rollCal(FFPorts$smme_vwret, width)
smhiRoll <- rollCal(FFPorts$smhi_vwret, width)
biloRoll <- rollCal(FFPorts$bilo_vwret, width)
bimeRoll <- rollCal(FFPorts$bime_vwret, width)
bihiRoll <- rollCal(FFPorts$bihi_vwret, width)
mktRoll <- rollCal(FFfactors$mktrf + FFfactors$rf, width)


colorSet <- rainbow(7)


plot(time, smloRoll$rollSharpe,ylab = "portfolio rolling Sharpe ratio(%)",type='l',col=colorSet[1])
lines(time, smmeRoll$rollSharpe,type='l',col=colorSet[2])
lines(time, smhiRoll$rollSharpe,type='l',col=colorSet[3])
lines(time, biloRoll$rollSharpe,type='l',col=colorSet[4])
lines(time, bimeRoll$rollSharpe,type='l',col=colorSet[5])
lines(time, bihiRoll$rollSharpe,type='l',col=colorSet[6])
lines(FFfactors$date[width:nrow(FFfactors)], mktRoll$rollSharpe,type='l',col=colorSet[7])
