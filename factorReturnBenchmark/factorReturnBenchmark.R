rm(list = ls())
library(anytime)
library(Hmisc)
setwd("E:\\Dropbox\\financeResearch\\factorReturnBenchmark")

FFfactors <- read.csv("FamaFrenchMonthlyData.csv")
FFfactors$dateff<- anydate(FFfactors$dateff)
names(FFfactors)[1] <- "date"


png("FFFactorReturnHist1926_2018.png", width = 600, height = 600)
par(mfrow = c(2,2))
hist(FFfactors$mktrf*100,20,xlab = "excess market monthly return(%)")
hist(FFfactors$smb*100,20,xlab = "smb monthly return(%)")
hist(FFfactors$hml*100,20,xlab = "hml monthly return(%)")
hist(FFfactors$umd*100,20,xlab = "umd monthly return(%)")

dev.off()

png("FFFactorReturnTime1926_2018.png", width = 600, height = 600)
par(mfrow = c(2,2))
plot(FFfactors$date, FFfactors$mktrf*100,ylab = "excess market monthly return(%)")
plot(FFfactors$date, FFfactors$smb*100,ylab = "smb monthly return(%)")
plot(FFfactors$date, FFfactors$hml*100,ylab = "hml monthly return(%)")
plot(FFfactors$date, FFfactors$umd*100,ylab = "umd monthly return(%)")
dev.off()

png("FFFactorCumReturnTime1926_2018.png", width = 600, height = 600)
par(mfrow = c(1,1))
mktrf_cum <- cumprod(FFfactors$mktrf+1) - 1
smb_cum <- cumprod(FFfactors$smb+1) - 1
hml_cum <- cumprod(FFfactors$hml+1) - 1
umd_cum <- cumprod(FFfactors$umd[7:length(FFfactors$umd)]+1) - 1

plot(FFfactors$date, mktrf_cum*100,ylab = "factor cumulative monthly return(%)",type='l',ylim = c(0, 60000))
lines(FFfactors$date, smb_cum*100,type='l',col = 'red')
lines(FFfactors$date, hml_cum*100,,type='l',col = 'green')
lines(FFfactors$date[7:length(FFfactors$umd)], umd_cum*100,,type='l',col = 'blue')
legend(1, 50000, legend=c("market excess", "smb","hmb","umd"),lty=1,col = c("black","red","green","blue"))
dev.off()


# now calculate the rolling mean and standard deviation of returns
library(zoo)

prodMean <- function(x){
  prod <- prod(1+x)
  n <- length(x)
  res <- prod^(1/n) - 1
  return(res)
}
width <- 60
mktRollMean <- rollapply(FFfactors$mktrf+FFfactors$rf, width = width, FUN=prodMean, align = "right")
mktRollSD <- rollapply(FFfactors$mktrf+FFfactors$rf, width = width, FUN=sd, align = "right")
mktRollSharpe <- mktRollMean/mktRollSD
time <- FFfactors$date[width:length(FFfactors$date)]
plot(time, mktRollMean)
plot(time, mktRollSharpe)
