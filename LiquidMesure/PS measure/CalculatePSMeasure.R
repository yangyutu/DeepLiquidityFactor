rm(list = ls())
#setwd("E:\\Dropbox\\financeResearch\\WRDS\\LiquidMesure\\Amihud and turnover")

# references: pricing of liquidity risks: evidence from multiple liquidity measure
#             pricing the commonality across alternative measures of liquidity

library(anytime)
library(lubridate)

FamaFactor <- read.csv("../Data/FamaFrenchFourFactorDaily.csv")
FamaFactor$date <- anydate(FamaFactor$date)
mkt <- FamaFactor$mktrf + FamaFactor$rf

FamaFactor <- cbind(FamaFactor, mkt)
dailyData <- read.csv("../Data/dailyReturn1962_2018NYSEAME_short_HL.csv")
permNoList <- unique(dailyData$PERMNO)
dailyData$date <- anydate(dailyData$date)


startDate <- anydate("1962-07-01")
endDate <- anydate("2018-08-01")


time <- vector()

startIter <- startDate
while (startIter < endDate){
  time <- c(time, startIter)
  startIter <-startIter %m+% months(1)
}

PSOutput <- data.frame(anydate(time))
names(PSOutput)[0] <- "month"
count <- 0
for (i in permNoList){
  count <- count + 1
  print(count)
  stockData = dailyData[dailyData$PERMNO == i,]
  stockData$date <- anydate(stockData$date)
  PSmeasure <- vector()
  
  startIter <- startDate
  
  while (startIter < endDate){
    monthEnd <- startIter %m+% months(1)
    monthData <- stockData[anytime(stockData$date) >= startIter & anytime(stockData$date) <monthEnd,]
    nonZeroData <- monthData[monthData$VOL != 0 & monthData$PRC > 0,]
    rows <- nrow(nonZeroData)
    if( rows > 10){
      monthmkt <- FamaFactor[nonZeroData$date,"mkt"]
      dvol <- nonZeroData$PRC * nonZeroData$VOL
      Y <- nonZeroData$RET[2: rows] -  monthmkt[2:rows]
      X1 <- nonZeroData$RET[1:rows - 1]
      X2temp <- sign(nonZeroData$RET - monthmkt)*dvol
      X2 <- X2temp[1:rows - 1]
      df<-data.frame(y=Y,x1=X1,x2=X2)
      fit <- lm(y~.,df)
      gamma = -summary(fit)$coefficients[3]
    }
    else {
      gamma <- NA
    }
      PSmeasure <- c(PSmeasure, gamma)  
      startIter <- monthEnd
  }
  PSOutput <- cbind(PSOutput, PSmeasure)
  names(PSOutput)[count + 1] = i
 } 


save(PSOutput,file="PSOutput.Rda")
write.csv(PSOutput, file="PSOutput.csv",row.names=FALSE)