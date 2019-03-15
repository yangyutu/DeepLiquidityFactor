rm(list = ls())
#setwd("E:\\Dropbox\\financeResearch\\WRDS\\LiquidMesure\\Amihud and turnover")

# references: pricing of liquidity risks: evidence from multiple liquidity measure
#             pricing the commonality across alternative measures of liquidity

library(anytime)
library(lubridate)

FamaFactor <- read.csv("../FamaFrenchFourFactorDaily.csv")
FamaFactor$date <- anydate(FamaFactor$date)
mkt <- FamaFactor$mktrf + FamaFactor$rf
FamaFactor <- cbind(FamaFactor, mkt)
dailyData <- read.csv("../dailyReturn1983_2000NYSE_short.csv")
dailyData$date <- anydate(dailyData$date)
permNoList <- unlist(read.table("../codeList.txt"))

startDate <- anydate("1983-01-01")
endDate <- anydate("2000-12-31")


time <- vector()

startIter <- startDate
while (startIter < endDate){
  time <- c(time, startIter)
  startIter <-startIter %m+% months(1)
}

PSOutput <- data.frame(anydate(time))
names(PSOutput)[0] <- "month"
count <- 0
for (i in permNoList[1]){
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
  
 } 


save(PSOutput,file="PSOutput.Rda")