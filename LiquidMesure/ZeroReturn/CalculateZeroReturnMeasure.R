rm(list = ls())
setwd(utils::getSrcDirectory()[1])

# references: pricing of liquidity risks: evidence from multiple liquidity measure
#             pricing the commonality across alternative measures of liquidity

library(anytime)
library(lubridate)


dailyData <- read.csv("../dailyReturn1983_2000NYSE_short.csv")

permNoList <- unlist(read.table("../codeList.txt"))

startDate <- anydate("1983-01-01")
endDate <- anydate("2000-12-31")


time <- vector()

startIter <- startDate
while (startIter < endDate){
  time <- c(time, startIter)
  startIter <-startIter %m+% months(1)
}

ZROutput <- data.frame(time)
names(ZROutput)[0] <- "month"
count <- 0
for (i in permNoList){
  count <- count + 1
  print(count)
  stockData = dailyData[dailyData$PERMNO == i,]
  
  ZRmeasure <- vector()
  startIter <- startDate

  while (startIter < endDate){

    monthEnd <- startIter %m+% months(1)
    monthData <- stockData[anytime(stockData$date) >= startIter & anytime(stockData$date) <monthEnd,]
    tradingDay <- nrow(monthData)
    

    if (tradingDay != 0) {
      ZRday = length(monthData$RET[monthData$RET == 0])
      ZR <- ZRday/tradingDay
      ZRmeasure <- c(ZRmeasure, ZR)
    } else{
      ZRmeasure <- c(ZRmeasure, NA)
    }
    
    startIter <- monthEnd
  }
  ZROutput <- cbind(ZROutput, ZRmeasure)
  
}

ZRMean <- rowMeans(ZROutput[,2:ncol(ZROutput)],na.rm = TRUE)

plot(ZROutput$time, ZRMean)
save(ZROutput,file="ZROutput.Rda")

