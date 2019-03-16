rm(list = ls())
#setwd(utils::getSrcDirectory()[1])

# references: pricing of liquidity risks: evidence from multiple liquidity measure
#             pricing the commonality across alternative measures of liquidity

library(anytime)
library(lubridate)


dailyData <- read.csv("../Data/dailyReturn1962_2018NYSEAME_short_HL.csv")

permNoList <- unique(dailyData$PERMNO)

startDate <- anydate("1962-07-01")
endDate <- anydate("2018-08-01")


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
  names(ZROutput)[count + 1] = i
}

ZRMean <- rowMeans(ZROutput[,2:ncol(ZROutput)],na.rm = TRUE)

plot(ZROutput$time, ZRMean)
save(ZROutput,file="ZROutput.Rda")
write.csv(ZROutput, file="ZROutput.csv",row.names=FALSE)
