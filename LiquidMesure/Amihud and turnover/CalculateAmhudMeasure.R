rm(list = ls())


# references: pricing of liquidity risks: evidence from multiple liquidity measure
#             pricing the commonality across alternative measures of liquidity

library(anytime)
library(lubridate)


#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)
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

AOutput <- data.frame(anydate(time))
names(AOutput)[0] <- "month"
TurnoverOutput <- data.frame(anytime(time))
names(TurnoverOutput)[0] <- "month"

count <- 0
for (i in permNoList){
  count <- count + 1
  print(count)
  stockData = dailyData[dailyData$PERMNO == i,]
  
  Ameasure <- vector()
  Tmeasure <- vector()
  Rmeasure <- vector()
  
  
  startIter <- startDate
  
  while (startIter < endDate){
    monthEnd <- startIter %m+% months(1)
    monthData <- stockData[anydate(stockData$date) >= startIter & anydate(stockData$date) <monthEnd,]
    nonZeroData <- monthData[monthData$VOL != 0,]
    if (nrow(nonZeroData) == 0) {
      A <- NA
      T <- NA ## turnOver
#      R <- NA
    }else{
      A <- mean(abs(nonZeroData$RET)/nonZeroData$PRC/nonZeroData$VOL)*1e6
      T <- sum(nonZeroData$VOL)/mean(nonZeroData$SHROUT)
#      R <- Roll(nonZeroData$RET)
    }
    Ameasure <- c(Ameasure, A)
    Tmeasure <- c(Tmeasure, T) 
#    Rmeasure <- c(Rmeasure, R)  
    
    
    
    
    
    startIter <- monthEnd
  }
  AOutput <- cbind(AOutput, Ameasure)
  TurnoverOutput <- cbind(TurnoverOutput, Tmeasure)
#  RollOutput <- cbind(RollOutput, Rmeasure)
  names(AOutput)[count + 1] = i
  names(TurnoverOutput)[count + 1] = i
#  names(RollOutput)[count + 1] = i
  
}

#plot(AOutput$time)
write.csv(AOutput, file="AOutput.csv",row.names=FALSE)
write.csv(TurnoverOutput, file="TurnoverOutput.csv",row.names=FALSE)
#write.csv(RollOutput, file="RollOutput.csv",row.names=FALSE)
