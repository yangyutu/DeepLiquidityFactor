rm(list = ls())
setwd("E:\\Dropbox\\financeResearch\\WRDS\\LiquidMesure\\Amihud and turnover")

# references: pricing of liquidity risks: evidence from multiple liquidity measure
#             pricing the commonality across alternative measures of liquidity

library(anytime)
library(lubridate)

# define the Roll measure function
Roll <- function(inp){
  a <- acf(as.matrix(inp),plot=FALSE,lag=1,type="covariance")
  r <- 0
  Scov <- a$acf[2]
  if (Scov<0){ return (2*sqrt(-Scov)) }
  else { return (-sqrt(Scov)) }
}



dailyData <- read.csv("..\dailyReturn1983_2000NYSE_short.csv")

permNoList <- unlist(read.table("codeList.txt"))

startDate <- anydate("1983-01-01")
endDate <- anydate("2000-12-31")


time <- vector()

startIter <- startDate
while (startIter < endDate){
  time <- c(time, startIter)
  startIter <-startIter %m+% months(1)
}

AOutput <- data.frame(time)
names(AOutput)[0] <- "month"
TurnoverOutput <- data.frame(time)
names(TurnoverOutput)[0] <- "month"
RollOutput <- data.frame(time)
names(RollOutput)[0] <- "month"
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
    monthData <- stockData[anytime(stockData$date) >= startIter & anytime(stockData$date) <monthEnd,]
    nonZeroData <- monthData[monthData$VOL != 0,]
    if (nrow(nonZeroData) == 0) {
      A <- NA
      T <- NA ## turnOver
      R <- NA
    }else{
      A <- mean(abs(nonZeroData$RET)/nonZeroData$PRC/nonZeroData$VOL)*1e6
      T <- sum(nonZeroData$VOL)/mean(nonZeroData$SHROUT)
      R <- Roll(nonZeroData$RET)
    }
    Ameasure <- c(Ameasure, A)
    Tmeasure <- c(Tmeasure, T) 
    Rmeasure <- c(Rmeasure, R)  
    
    startIter <- monthEnd
  }
  AOutput <- cbind(AOutput, Ameasure)
  
  TurnoverOutput <- cbind(TurnoverOutput, Tmeasure)
  RollOutput <- cbind(RollOutput, Rmeasure)
  
  
}

plot(AOutput$time)


