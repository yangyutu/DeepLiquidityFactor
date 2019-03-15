rm(list = ls())
#setwd("E:\\Dropbox\\financeResearch\\WRDS\\LiquidMesure\\Amihud and turnover")

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

RollOutput <- data.frame(anydate(time))
names(RollOutput)[0] <- "month"
count <- 0
for (i in permNoList){
  count <- count + 1
  print(count)
  stockData = dailyData[dailyData$PERMNO == i,]

  Rmeasure <- vector()
  
  
  startIter <- startDate
  
  while (startIter < endDate){
    monthEnd <- startIter %m+% months(1)
    monthData <- stockData[anytime(stockData$date) >= startIter & anytime(stockData$date) <monthEnd,]
    nonZeroData <- monthData[monthData$VOL != 0,]
    monthData <- monthData[order(monthData$date),]
    # if less than 10 rows, cannot calculate Roll measure reliably
    if (nrow(nonZeroData) <= 10) {
      R <- NA
    }else{
      R <- Roll(nonZeroData$RET)
    }
    Rmeasure <- c(Rmeasure, R)  
    
    startIter <- monthEnd
  }
  RollOutput <- cbind(RollOutput, Rmeasure)
  
  
}

save(RollOutput,file="RollOutput.Rda")