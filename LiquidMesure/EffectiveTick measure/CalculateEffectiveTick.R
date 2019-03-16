rm(list = ls())
#setwd("E:\\Dropbox\\financeResearch\\WRDS\\LiquidMesure\\Amihud and turnover")

# references: pricing of liquidity risks: evidence from multiple liquidity measure
#             pricing the commonality across alternative measures of liquidity

library(anytime)
library(lubridate)

tickOne <- c(0.125, 0.25, 0.5, 1) # before July 1997
tickTwo <- c(0.0625, 0.125, 0.25, 0.5, 1) # between July 1997 and January 2001
tickThree <- c(0.01, 0.05, 0.1, 0.25, 1) # after January 2001

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

ETOutput <- data.frame(anydate(time))
names(ETOutput)[0] <- "month"
count <- 0
for (i in permNoList){
  count <- count + 1
  print(count)
  stockData = dailyData[dailyData$PERMNO == i,]
  
  ETmeasure <- vector()
  
  startIter <- startDate
  
  while (startIter < endDate){
    monthEnd <- startIter %m+% months(1)
    monthData <- stockData[anytime(stockData$date) >= startIter & anytime(stockData$date) <monthEnd,]
    nonZeroData <- monthData[monthData$VOL != 0 & monthData$PRC > 0,]
    avgPrice <- mean(nonZeroData$PRC)
    
    if(nrow(nonZeroData) > 10){
      
    if (startIter < anydate("1997-07")){
      tick <- tickOne
    } else if (startIter < anydate("2001-01")){
      tick <- tickTwo
    } else {
      tick <- tickThree
    }
    tickSize <- length(tick)
    Nset <- vector()
    for(j in c(1:tickSize)){
      N <- sum((nonZeroData$PRC %% tick[j]) == 0)
      Nset <- c(Nset, N)
    }
    if(sum(Nset) == 0){
      stop("sum of Nset is zero")
      
    }
    F<- Nset/sum(Nset)
    
    U <- vector(mode = "double",length =  tickSize)
    U[1] <- 2*F[1]
    for(j in c(2:(tickSize - 1))){
      U[j] <- (2*F[j] - F[j-1])
    }
    U[tickSize] <- F[tickSize] - F[tickSize - 1]
    
    gamma <- vector(mode = "double",length =  tickSize)
    
    gamma[1] <- min(max(U[1],0),1)
    gammacum = gamma[1]
    for(j in c(2:tickSize)){
      gamma[j] <- min(max(U[1],0),1 - gammacum)
      gammacum <- gammacum + gamma[j]
    }
      nom <- gamma %*% tick 
      ET <- nom / avgPrice
    }
    else {
      ET <- NA
    }
      ETmeasure <- c(ETmeasure, ET)  
      startIter <- monthEnd
  }
  ETOutput <- cbind(ETOutput, ETmeasure)
  names(ETOutput)[count + 1] = i
 } 


save(ETOutput,file="ETOutput.Rda")
write.csv(ETOutput, file="ETOutput.csv",row.names=FALSE)