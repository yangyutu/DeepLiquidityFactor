rm(list = ls())

# references: pricing of liquidity risks: evidence from multiple liquidity measure
#             pricing the commonality across alternative measures of liquidity

library(anytime)
library(lubridate)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
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

CSOutput <- data.frame(anydate(time))
names(CSOutput)[1] <- "month"
count <- 0

for (i in permNoList){
  count <- count + 1
  print(count)
  stockData = dailyData[dailyData$PERMNO == i,]
  stockData$date <- anydate(stockData$date)
  
  CSmeasure <- vector()
  startIter <- startDate
  
  while (startIter < endDate){
    
    monthEnd <- startIter %m+% months(1)
    monthData <- stockData[anytime(stockData$date) >= startIter & anytime(stockData$date) <monthEnd,]
    monthData <- monthData[order(monthData$date),]
    # day iter 
    monthlySSet <- vector()
    idx <- 2
    while(idx < length(monthData$date)){
      
      ratio <- (log(monthData$ASKHI[idx-1]/monthData$BIDLO[idx-1]))^2
      beta <- ratio + (log(monthData$ASKHI[idx]/monthData$BIDLO[idx]))^2
    
      twoDayHI = max(monthData$ASKHI[(idx-1):idx])
      twoDayLO = max(monthData$BIDLO[(idx-1):idx])
      gamma <- (log(twoDayHI/twoDayLO))^2
    
      denom <- 3 - 2*sqrt(2)
      alpha <- (sqrt(2*beta) - sqrt(beta))/denom - sqrt(gamma/denom)
      
      if (!is.na(alpha)){
        Stemp <- 2*(exp(alpha) - 1)/(1 + exp(alpha))
        monthlySSet <- c(monthlySSet, Stemp)
      }
      idx <- idx + 1
    }
    S <- mean(monthlySSet)
    
    CSmeasure <- c(CSmeasure, S)
    
    startIter <- monthEnd
  }
  CSOutput <- cbind(CSOutput, CSmeasure)
  names(CSOutput)[count + 1] = i
}

#ZRMean <- rowMeans(ZROutput[,2:ncol(ZROutput)],na.rm = TRUE)

#plot(ZROutput$time, ZRMean)
save(CSOutput,file="CSOutput.Rda")
write.csv(CSOutput, file="CSOutput.csv",row.names=FALSE)
