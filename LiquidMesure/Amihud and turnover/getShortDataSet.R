rm(list = ls())

setwd("E:\\Dropbox\\financeResearch\\WRDS\\CRSPDailyReturn")
dailyData <- read.csv("dailyReturn1983_2000NYSE.csv")

shortData <- dailyData[,c("PERMNO","date","TICKER","PRC","VOL","RET","SHROUT")]
shortData$RET <- as.numeric(as.character(shortData$RET))
print(sum(is.na(shortData)))
shortData <- na.omit(shortData)
print(sum(is.na(shortData)))
write.csv(shortData, "dailyReturn1983_2000NYSE_short.csv")
