library(randomForest)

# make data
head_name <- read.table('header.txt', sep = ',', stringsAsFactors = F)
rawData <- read.table('dev100v2.txt', sep = ',')
ips = as.character(rawData[,1])
devData <- rawData[,-1]
names(devData) <- as.character(head_name)
sep <- c(20, 50, 75, 100)
findLabel <- function(x, sep) {
  ip <- as.integer(tail(unlist(strsplit(x, '[.]')), n=1))
  which(sep >= ip)[1]
}
cls <- sapply(ips, findLabel, sep)

devData$target <- as.factor(cls)

# rf
set.seed(1)
rf.dev <- randomForest(target~., data = devData, importance = T)
rf.dev
importance(rf.dev)
varImpPlot(rf.dev)
