source("dataFunctionsOMDeltaFixedT.R")
source("getPlotsOMFixedT.R")
# 1-Oct-21 S0 = 4357.04 close SPX
# 4-Oct-21 S0 = 4300.46 
# 5-Oct-21 S0 = 4345.72 

S0 <- 4300.46

# load all forward prices
allFwdData <- read.csv(file = 'forward_optionmetrics.csv')

names(allFwdData)
head(allFwdData)

asOfDay <- '2021-10-04'

filterFwdData <- allFwdData[ which(allFwdData$date==asOfDay),]

filterFwdData <- subset(filterFwdData, select=c(expiration,AMSettlement,ForwardPrice))
head(filterFwdData)
tail(filterFwdData)
length(filterFwdData$expiration)
#class(filterFwdData)

names(filterFwdData) <- c("T","AM","Fwd")
tail(filterFwdData)
n.exp <- length(filterFwdData)

FwdDataNoDupl <- subset(filterFwdData, !duplicated(filterFwdData$T))
length(FwdDataNoDupl$T)
tail(FwdDataNoDupl)

# keep only first row for any duplicated rows
#subset(df1, !duplicated(EVENTID))

finalFwdData <- filterFwdData[ which(filterFwdData$AM==0),]
head(finalFwdData)
tail(finalFwdData)

length(filterFwdData$AM)
length(finalFwdData$AM)

fwd.1 <- extract.fwd('2021-10-04')
head(fwd.1)
length(fwd.1$T)

######======================================================
# read in the forward prices for an asOf date
extract.fwd <- function(asOf = '2021-10-04'){

bulkData <- read.csv(file = 'forward_optionmetrics.csv')
asOfData <- bulkData[ which(bulkData$date==asOfDay),]
  
asOfData <- subset(asOfData, select=c(expiration,AMSettlement,ForwardPrice))

names(asOfData) <- c("T","AM","Fwd")
asOfData$T <- droplevels(asOfData$T)

FwdData <- subset(asOfData, !duplicated(asOfData$T))
#FwdData <- asOfData[ which(asOfData$AM==0),]

all.mat <- FwdData$T
all.fwd <- FwdData$Fwd

final.fwd <- data.frame("T"=all.mat,"Fwd"=all.fwd)

return(final.fwd)
  
}
#====================================================================

#maturities <- unique(filterFwdData$T)
df$v1 <- droplevels(df$v1)

filterFwdData$T <- droplevels(filterFwdData$T) #drop levels with zero counts
all.mat <- filterFwdData$T
length(all.mat)
class(all.mat)

summary(all.mat)

class(all.mat)
maturities <- levels(all.mat)
head(maturities)
tail(maturities)

maturities <- as.Date(maturities, "%Y-%m-%d")
maturities <- sort(maturities)
n.T <- length(maturities)
n.T
