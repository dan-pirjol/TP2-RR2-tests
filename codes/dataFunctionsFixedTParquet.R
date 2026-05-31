#-----  extract call data ------------------------------------
dataCall <- function(bulkData, Tdate = '2026-02-06'){
  
#xC <- bulkData[ which(bulkData$cp_flag=='C' 
#                      & as.Date(bulkData$exdate, "%m/%d/%Y")==Tdate),]
xC <- bulkData[ which(bulkData$cp_flag=='C' 
                      & as.Date(bulkData$exdate)==Tdate),]

dataC <- subset(xC, select=c(strike_price,best_bid,best_offer,volume, impl_volatility, delta))

dataC$mid <- 0.5*(dataC$best_bid + dataC$best_offer)

names(dataC) <- c("K","Cbid","Cask","volume","IV","delta","Cmid")

dataC$K <- dataC$K/1000

dataC <- subset(dataC, dataC$Cbid != 0)
dataC <- subset(dataC, dataC$volume > 0)
dataC <- subset(dataC, dataC$delta <= 0.99)
dataC <- subset(dataC, dataC$delta >= 0.01)

dataC <- dataC[order(dataC$K, decreasing = FALSE),]

return(dataC)

}
#=======================================================
# ---------  extract put data  -------------------------
dataPut <- function(bulkData, Tdate = '2026-02-06'){

xP <- bulkData[ which(bulkData$cp_flag=='P' 
                    & as.Date(bulkData$exdate)==Tdate),]
  
dataP <- subset(xP, select=c(strike_price,best_bid,best_offer,volume, impl_volatility, delta))

dataP$mid <- 0.5*(dataP$best_bid + dataP$best_offer)
  
names(dataP) <- c("K","Pbid","Pask","volume","IV","delta","Pmid")

dataP$K <- dataP$K/1000

dataP <- subset(dataP, dataP$Pbid != 0)
dataP <- subset(dataP, dataP$volume > 0)
dataP <- subset(dataP, dataP$delta >= -0.99)
dataP <- subset(dataP, dataP$delta <= -0.01)

dataP <- dataP[order(dataP$K, decreasing = FALSE),]
  
return(dataP)
  
}
#------------------------------------------------------------
# return the forward, as the strike where Call = Put 
getForward <- function(dataC, dataP){
  
  x <- merge(dataC, dataP, by="K")
  
  diff <- x$Cmid - x$Pmid
  
  n <- length(x$K)
  if (n<=1) {
    print('empty overlap of calls and puts')
    return(690.62)
  }
  
  ind <- 1
  for (i in 1:(n-1)) {
    if (diff[i]*diff[i+1] < 0) {
      ind <- i
    }
  }
#  ind
  
a <- diff[ind]
b <- -diff[ind+1]

fwd <- x$K[ind] + (x$K[ind+1] - x$K[ind])*a/(a+b)

  return(fwd)
}
#################################################
getForwardIndex <- function(dataC, dataP){

  x <- merge(dataC, dataP, by="K")
  
  diff <- x$Cmid - x$Pmid
  
  n <- length(x$Cmid)
  if (n<=1) {
    print('empty overlap of calls and puts')
    return(0.0)
  }
  
  ind <- 1
  for (i in 1:(n-1)) {
    if (diff[i]*diff[i+1] < 0) {
      ind <- i
    }
  }
  #  ind
  
#  a <- diff[ind]
#  b <- -diff[ind+1]
  
#  fwd <- dataC$K[ind] + (dataC$K[ind+1] - dataC$K[ind])*a/(a+b)
  
  return(ind)
}
#============ get ATM Spread =================================
# return the ATM spread
getSpread <- function(dataC, dataP){
  
  diff <- dataC$Cmid - dataP$Pmid
  
  n <- length(dataC$Cmid)
  
  ind <- 1
  for (i in 1:(n-1)) {
    if (diff[i]*diff[i+1] < 0) {
      ind <- i
    }
  }

#  a <- diff[ind]
#  b <- -diff[ind+1]
  
#  fwd <- dataC$K[ind] + (dataC$K[ind+1] - dataC$K[ind])*a/(a+b)
  ATMspC <- (dataC$Cask[ind]-dataC$Cbid[ind]) #/dataC$Cmid[ind]
  ATMspP <- (dataP$Pask[ind]-dataP$Pbid[ind]) #/dataP$Pmid[ind]
  ATMsp = c(ATMspC, ATMspP,dataC$K[ind], dataP$K[ind])
    
  return(ATMsp)
}
#=====================================================

#################################################################
# read in the forward prices for an asOf date
extract.fwd <- function(asOf = '2021-10-04'){
  
  bulkData <- read.csv(file = 'forward_optionmetrics.csv')
  asOfData <- bulkData[ which(bulkData$date==asOf),]
  
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
getTradedVolumes <- function(dataC, dataP){
  
  
  n.C <- length(dataC$K)
  n.P <- length(dataP$K)
  
  C.vol <- 0
  P.vol <- 0
  
  for (i in 1:n.C) C.vol <- C.vol + dataC$volume[i]
  
  for (i in 1:n.P) P.vol <- P.vol + dataP$volume[i]
  
  #print(C.vol)
  #print(P.vol)
  
  vol.total <- data.frame("volC"=C.vol,"volP"=P.vol)
  
  return(vol.total)
}
#################################################
