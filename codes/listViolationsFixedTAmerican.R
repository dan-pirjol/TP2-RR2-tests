# find TP2/RR2 violations for two call/put strips
# lists violations
#---------------------------------------------------------------
# listTP2ViolationsAmer <- function(dataC1, dataC2, F1, F2)
# returns list.breaks("K1", "K2", "det", "del1", "del2")

#################################################################
testTP2CallsAmer <- function(dataC1, dataC2, F1, F2, showPlot, Kmin, Kmax){
# do not include fwd-adjustment  
dataC12 <- merge(dataC1, dataC2, by="K")
  
##########################################################
# for all k1 < k2 compute
# det = C1(k1) * C2(k2) - C1(k2)* C2(k1)
# construct a matrix of tcvx[i,j] = C1[i]*C2[j] - C1[j]*C2[i]
# K Cbid.x Cofr.x  Cmid.x Cbid.y Cofr.y  Cmid.y
n.12 <- length(dataC12$K)
#print(n.12)
  
if (n.12==0) {
  counts <- data.frame("n.q"=1, "n.b"=0)
  return(counts)
}
  
n.b <- 0
n.q <- 0
  
vK1 <- c()
vK2 <- c()
all.test <- c()
# violations taking into account bid-ask spread
vL1 <- c()
vL2 <- c()
  
for (i in 1:n.12){
  for (j in 1:n.12){
#    if (dataC12$K[i]/F1 < dataC12$K[j]/F2 && dataC12$K[i]/F1 >= 1)  {
#    if (dataC12$K[i]/F1 < dataC12$K[j]/F2 )  {
    if (dataC12$K[i] < dataC12$K[j] )  {
        n.q <- n.q +1

# TP2 test    
test <- dataC12$Cmid.x[i]*dataC12$Cmid.y[j] - dataC12$Cmid.x[j]*dataC12$Cmid.y[i]
testBA <- dataC12$Cask.x[i]*dataC12$Cask.y[j] - dataC12$Cbid.x[j]*dataC12$Cbid.y[i]

#    if (test < 0 && dataC12$K[i]/F1 < dataC12$K[j]/F2 && dataC12$K[i]/F1 >= 1) {
  if (test < 0) {
    vK1 <- c(vK1,dataC12$K[i]/F1)
    vK2 <- c(vK2,dataC12$K[j]/F2)
    all.test <- c(all.test, test)
    n.b <- n.b + 1
  }
if (testBA < 0) {
  vL1 <- c(vL1,dataC12$K[i]/F1)
  vL2 <- c(vL2,dataC12$K[j]/F2)
}
}
}
}
  
list.breaks <- data.frame("K1"=vK1, "K2"=vK2, "det"=all.test)
  
#if (showPlot ==  1){
#  plot(vK1,vK2, col="blue", type="p", pch=20, 
#        xlim=c(Kmin,Kmax), ylim=c(Kmin,Kmax),
#        main="Calls", xlab="K1/S0", ylab="K2/S0")
#  lines(vL1,vL2, col="yellow", type="p", pch=20)
#  abline(a=0,b=1,col="red")
#  abline(h=1, col="red")
#  abline(v=1, col="red")
#}
  
counts <- data.frame("n.q"=n.q, "n.b"=n.b)
#print(n.q)
#print(n.b)
#return(list.breaks)
return(counts)
  
  
}
#---------------------------------------------------------------------------
###########################################################################
testRR2PutsAmer <- function(dataP1, dataP2, F1, F2, showPlot, Kmin, Kmax){
# test RR2 without fwd-adjustment  
dataP12 <- merge(dataP1, dataP2, by="K")
  
##########################################################
# for all k1 < k2 compute
# det = P1(k1) * P2(k2) - P1(k2*F1/F2) * P2(k1*F2/F1)
# construct a matrix of tcvx[i,j] = C1[i]*C2[j] - C1[j]*C2[i]
# K Cbid.x Cofr.x  Cmid.x Cbid.y Cofr.y  Cmid.y
n.12 <- length(dataP12$K)
  
if (n.12==0) {
  counts <- data.frame("n.q"=1, "n.b"=0)
  return(counts)
}
  
n.b <- 0
n.q <- 0
  
vK1 <- c()
vK2 <- c()
# violations taking into account bid-ask spread
vL1 <- c()
vL2 <- c()
  
for (i in 1:n.12){
  for (j in 1:n.12){
#    if (dataC12$K[i]/F1 < dataC12$K[j]/F2 && dataC12$K[i]/F1 >= 1)  {
#    if (dataP12$K[i]/F1 < dataP12$K[j]/F2 )  {
      if (dataP12$K[i] < dataP12$K[j] )  {
      n.q <- n.q +1

        
# RR2 test    
test <- dataP12$Pmid.x[i]*dataP12$Pmid.y[j] - dataP12$Pmid.x[j]*dataP12$Pmid.y[i]
testBA <- dataP12$Pbid.x[i]*dataP12$Pbid.y[j] - dataP12$Pask.x[j]*dataP12$Pask.y[i]

      if (test > 0) {
        vK1 <- c(vK1,dataP12$K[i]/F1)
        vK2 <- c(vK2,dataP12$K[j]/F2)
        n.b <- n.b + 1
      }
if (testBA > 0) {
  vL1 <- c(vL1,dataP12$K[i]/F1)
  vL2 <- c(vL2,dataP12$K[j]/F2)
}
    }
  }
}
  
#if (showPlot ==  1){
#  plot(vK1,vK2, col="blue", pch=20, 
#        xlim=c(Kmin,Kmax), ylim=c(Kmin,Kmax),
#        main="Puts: (T1 < T2)", xlab="K1/S0", ylab="K2/S0")
#  lines(vL1,vL2, col="yellow", type="p", pch=20) 
#  abline(a=0,b=1,col="red")
#  abline(h=1, col="red")
#  abline(v=1, col="red")
#}
  
counts <- data.frame("n.q"=n.q, "n.b"=n.b)
  
return(counts)
  
  
}
#----------------------------------------------------
listTP2ViolationsAmer <- function(dataC1, dataC2, F1, F2){
# no Fwd-adjustment
  
#  dataC1 <- dataCall(dataOneDay,datesExpiry[2])
#  dataC2 <- dataCall(dataOneDay,datesExpiry[3])
dataC12 <- merge(dataC1, dataC2, by="K")
#head(dataC12)  
##########################################################
# for all k1 < k2 compute
# det = C1(k1) * C2(k2) - C1(k2)* C2(k1)
# construct a matrix of tcvx[i,j] = C1[i]*C2[j] - C1[j]*C2[i]
# K Cbid.x Cofr.x  Cmid.x Cbid.y Cofr.y  Cmid.y
n.12 <- length(dataC12$K)
  
if (n.12==0) {
#  counts <- data.frame("n.q"=1, "n.b"=0)
  list.breaks <- data.frame("K1"=0, "K2"=0, "det"=0, "del1"=0, "del2"=0)
  return(list.breaks)
}
  
n.b <- 0
n.q <- 0
  
vK1 <- c()
vK2 <- c()
deltaC1 <- c()
deltaC2 <- c()
all.test <- c()
  
for (i in 1:n.12){
  for (j in 1:n.12){
    #    if (dataC12$K[i]/F1 < dataC12$K[j]/F2 && dataC12$K[i]/F1 >= 1)  {
    if (dataC12$K[i] < dataC12$K[j] )  {
      n.q <- n.q +1
#      x1 <- dataC12$K[i]*F2/F1 # this is K1*F2/F1
#      x2 <- dataC12$K[j]*F1/F2 # this is K2*F1/F2
#      K1.ind <- CKtilde(dataC12, x1)
#      K2.ind <- CKtilde(dataC12, x2)
        
#      ind1 <- K1.ind$ind
#      ind2 <- K2.ind$ind
        
  # TP2 test    
    test <- dataC12$Cmid.x[i]*dataC12$Cmid.y[j] - dataC12$Cmid.x[j]*dataC12$Cmid.y[i]
        
    if (test < 0) {
      vK1 <- c(vK1,dataC12$K[i])
      vK2 <- c(vK2,dataC12$K[j])
      deltaC1 <- c(deltaC1, dataC12$delta.x[i])
      deltaC2 <- c(deltaC2, dataC12$delta.y[j])
      all.test <- c(all.test, test)
      n.b <- n.b + 1
      }
    }
  }
}
  
list.breaks <- data.frame("K1"=vK1, "K2"=vK2, "det"=all.test, "del1"=deltaC1, "del2"=deltaC2)
  
counts <- data.frame("n.q"=n.q, "n.b"=n.b)
#print(n.q)
#print(n.b)
return(list.breaks)
  
  
}
#-------------------------------------------------------------
###########################################################################
listRR2ViolationsAmer <- function(dataP1, dataP2, F1, F2){
# list all RR2 violations
dataP12 <- merge(dataP1, dataP2, by="K")
  
##########################################################
# for all k1 < k2 compute
# det = P1(k1) * P2(k2) - P1(k2) * P2(k1)
# K Pbid.x Pofr.x  Pmid.x Pbid.y Pofr.y  Pmid.y
n.12 <- length(dataP12$K)
  
if (n.12==0) {
  list.breaks <- data.frame("K1"=0, "K2"=0, "det"=0, "del1"=0, "del2"=0)
  return(list.breaks)
}
  
n.b <- 0
n.q <- 0
all.test <- c()
  
vK1 <- c()
vK2 <- c()
deltaP1 <- c()
deltaP2 <- c()
# violations taking into account bid-ask spread
#vL1 <- c()
#vL2 <- c()
  
for (i in 1:n.12){
  for (j in 1:n.12){
    if (dataP12$K[i] < dataP12$K[j] )  {
    n.q <- n.q +1
        
  # RR2 test    
  test <- dataP12$Pmid.x[i]*dataP12$Pmid.y[j] - dataP12$Pmid.x[j]*dataP12$Pmid.y[i]

   if (test > 0) {
    vK1 <- c(vK1,dataP12$K[i]/F1)
    vK2 <- c(vK2,dataP12$K[j]/F2)
    deltaP1 <- c(deltaP1, dataP12$delta.x[i])
    deltaP2 <- c(deltaP2, dataP12$delta.y[j])
    all.test <- c(all.test, test)
    n.b <- n.b + 1
    }
  }
}
}
  

list.breaks <- data.frame("K1"=vK1, "K2"=vK2, "det"=all.test, "del1"=deltaP1, "del2"=deltaP2)
  
counts <- data.frame("n.q"=n.q, "n.b"=n.b)
return(list.breaks)
#return(counts)
  
  
}
#################################################################
testTP2CallsAmerBidAsk <- function(dataC1, dataC2, F1, F2, showPlot, Kmin, Kmax){
# do not include fwd-adjustment  
dataC12 <- merge(dataC1, dataC2, by="K")
  
##########################################################
# for all k1 < k2 compute
# det = C1(k1) * C2(k2) - C1(k2)* C2(k1)
# construct a matrix of tcvx[i,j] = C1[i]*C2[j] - C1[j]*C2[i]
# K Cbid.x Cofr.x  Cmid.x Cbid.y Cofr.y  Cmid.y
n.12 <- length(dataC12$K)
print(n.12)
  
if (n.12==0) {
  counts <- data.frame("n.q"=1, "n.b"=0)
  return(counts)
}
  
n.b <- 0
n.q <- 0
  
vK1 <- c()
vK2 <- c()
all.test <- c()
  
for (i in 1:n.12){
  for (j in 1:n.12){
    if (dataC12$K[i] < dataC12$K[j] )  {
      n.q <- n.q +1
        
# TP2 test    
test <- dataC12$Cask.x[i]*dataC12$Cask.y[j] - dataC12$Cbid.x[j]*dataC12$Cbid.y[i]
   
#    if (test < 0 && dataC12$K[i]/F1 < dataC12$K[j]/F2 && dataC12$K[i]/F1 >= 1) {
        if (test < 0) {
          vK1 <- c(vK1,dataC12$K[i]/F1)
          vK2 <- c(vK2,dataC12$K[j]/F2)
          all.test <- c(all.test, test)
          n.b <- n.b + 1
        }
      }
    }
  }
  
list.breaks <- data.frame("K1"=vK1, "K2"=vK2, "det"=all.test)
  
if (showPlot ==  1){
  plot(vK1,vK2, col="blue", pch=20, 
       xlim=c(Kmin,Kmax), ylim=c(Kmin,Kmax),
       main="Calls: (T1, T2)", xlab="K(i)/F(T1)", ylab="K(j)/F(T2)")
  abline(a=0,b=1,col="red")
  abline(h=1, col="red")
  abline(v=1, col="red")
  }
  
  counts <- data.frame("n.q"=n.q, "n.b"=n.b)
  print(n.q)
  print(n.b)
  #return(list.breaks)
  return(counts)
  
  
}
#---------------------------------------------------------------------------
###########################################################################
testRR2PutsAmerBidAsk <- function(dataP1, dataP2, F1, F2, showPlot, Kmin, Kmax){
# test RR2 without fwd-adjustment including the bid-ask spread 
dataP12 <- merge(dataP1, dataP2, by="K")
  
##########################################################
# for all k1 < k2 compute
# det = P1(k1) * P2(k2) - P1(k2*F1/F2) * P2(k1*F2/F1)
# construct a matrix of tcvx[i,j] = C1[i]*C2[j] - C1[j]*C2[i]
# K Cbid.x Cofr.x  Cmid.x Cbid.y Cofr.y  Cmid.y
n.12 <- length(dataP12$K)
  
if (n.12==0) {
  counts <- data.frame("n.q"=1, "n.b"=0)
  return(counts)
}
  
  n.b <- 0
  n.q <- 0
  
  vK1 <- c()
  vK2 <- c()
  
  for (i in 1:n.12){
    for (j in 1:n.12){
      #    if (dataC12$K[i]/F1 < dataC12$K[j]/F2 && dataC12$K[i]/F1 >= 1)  {
      #    if (dataP12$K[i]/F1 < dataP12$K[j]/F2 )  {
      if (dataP12$K[i] < dataP12$K[j] )  {
        n.q <- n.q +1
        #      x1 <- dataP12$K[i] #*F2/F1 # this is K1*F2/F1
        #      x2 <- dataP12$K[j] #*F1/F2 # this is K2*F1/F2
        #      K1.ind <- CKtilde(dataP12, x1)
        #      K2.ind <- CKtilde(dataP12, x2)
        
        #      ind1 <- K1.ind$ind
        #      ind2 <- K2.ind$ind
        
# RR2 test    
test <- dataP12$Pbid.x[i]*dataP12$Pbid.y[j] - dataP12$Pask.x[j]*dataP12$Pask.y[i]
        
        if (test > 0) {
          vK1 <- c(vK1,dataP12$K[i]/F1)
          vK2 <- c(vK2,dataP12$K[j]/F2)
          n.b <- n.b + 1
        }
      }
    }
  }
  
  if (showPlot ==  1){
    plot(vK1,vK2, col="blue", pch=20, 
         xlim=c(Kmin,Kmax), ylim=c(Kmin,Kmax),
         main="Puts: (T1 < T2)", xlab="K1/F(T1)", ylab="K2/F(T2)")
    abline(a=0,b=1,col="red")
    abline(h=1, col="red")
    abline(v=1, col="red")
  }
  
  counts <- data.frame("n.q"=n.q, "n.b"=n.b)
  
  return(counts)
  
  
}