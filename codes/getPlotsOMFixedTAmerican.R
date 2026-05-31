# plot TP2 violations for two call strips
# lists violations
#---------------------------------------------------------------

testTP2Calls <- function(dataC1, dataC2, S0, Kmin=4100, Kmax=4600){
  #--- build the overlap of T1, T2
  x <- merge(dataC1, dataC2, by="K")
  
  ##########################################################
  # construct a matrix of tcvx[i,j] = C1[i]*C2[j] - C1[j]*C2[i]
  # K Cbid.x Cofr.x  Cmid.x Cbid.y Cofr.y  Cmid.y
  n.12 <- length(x$K)
  
  vK1 <- c()
  vK2 <- c()
  
  for (i in 1:n.12){
    for (j in 1:n.12){
      test <- x$Cmid.x[i]*x$Cmid.y[j] - x$Cmid.x[j]*x$Cmid.y[i]
      if (test < 0 && x$K[i] < x$K[j] && x$K[i] > S0) {
        vK1 <- c(vK1,x$K[i])
        vK2 <- c(vK2,x$K[j])
      }
    }
  }
  
  # same using ask*ask - bid*bid
  yK1 <- c()
  yK2 <- c()
  
  for (i in 1:n.12){
    for (j in 1:n.12){
      test <- x$Cask.x[i]*x$Cask.y[j] - x$Cbid.x[j]*x$Cbid.y[i]
      if (test < 0 && x$K[i] < x$K[j] && x$K[i] > S0) {
        yK1 <- c(yK1,x$K[i]/S0)
        yK2 <- c(yK2,x$K[j]/S0)
      }
    }
  }
  
  
  plot(vK1,vK2, col="blue", pch=20, 
       xlim=c(Kmin,Kmax), ylim=c(Kmin,Kmax),
       #     xlim = c(4100,5000), ylim=c(4100,5000),
       main="Calls: SPX T1, T2", xlab="K(i)", ylab="K(j)")
  lines(yK1,yK2, col="yellow", type="p",pch=20)
  abline(a=0,b=1,col="red")
#  abline(h=S0, col="red")
#  abline(v=S0, col="red")
  
  return()
}

#################################################################
testTP2Puts <- function(dataP1, dataP2, S0, Kmin=4100, Kmax=4600){
#--- build the overlap of T1, T2
xP <- merge(dataP1, dataP2, by="K")
  
##########################################################
# construct a matrix of tcvx[i,j] = P1[i]*P2[j] - P1[j]*P2[i]
nP.12 <- length(xP$K)

vPK1 <- c()
vPK2 <- c()
  
for (i in 1:nP.12){
  for (j in 1:nP.12){
    test <- xP$Pmid.x[i]*xP$Pmid.y[j] - xP$Pmid.x[j]*xP$Pmid.y[i]
    if (test < 0 && xP$K[i] > xP$K[j] && xP$K[i] < S0) {
      vPK1 <- c(vPK1,xP$K[i])
      vPK2 <- c(vPK2,xP$K[j])
    }
  }
}
  
# same using ask*ask - bid*bid
yPK1 <- c()
yPK2 <- c()
  
for (i in 1:nP.12){
  for (j in 1:nP.12){
    test <- xP$Pask.x[i]*xP$Pask.y[j] - xP$Pbid.x[j]*xP$Pbid.y[i]
    if (test < 0 && xP$K[i] > xP$K[j] && xP$K[i] < S0) {
      yPK1 <- c(yPK1,xP$K[i])
      yPK2 <- c(yPK2,xP$K[j])
    }
  }
}
  
plot(vPK1,vPK2, col="blue", pch=20, 
       xlim=c(Kmin,Kmax), ylim=c(Kmin,Kmax),
       main="Puts: SPX T1=6/16/23(m), T2=7/21/23(m)", xlab="K(i)", ylab="K(j)")
lines(yPK1,yPK2, col="yellow", type="p",pch=20)
abline(a=0,b=1,col="red")
abline(h=S0, col="red")
abline(v=S0, col="red")
  
return()
}
#---------- ---------------------
#==============================================
TP2ViolationsCounts <- function(dataC1, dataC2, S0){
# returns: n.1 = 4-uples with at least one trade
# n.50 = 4-uples with at least 50 trades
# n.b1 = breaks with at least one trade
# n.b50 = breaks with at least 50 trades
  
#--- build the overlap of T1, T2
x <- merge(dataC1, dataC2, by="K")
  
##########################################################
# construct a matrix of tcvx[i,j] = C1[i]*C2[j] - C1[j]*C2[i]
# K Cbid.x Cofr.x  Cmid.x Cbid.y Cofr.y  Cmid.y
n.12 <- length(x$K)

n.q <- 0
#n.1 <- 0

n.b <- 0
#n.b1 <- 0

  
for (i in 1:n.12){
  for (j in 1:n.12){
    if (x$K[i] < x$K[j] && x$K[i] > S0)  n.q <- n.q +1
    test <- x$Cmid.x[i]*x$Cmid.y[j] - x$Cmid.x[j]*x$Cmid.y[i]
#    minvol <- min(x$volume.x[i],x$volume.x[j],x$volume.y[i],x$volume.x[j])
    if (test < 0 && x$K[i] < x$K[j] && x$K[i] > S0 ) n.b <- n.b+1 

#    if (test < 0 && x$K[i] < x$K[j] && x$K[i] > S0 && minvol >= 1) n.b1 <- n.b1+1
#    if (x$K[i] < x$K[j] && x$K[i] > S0 && minvol >= 1) n.1 <- n.1 + 1
  }
}
  
counts <- data.frame("n.q"=n.q, "n.b"=n.b)
  
  return(counts)
}

#################################################################
#==============================================
TP2Violations <- function(dataC1, dataC2, F1, F2){
# lists (K1,K2) for TP2 violations with minimum volume threshold
  
#--- build the overlap of T1, T2
x <- merge(dataC1, dataC2, by="K")
  
##########################################################
# construct a matrix of tcvx[i,j] = C1[i]*C2[j] - C1[j]*C2[i]
# K Cbid.x Cofr.x  Cmid.x Cbid.y Cofr.y  Cmid.y
n.12 <- length(x$K)
  
vK1 <- c()
vK2 <- c()
tp2 <- c()
  
for (i in 1:n.12){
  for (j in 1:n.12){
    test <- x$Cmid.x[i]*x$Cmid.y[j] - x$Cmid.x[j]*x$Cmid.y[i]
    if (test < 0 && x$K[i]/F1 < x$K[j]/F2 ) {
      vK1 <- c(vK1,x$K[i])
      vK2 <- c(vK2,x$K[j])
      tp2 <- c(tp2,test)
       
    }
  }
}
  
  violations <- data.frame("K1"=vK1,"K2"=vK2,"det"=tp2)  

  return(violations)
}

#################################################################


#################################################################
# findCeil(dataC1,x) find smallest index ind such that dataC1[ind]>x
CKtilde <- function(dataC, x){
  n.K <- length(dataC$K)
  if (dataC$K[n.K] < x) { 
#    print('x not in range')
    all.Kind <- data.frame("ind"=n.K,"K"=dataC$K[n.K])
    return (all.Kind)
    }
  
  ind <- 1
  while (dataC$K[ind] < x) {
#    print(dataC$K[ind])
    ind <- ind + 1
  }
  
  K.tilde <- dataC$K[ind]
  all.Ktilde <- data.frame("ind"=ind,"K"=K.tilde)
  
  return(all.Ktilde)
}
#################################################################
#################################################################
testTP2CallsFwd <- function(dataC1, dataC2, F1, F2, showPlot, Kmin, Kmax){

dataC12 <- merge(dataC1, dataC2, by="K")
  
##########################################################
# for all 1 < k1/F1 < k2/F2 compute
# det = C1(k1) * C2(k2) - C1(k2*F1/F2)* C2(k1*F2/F1)
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
#    if (dataC12$K[i]/F1 < dataC12$K[j]/F2 && dataC12$K[i]/F1 >= 1)  {
    if (dataC12$K[i]/F1 < dataC12$K[j]/F2 )  {
     n.q <- n.q +1
     x1 <- dataC12$K[i]*F2/F1 # this is K1*F2/F1
     x2 <- dataC12$K[j]*F1/F2 # this is K2*F1/F2
     K1.ind <- CKtilde(dataC12, x1)
     K2.ind <- CKtilde(dataC12, x2)
    
     ind1 <- K1.ind$ind
     ind2 <- K2.ind$ind
    
# TP2 test    
test <- dataC12$Cmid.x[i]*dataC12$Cmid.y[j] - dataC12$Cmid.x[ind2]*dataC12$Cmid.y[ind1]
    
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
testRR2PutsFwd <- function(dataP1, dataP2, F1, F2, showPlot, Kmin, Kmax){
  
  dataP12 <- merge(dataP1, dataP2, by="K")
  
  ##########################################################
  # for all 1 < k1/F1 < k2/F2 compute
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
      if (dataP12$K[i]/F1 < dataP12$K[j]/F2 )  {
        n.q <- n.q +1
        x1 <- dataP12$K[i]*F2/F1 # this is K1*F2/F1
        x2 <- dataP12$K[j]*F1/F2 # this is K2*F1/F2
        K1.ind <- CKtilde(dataP12, x1)
        K2.ind <- CKtilde(dataP12, x2)
        
        ind1 <- K1.ind$ind
        ind2 <- K2.ind$ind
        
        # RR2 test    
        test <- dataP12$Pmid.x[i]*dataP12$Pmid.y[j] - dataP12$Pmid.x[ind2]*dataP12$Pmid.y[ind1]
        
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
         main="Puts: (T1, T2)", xlab="K(i)/F(T1)", ylab="K(j)/F(T2)")
    abline(a=0,b=1,col="red")
    abline(h=1, col="red")
    abline(v=1, col="red")
  }
  
  counts <- data.frame("n.q"=n.q, "n.b"=n.b)
  
  return(counts)
  
  
}
#----------------------------------------------------
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
  
if (showPlot ==  1){
  plot(vK1,vK2, col="blue", type="p", pch=20, 
        xlim=c(Kmin,Kmax), ylim=c(Kmin,Kmax),
        main="Calls", xlab="K1/S0", ylab="K2/S0")
  lines(vL1,vL2, col="yellow", type="p", pch=20)
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
  
if (showPlot ==  1){
  plot(vK1,vK2, col="blue", pch=20, 
        xlim=c(Kmin,Kmax), ylim=c(Kmin,Kmax),
        main="Puts: (T1 < T2)", xlab="K1/S0", ylab="K2/S0")
  lines(vL1,vL2, col="yellow", type="p", pch=20) 
  abline(a=0,b=1,col="red")
  abline(h=1, col="red")
  abline(v=1, col="red")
}
  
counts <- data.frame("n.q"=n.q, "n.b"=n.b)
  
return(counts)
  
  
}
#----------------------------------------------------
listTP2Violations <- function(dataC1, dataC2, F1, F2){

  dataC12 <- merge(dataC1, dataC2, by="K")
  
  ##########################################################
  # for all k1/F1 < k2/F2 compute
  # det = C1(k1) * C2(k2) - C1(k2*F1/F2)* C2(k1*F2/F1)
  # construct a matrix of tcvx[i,j] = C1[i]*C2[j] - C1[j]*C2[i]
  # K Cbid.x Cofr.x  Cmid.x Cbid.y Cofr.y  Cmid.y
  n.12 <- length(dataC12$K)
  
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
      #    if (dataC12$K[i]/F1 < dataC12$K[j]/F2 && dataC12$K[i]/F1 >= 1)  {
      if (dataC12$K[i]/F1 < dataC12$K[j]/F2 )  {
        n.q <- n.q +1
        x1 <- dataC12$K[i]*F2/F1 # this is K1*F2/F1
        x2 <- dataC12$K[j]*F1/F2 # this is K2*F1/F2
        K1.ind <- CKtilde(dataC12, x1)
        K2.ind <- CKtilde(dataC12, x2)
        
        ind1 <- K1.ind$ind
        ind2 <- K2.ind$ind
        
        # TP2 test    
        test <- dataC12$Cmid.x[i]*dataC12$Cmid.y[j] - dataC12$Cmid.x[ind2]*dataC12$Cmid.y[ind1]
        
        #    if (test < 0 && dataC12$K[i]/F1 < dataC12$K[j]/F2 && dataC12$K[i]/F1 >= 1) {
        if (test < 0) {
          vK1 <- c(vK1,dataC12$K[i])
          vK2 <- c(vK2,dataC12$K[j])
          all.test <- c(all.test, test)
          n.b <- n.b + 1
        }
      }
    }
  }
  
  list.breaks <- data.frame("K1"=vK1, "K2"=vK2, "det"=all.test)
  
  counts <- data.frame("n.q"=n.q, "n.b"=n.b)
  print(n.q)
  print(n.b)
  return(list.breaks)
  
  
}
#-------------------------------------------------------------
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