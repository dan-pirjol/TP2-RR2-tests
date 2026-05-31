#Load parquet data
library(arrow)

rm(list = ls())
source("dataFunctionsFixedTParquet.R")
source("getPlotsOMFixedTAmerican.R")


# load SPY options data for 2025
my_data <- read_parquet("mktdata/spy_options_2025.parquet")


#View(allData)
names(my_data)
length(my_data$exdate)
# 1,584,733 rows
head(my_data)

summary(my_data$date) # all pricing dates in the data
#=====================================================================
# write to csv
#write.csv(my_data, file="mktdata/SPY_2025.csv")
######################################################################
oneDay <- '2025-08-06'

S0 <- 629.17

dataOneDay <- my_data[ which(my_data$date==oneDay),]

#==================================================================
#==================================================================
# extract all expirations
summary(dataOneDay$exdate)
table(dataOneDay$exdate)

#dataOneDay$exdate <- droplevels(dataOneDay$exdate)
datesExpiry <- unique(dataOneDay$exdate)

head(datesExpiry)

#datesExpiry <- levels(datesExpiry)
datesExpiry <- as.Date(datesExpiry, "%Y-%m-%d")
datesExpiry <- sort(datesExpiry)
head(datesExpiry)
tail(datesExpiry)
n.exp <- length(datesExpiry)
n.exp

data0DTE <- dataOneDay[ which(dataOneDay$exdate==oneDay),] 
head(data0DTE)

#Tgrid <- seq(0,length(datesExpiry))

Tdays <- numeric()
for (i in 1:n.exp) Tdays[i] <- as.numeric(datesExpiry[i]) - as.numeric(datesExpiry[1])
Tdays

# maturities in units of 5 days
#hist(Tdays, breaks=seq(0,1050,5), xlim=c(0,100))

# === compute the forwards and traded volumes ===========
n.KC <- c()
n.KP <- c()
call.vol <- 0
put.vol <- 0
Fwd <- c()

#dataCall(dataOneDay, datesExpiry[2])

for (i in 1:n.exp){
  dataCi <- dataCall(dataOneDay,datesExpiry[i])
  dataPi <- dataPut(dataOneDay,datesExpiry[i])
#  print(i)
  fwdPrice <- getForward(dataCi,dataPi)
  n.KC <- c(n.KC,length(dataCi$K))
  n.KP <- c(n.KP,length(dataPi$K))
  vol.data <- getTradedVolumes(dataCi,dataPi)
  if (i==1) vol.data <- data.frame("volC"=0, "volP"=0)
  call.vol <- call.vol + vol.data$volC
  put.vol <- put.vol + vol.data$volP
  Fwd <- c(Fwd, fwdPrice)
  
}

sum(n.KC)
sum(n.KP)
call.vol
put.vol
#write the forwards to a csv file c(date,expiration,ForwardPrice)
Fwd.OM <- data.frame("date"=oneDay,"expiration"=datesExpiry,
                     "ForwardPrice"=Fwd)
head(Fwd.OM,33)
#write.csv(Fwd.OM, file="SPY_forwards_11Feb2026.csv")

################################################
# plot some graphs 
source("dataFunctionsFixedTParquet.R")
dataC1 <- dataCall(dataOneDay,datesExpiry[1])
dataP1 <- dataPut(dataOneDay,datesExpiry[1])
head(dataC1)
head(dataP1)
datesExpiry[4]
tv <- getTradedVolumes(dataC1,dataP1)
tv$volC
head(dataP1,10)
fwdPrice1 <- getForward(dataC1,dataP1)
fwdPrice1

#head(dataC1,50)

# the implied volatility smile
plot(dataC1$K, dataC1$IV, type="p", pch=20, 
     xlim=c(500,800), ylim=c(0,1))
lines(dataP1$K, dataP1$IV, type="p", pch=20, col="red")



#======== plot counts of traded strikes n.KC and n.KP vs maturity ====================
plot(Tdays, n.KC, type="h",xlim=c(0,100),ylim=c(0,200),
     main="Calls: strikes traded on 10.Feb.26", 
     xlab="Maturity [days]", ylab="no.strikes")

plot(Tdays, n.KP, type="h",xlim=c(0,100),ylim=c(0,200),
     main="Puts: strikes traded on 10.Feb.26", 
     xlab="Maturity [days]", ylab="no.strikes")

plot(datesExpiry[1:24], n.KC[1:24], type="h",ylim=c(0,200),
     main="Calls: strikes traded on 10.Feb.26", 
     xlab="Maturity", ylab="n.KC")

length(datesExpiry)
length(n.KC)
Fwd

plot(Tdays, Fwd, type="p",pch=20,xlim=c(0,100),ylim=c(650,900),
     main="SPY Forward prices S0=689.04", 
     xlab="Maturity [days]", ylab="Fwd")
abline(h=S0, col="red")

Fwd[1]
S0
head(Fwd)


#---------- read in the forwards ------------------------------
################ using the YF-OM dataset ######################
#source("dataFunctionsOMDeltaFixedT.R")
#source("extractForward.R")
#fwd.1 <- extract.fwd('2021-10-01')
#head(fwd.1)
#tail(fwd.1)
#length(fwd.1$T)
FwdData <- read.csv(file = 'mktdata/SPY_forwards_6Feb2026.csv')
head(FwdData)

FwdIn <- FwdData$ForwardPrice
#---------------------------------------------------------------
#------------ graphical TP2 test ----------------------------------------
source("getPlotsOMFixedTAmerican.R")
ind1 <- 2
ind2 <- 3
dataC1 <- dataCall(dataOneDay,datesExpiry[ind1])
dataC2 <- dataCall(dataOneDay,datesExpiry[ind2])
head(datesExpiry)
datesExpiry[ind1]
datesExpiry[ind2]
#Fwd1 <- Fwd[ind1]
#Fwd2 <- Fwd[ind2]
#Fwd1
#Fwd2

#class(dataC1)

tij <- testTP2CallsAmer(dataC1, dataC2, S0, S0, 1, 0.9, 1.1)

head(tij)


#----------- TP2 breaks for all (T1,T2) pairs ----------------------
#source("dataFunctionsOMDeltaFixedT.R")
source("getPlotsOMFixedTAmerican.R")
count.q <- c()
count.b <- c()
vr <- c()
t.a <- c()
t.b <- c()
t.1 <- c()
t.2 <- c()
b.sum <- 0
q.sum <- 0

n.exp

#datesExpiry[20]
#Tdays[22]

for (i in 2:n.exp){
  for (j in 2:n.exp){
    if(j-i>0){
      dataC1 <- dataCall(dataOneDay,datesExpiry[i])
      dataC2 <- dataCall(dataOneDay,datesExpiry[j])
      print(i)
      print(j)
      tij <- testTP2CallsAmer(dataC1, dataC2, S0, S0, 0, 1.0, 1.1)
# testTP2CallsFwd(dataC1, dataC2, f1, f2, showPlot, Kmin/F1, Kmax/F1)
# returns a list of all TP2 breaks (K1, K2, det) and counts (n.q,n.b)
      count.q <- c(count.q, tij$n.q)
      count.b <- c(count.b, tij$n.b)
      vr <- c(vr, tij$n.b/tij$n.q)
      b.sum <- b.sum + tij$n.b
      q.sum <- q.sum + tij$n.q

# original version: returns days to expiry Tdays      
      t.1 <- c(t.1,Tdays[i])
      t.2 <- c(t.2,Tdays[j])
      t.a <- c(t.a,i)
      t.b <- c(t.b,j)
    }
  }
}


b.sum
q.sum
b.sum/q.sum

v.rates <- data.frame("ind1"=t.a,"ind2"=t.b,"T1"=t.1, "T2"=t.2, "nq"=count.q, "nb"=count.b, "vr"=vr)
head(v.rates)
tail(v.rates)

#vc.rates <- read.csv(file = 'VRatesCallsSPY6Feb2026.csv')
#sum(vc.rates$nb)
#sum(vc.rates$nq)
#sum(vc.rates$nb)/sum(vc.rates$nq)

write.csv(v.rates, "VRatesCallsSPY9Mar2026.csv")

#==========================================================
#---  Map TP2 violations for calls -------------------------
#==========================================================
# plot all pairs (T1,T2) for which vr > 0.01
vc.rates <- v.rates
nc.vr <- length(vc.rates$T1)
nc.vr

#32*31/2

(n.exp-2)*(n.exp-1)/2

vcT1 <- c()
vcT2 <- c()

for (ivr in 1:nc.vr){
  if(vc.rates$vr[ivr] > 0.05) {
    vcT1 <- c(vcT1,vc.rates$T1[ivr])
    vcT2 <- c(vcT2,vc.rates$T2[ivr])
}
}

#length(vcT1)
plot(vcT1,vcT2,type="p",pch=20,col="grey",
     main="18.Feb.Calls. v.ratios < 1%(blue),2%(red),5%(yellow)",
     xlab="T1 [days]", ylab="T2 [days]",
     xlim=c(0,360), ylim=c(0,360))
lines(vcT1,vcT2,type="p",pch=20,col="blue")
lines(vcT1,vcT2,type="p",pch=20,col="red")
lines(vcT1,vcT2,type="p",pch=20,col="yellow")

plot(Tdays/30, type="p", pch=20,col="blue",
     main="Expiries [months]")


head(datesExpiry,10)

tail(datesExpiry)

#============ RR2 violations =======================================
source("getPlotsOMFixedTAmerican.R")
#source("dataFunctionsOMDeltaFixedT.R")
ind1 <- 4
ind2 <- 5
dataP1 <- dataPut(dataOneDay,datesExpiry[ind1])
dataP2 <- dataPut(dataOneDay,datesExpiry[ind2])
Fwd1 <- Fwd[ind1]
Fwd2 <- Fwd[ind2]
datesExpiry[ind1]
datesExpiry[ind2]

testRR2PutsAmer(dataP1, dataP2, S0, S0, 1, 0.8, 1.1)

dataP12 <- merge(dataP1, dataP2, by="K")
n.12 <- length(dataP12$K)
n.12
head(dataP12)
tail(dataP12)

head(dataP1)
tail(dataP2)
#-------------------------------------------------------------------
#----------- RR2 breaks for all (T1,T2) pairs ----------------------
#-------------------------------------------------------------------
source("getPlotsOMFixedTAmerican.R")
count.q <- c()
count.b <- c()
vr <- c()
t.a <- c()
t.b <- c()
t.1 <- c()
t.2 <- c()
q.sum <- 0
b.sum <- 0


for (i in 2:n.exp){
  for (j in 2:n.exp){
    if(j-i>0){
      dataP1 <- dataPut(dataOneDay,datesExpiry[i])
      dataP2 <- dataPut(dataOneDay,datesExpiry[j])
      print(i)
      print(j)
      tij <- testRR2PutsAmer(dataP1, dataP2, S0, S0, 0, 1.0, 1.1)
      
      count.q <- c(count.q, tij$n.q)
      count.b <- c(count.b, tij$n.b)
      vr <- c(vr, tij$n.b/tij$n.q)
      b.sum <- b.sum + tij$n.b
      q.sum <- q.sum + tij$n.q
      
# original version: returns days to expiry Tdays      
      t.1 <- c(t.1,Tdays[i])
      t.2 <- c(t.2,Tdays[j])
      t.a <- c(t.a,i)
      t.b <- c(t.b,j)
    }
  }
}

b.sum
q.sum
b.sum/q.sum


v.rates.P <- data.frame("ind1"=t.a,"ind2"=t.b,"T1"=t.1,"T2"=t.2,"nq"=count.q, "nb"=count.b, "vr"=vr)
head(v.rates.P)
tail(v.rates.P)

#v.rates.P <- subset(v.rates.P, v.rates.P$T1<100)
#v.rates.P <- subset(v.rates.P, v.rates.P$T2<100)


#write.csv(v.rates.P, "StrongVRatesPutsSPY9Mar2026.csv")
Fwd[1]
#==========================================================
#---  Map RR2 violations for puts -------------------------
#==========================================================
# plot all pairs (T1,T2) for which vr > 0.01, 0.05, 0.1, etc
np.vr <- length(v.rates.P$T1)
np.vr

29*28/2

vpT1 <- c()
vpT2 <- c()

for (ivr in 1:np.vr){
  if(v.rates$vr[ivr] > 0.05) {
    vpT1 <- c(vpT1,v.rates$T1[ivr])
    vpT2 <- c(vpT2,v.rates$T2[ivr])
  }
}

plot(vpT1,vpT2,type="p",pch=20,col="blue",
     main="12.Feb.Puts. RR2 v.ratios > 1%(blue),2%(red),5%(yellow)",
     xlab="T1 [days]", ylab="T2 [days]",xlim=c(0,90),ylim=c(0,90))
lines(vpT1,vpT2,type="p",pch=20,col="red")
lines(vpT1,vpT2,type="p",pch=20,col="yellow")

plot(Tdays, type="p", pch=20,col="blue",
     main="Expiries [days]")

#--------- Heatmap of RR2 violation ratios  ------------------------------



#==================== summarize data ===========================
n.T
myFwd <- c()
nKC <- c()
nKP <- c()

for (i in 1:n.T){
  dataCi <- dataCall(allData,datesExpiry[i])
  dataPi <- dataPut(allData,datesExpiry[i])
  fwdi <- getForward(dataCi,dataPi)
  nC <- length(dataCi$K)
  nP <- length(dataPi$K)
  myFwd <- c(myFwd,fwdi)
  nKC <- c(nKC, nC)
  nKP <- c(nKP, nP)
}

allOptions <- data.frame('fwd'=myFwd, "nKC"=nKC, "nKP"=nKP)
allOptions

#================= sand box =======================
dataC1 <- dataCall(allData,datesExpiry[2])
dataC2 <- dataCall(allData,datesExpiry[3])



datesExpiry[5]
datesExpiry[8]

#----------------------------------------------------
dataC12 <- merge(dataC1, dataC2, by="K")
head(dataC12,10)
tail(dataC12)

n.12 <- length(dataC12$K)
n.12

i <- 5
j <- 8

F1 <- Fwd[i]
F2 <- Fwd[j]

x1 <- dataC12$K[i]*F2/F1
x2 <- dataC12$K[j]*F1/F2
x1
x2

K1.ind <- CKtilde(dataC12, x1)
K2.ind <- CKtilde(dataC12, x2)

#k1.tilde <- K1.ind$K
#k2.tilde <- K2.ind$K

ind1 <- K1.ind$ind
ind2 <- K2.ind$ind

test <- dataC12$Cmid.x[i]*dataC12$Cmid.y[j] 
- dataC12$Cmid.x[ind2]*x$Cmid.y[ind1]

#test <- dataC12$Cmid.x[i]*dataC12$Cmid.y[j] - dataC12$Cmid.x[j]*x$Cmid.y[i]
if (test < 0 && dataC12$K[i]/F1 < x$K[j]/F2 && dataC12$K[i]/F1 > 1) {
  vK1 <- c(vK1,dataC12$K[i]/F1)
  vK2 <- c(vK2,dataC12$K[j]/F2)
}


