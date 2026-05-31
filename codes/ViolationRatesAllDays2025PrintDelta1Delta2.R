#Load parquet data
library(arrow)

rm(list = ls())
source("dataFunctionsFixedTParquet.R")
#source("getPlotsOMFixedTAmerican.R")
source("listViolationsFixedTAmerican.R")


# load SPY options data for 2025
my_data <- read_parquet("mktdata/spy_options_2025.parquet")


#View(allData)
names(my_data)
length(my_data$exdate)
# 1,584,733 rows
head(my_data)
#-----------------------------------------------------
summary(my_data$date) # all pricing dates in the data
table(my_data$date)

#dataOneDay$exdate <- droplevels(dataOneDay$exdate)
AsOfDates <- unique(my_data$date)

head(AsOfDates)

#datesExpiry <- levels(datesExpiry)
AsOfDates <- as.Date(AsOfDates, "%Y-%m-%d")
AsOfDates <- sort(AsOfDates)
head(AsOfDates)
tail(AsOfDates)
n.asof <- length(AsOfDates)
n.asof
# 165 dates
#=====================================================================
# write to csv
#write.csv(my_data, file="mktdata/SPY_2025.csv")
######################################################################

delC1.all <- c()
delC2.all <- c()
delP1.all <- c()
delP2.all <- c()

iasC.day <- c()
iasP.day <- c()

#bp12.day <- c()
#qp12.day <- c()
#vrp12.day <- c()
#----------------------
#loop through all AsOfDates
for(ias in 1:n.asof){
#for(ias in 1:10){
  print(ias)
#  ias <- 1
  oneDay <- AsOfDates[ias]
#oneDay <- '2025-08-06'

S0 <- 629.17

dataOneDay <- my_data[ which(my_data$date==oneDay),]

#==================================================================
#==================================================================
# extract all expirations
#summary(dataOneDay$exdate)
#table(dataOneDay$exdate)

#dataOneDay$exdate <- droplevels(dataOneDay$exdate)
datesExpiry <- unique(dataOneDay$exdate)

#head(datesExpiry)

#datesExpiry <- levels(datesExpiry)
datesExpiry <- as.Date(datesExpiry, "%Y-%m-%d")
datesExpiry <- sort(datesExpiry)
head(datesExpiry)
tail(datesExpiry)
n.exp <- length(datesExpiry)
n.exp


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
#----------- TP2 breaks for all (T1,T2) pairs ----------------------
#source("dataFunctionsOMDeltaFixedT.R")
source("listViolationsFixedTAmerican.R")
count.q <- c()
count.b <- c()
vr <- c()
t.a <- c()
t.b <- c()
t.1 <- c()
t.2 <- c()
b.sum <- 0
q.sum <- 0

delC1.day <- c()
delC2.day <- c()
delP1.day <- c()
delP2.day <- c()

# collect all delC1,delC2 for AsOf day ias
for (i in 2:n.exp){
  for (j in 2:n.exp){
    if(j-i>0){
      dataC1 <- dataCall(dataOneDay,datesExpiry[i])
      dataC2 <- dataCall(dataOneDay,datesExpiry[j])
#      print(i)
#      print(j)
#      tij <- testTP2CallsAmer(dataC1, dataC2, S0, S0, 0, 1.0, 1.1)
      breaks.ij <- listTP2ViolationsAmer(dataC1, dataC2, S0, S0)
      n.vioC <- length(breaks.ij$del1)
#      print("n.vioC")
#      print(n.vioC)
      ias.vector <- rep(ias, times=n.vioC)
      iasC.day <- c(iasC.day, ias.vector)
#      for (jb in 1:n.vioC) iasC.day <- c(iasC.day, ias)
      # testTP2CallsFwd(dataC1, dataC2, f1, f2, showPlot, Kmin/F1, Kmax/F1)
# returns a list of all TP2 breaks (K1, K2, det,del1,del2) and counts (n.q,n.b)
      delC1.day <- c(delC1.day, breaks.ij$del1)
      delC2.day <- c(delC2.day, breaks.ij$del2)
#      vr <- c(vr, tij$n.b/tij$n.q)
#      b.sum <- b.sum + tij$n.b
#      q.sum <- q.sum + tij$n.q

#      t1c.day <- c(t1c.day, Tdays[i])
#      t2c.day <- c(t2c.day, Tdays[j])
#      ias.day <- c(ias.day, ias)

    }
  }
}

length(delC1.day)
length(delC2.day)

#b.sum
#q.sum
#vrc <- b.sum/q.sum

delC1.all <- c(delC1.all, delC1.day)
delC2.all <- c(delC2.all, delC2.day)
#bc.day <- c(bc.day, b.sum)
#qc.day <- c(qc.day, q.sum)
#vrc.day <- c(vrc.day, vrc)


#-------------------------------------------------------------------
#----------- RR2 breaks for all (T1,T2) pairs ----------------------
#-------------------------------------------------------------------
#source("getPlotsOMFixedTAmerican.R")
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
#      print(i)
#      print(j)
      breaks.ij <- listRR2ViolationsAmer(dataP1, dataP2, S0, S0)
      n.vioP <- length(breaks.ij$del1)
#      print("n.vioP")
#      print(n.vioP)
      ias.vector <- rep(ias, times=n.vioP)
      iasP.day <- c(iasP.day, ias.vector)
      # returns a list of all TP2 breaks (K1, K2, det,del1,del2) a
      delP1.day <- c(delP1.day, breaks.ij$del1)
      delP2.day <- c(delP2.day, breaks.ij$del2)
    }
  }
}

delP1.all <- c(delP1.all, delP1.day)
delP2.all <- c(delP2.all, delP2.day)

# end loop over AsOfDates
}

length(iasC.day)
length(delC1.all)
length(delC2.all)

length(iasP.day)
length(delP1.all)
length(delP2.all)

# write all data to a data.frame
delta.C.all <- data.frame("ias"=iasC.day,"delC1"=delC1.all,"delC2"=delC2.all)
head(delta.C.all)
tail(delta.C.all)

delta.P.all <- data.frame("ias"=iasP.day,"delP1"=delP1.all,"delP2"=delP2.all)
head(delta.P.all)
tail(delta.P.all)


?hist

# plot the distribution of Deltas
summary(delC1.all)
delC1freq <- hist(delC1.all, breaks=seq(0,1,0.01), xlim=c(0,1), freq=FALSE,
                  main="Density of Delta C(K1T1)")
summary(delC2.all)
delC2freq <- hist(delC2.all, breaks=seq(0,1,0.01), xlim=c(0,1), freq=FALSE,
                  main="Density of Delta C(K2T2)")

plot(delC1freq$density, type="l", pch=20, col="blue", main="Density of Delta(K1T1) (blue) and Delta(K2T2) (red)", xlab="Delta")
#lines(delC1freq$density, type="l", col="blue")
lines(delC2freq$density, type="l", pch=20, col="red")
#lines(delC2freq$density, type="l", col="red")

plot(delC1.all[1:10000], delC2.all[1:10000], type="p", pch=20, 
     main="Delta TP2 violations", xlab="Delta C(K1T1)", ylab="Delta C(K2T2)")
abline(a=0,b=1,col="red")

summary(delP1.all)
summary(delP2.all)

delP1freq <- hist(delP1.all, breaks=seq(-1,0,0.01), xlim=c(-1,0), freq=FALSE,
                  main="Density of Delta P(K1T1)")
delP2freq <- hist(delP2.all, breaks=seq(-1,0,0.01), xlim=c(-1,0), freq=FALSE,
                  main="Density of Delta P(K2T2)")


plot(delP1.all[1:10000], delP2.all[1:10000], type="p", pch=20, 
     main="Delta RR2 violations", xlab="Delta P(K1T1)", ylab="Delta P(K2T2)")
abline(a=0,b=1,col="red")

#write all T1,T2 calls data to a data.frame
#v.call.all <- data.frame("asof"=ias.day, "vio.C"=bc12.day,"no.C"=qc12.day,"vr.C"=vrc12.day,"t1"=t1c.day,"t2"=t2c.day)
#head(v.call.all)
#tail(v.call.all)
#length(v.call.all$asof)

#write all T1,T2 puts data to a data.frame


write.csv(delta.C.all, "DeltaCalls-10days-2025.csv")
write.csv(delta.P.all, "DeltaPuts-10days-2025.csv")
Fwd[1]

n.asof
#---------------- aggregate over T1,T2 ----------------------
df.call.t1t2 <- aggregate( cbind(no.C,vio.C) ~ t1+t2, v.call.all, sum)
head(df.call.t1t2,20)

df.call.t1t2$vrc <- df.call.t1t2$vio.C/df.call.t1t2$no.C

nc.vr <- length(df.call.t1t2$t1)
nc.vr

#--- plot the vio ratios for calls vs (T1,T2)
vcT1 <- c()
vcT2 <- c()

for (ivr in 1:nc.vr){
  if(df.call.t1t2$vrc[ivr] > 0.05) {
    vcT1 <- c(vcT1,df.call.t1t2$t1[ivr])
    vcT2 <- c(vcT2,df.call.t1t2$t2[ivr])
  }
}

#length(vcT1)
plot(vcT1,vcT2,type="p",pch=20,col="grey",
     main="Calls 2025. TP2 vr > 1%(blue),2%(red),5%(yellow)",
     xlab="T1 [days]", ylab="T2 [days]",
     xlim=c(0,360), ylim=c(0,360))
lines(vcT1,vcT2,type="p",pch=20,col="blue")
lines(vcT1,vcT2,type="p",pch=20,col="red")
lines(vcT1,vcT2,type="p",pch=20,col="yellow")


# --- plot violation ratios for puts aggregated over (T1,T2)
#---------------- aggregate over T1,T2 ----------------------
df.put.t1t2 <- aggregate( cbind(no.P,vio.P) ~ t1+t2, v.put.all, sum)
head(df.put.t1t2,100)

df.put.t1t2 <- na.omit(df.put.t1t2)

df.put.t1t2$vrc <- df.put.t1t2$vio.P/df.put.t1t2$no.P

np.vr <- length(df.put.t1t2$t1)
np.vr

#--- plot the vio ratios for puts vs (T1,T2)
vpT1 <- c()
vpT2 <- c()

for (ivr in 1:np.vr){
  if(df.put.t1t2$vrc[ivr] > 0.15) {
    vpT1 <- c(vpT1,df.put.t1t2$t1[ivr])
    vpT2 <- c(vpT2,df.put.t1t2$t2[ivr])
  }
}

#length(vcT1)
plot(vpT1,vpT2,type="p",pch=20,col="grey",
     main="Puts 2025. RR2 vr > 5%(blue),10%(red),15%(yellow)",
     xlab="T1 [days]", ylab="T2 [days]",
     xlim=c(0,360), ylim=c(0,360))
lines(vpT1,vpT2,type="p",pch=20,col="blue")
lines(vpT1,vpT2,type="p",pch=20,col="red")
lines(vpT1,vpT2,type="p",pch=20,col="yellow")

summary(df.put.t1t2$vrc)
summary(df.call.t1t2$vrc)

sum(df.call.t1t2$vio.C)/sum(df.call.t1t2$no.C)
# vr.c = 0.4077% ok correct

sum(df.put.t1t2$vio.P)/sum(df.put.t1t2$no.P)
# vr.c = 1.8975% ok correct



#--------------------- Plot the violation ratios ------------
plot(1:n.asof, 100*vrc.day, type = "p", pch=20, col="blue", xaxt = "none", xlab = "date",
     ylim=c(0,15),ylab="Violation Ratios [%]",main="SPY violation ratios 2025 - No Delta Filter - Calls(blue), Puts(red)")
lines(1:n.asof, 100*vrc.day, type = "l", col="blue")
lines(1:n.asof, 100*vrp.day, type = "l", col="red")
lines(1:n.asof, 100*vrp.day, type = "p", pch=20, col="red")

#abline(h=0.408, lty=2, col="blue")
#abline(h=1.897, lty=2, col="red")

abline(h=5.697, lty=2, col="blue")
abline(h=5.656, lty=2, col="red")

axis(side = 1, at = 1:n.asof, labels = AsOfDates)


sum(bc.day)/sum(qc.day)
sum(bp.day)/sum(qp.day)

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


