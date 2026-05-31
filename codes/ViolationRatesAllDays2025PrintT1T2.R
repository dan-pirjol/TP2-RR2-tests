#Load parquet data
library(arrow)

rm(list = ls())
source("dataFunctionsFixedTParquet.R")
#source("getPlotsOMFixedTAmerican.R")
source("computeViolationsFixedTAmerican.R")


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
bc.day <- c()
qc.day <- c()
vrc.day <- c()
bp.day <- c()
qp.day <- c()
vrp.day <- c()
#-------- calls -----
t1c.day <- c()
t2c.day <- c()
ias.day <- c()

bc12.day <- c()
qc12.day <- c()
vrc12.day <- c()
#--------- puts ------
t1p.day <- c()
t2p.day <- c()
iasp.day <- c()

bp12.day <- c()
qp12.day <- c()
vrp12.day <- c()
#----------------------
#loop through all AsOfDates
for(ias in 1:n.asof){
#for(ias in 1:30){
  
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
  head(dataCi)
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
source("computeViolationsFixedTAmerican.R")
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



for (i in 2:n.exp){
  for (j in 2:n.exp){
    if(j-i>0){
      dataC1 <- dataCall(dataOneDay,datesExpiry[i])
      dataC2 <- dataCall(dataOneDay,datesExpiry[j])
      print(i)
      print(j)
      # use listTP2ViolationsAmer(dataC1, dataC2, S0, S0)
      tij <- testTP2CallsAmer(dataC1, dataC2, S0, S0, 0, 1.0, 1.1)
# testTP2CallsFwd(dataC1, dataC2, f1, f2, showPlot, Kmin/F1, Kmax/F1)
# returns a list of all TP2 breaks (K1, K2, det, delta1, delta22) and counts (n.q,n.b)
      count.q <- c(count.q, tij$n.q)
      count.b <- c(count.b, tij$n.b)
      vr <- c(vr, tij$n.b/tij$n.q)
      b.sum <- b.sum + tij$n.b
      q.sum <- q.sum + tij$n.q

# original version: returns days to expiry Tdays      
#      t.1 <- c(t.1,Tdays[i])
#      t.2 <- c(t.2,Tdays[j])
#      t.a <- c(t.a,i)
#      t.b <- c(t.b,j)
      t1c.day <- c(t1c.day, Tdays[i])
      t2c.day <- c(t2c.day, Tdays[j])
      ias.day <- c(ias.day, ias)
      bc12.day <- c(bc12.day, tij$n.b)
      qc12.day <- c(qc12.day, tij$n.q)
      vrc12.day <- c(vrc12.day, tij$n.b/tij$n.q)
    }
  }
}


b.sum
q.sum
vrc <- b.sum/q.sum

bc.day <- c(bc.day, b.sum)
qc.day <- c(qc.day, q.sum)
vrc.day <- c(vrc.day, vrc)


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
      print(i)
      print(j)
      tij <- testRR2PutsAmer(dataP1, dataP2, S0, S0, 0, 1.0, 1.1)
      
      count.q <- c(count.q, tij$n.q)
      count.b <- c(count.b, tij$n.b)
      vr <- c(vr, tij$n.b/tij$n.q)
      b.sum <- b.sum + tij$n.b
      q.sum <- q.sum + tij$n.q
      
# original version: returns days to expiry Tdays      
#      t.1 <- c(t.1,Tdays[i])
#      t.2 <- c(t.2,Tdays[j])
#      t.a <- c(t.a,i)
#      t.b <- c(t.b,j)
      t1p.day <- c(t1p.day, Tdays[i])
      t2p.day <- c(t2p.day, Tdays[j])
      iasp.day <- c(iasp.day, ias)
      bp12.day <- c(bp12.day, tij$n.b)
      qp12.day <- c(qp12.day, tij$n.q)
      vrp12.day <- c(vrp12.day, tij$n.b/tij$n.q)
    }
  }
}

b.sum
q.sum
b.sum/q.sum

vrp <- b.sum/q.sum

bp.day <- c(bp.day, b.sum)
qp.day <- c(qp.day, q.sum)
vrp.day <- c(vrp.day, vrp)

# end loop over AsOfDates
}


# write all data to a data.frame
#v.rates.all <- data.frame("vio.C"=bc.day,"no.C"=qc.day,"vr.c"=vrc.day,"vio.P"=bp.day,"no.P"=qp.day, "vr.p"=vrp.day)
#head(v.rates.all)
#tail(v.rates.all)

#write all T1,T2 calls data to a data.frame
v.call.all <- data.frame("asof"=ias.day, "vio.C"=bc12.day,"no.C"=qc12.day,"vr.C"=vrc12.day,"t1"=t1c.day,"t2"=t2c.day)
head(v.call.all)
tail(v.call.all)
length(v.call.all$asof)

#write all T1,T2 puts data to a data.frame
v.put.all <- data.frame("asof"=iasp.day, "vio.P"=bp12.day,"no.P"=qp12.day,"vr.P"=vrp12.day,"t1"=t1p.day,"t2"=t2p.day)
head(v.put.all)
tail(v.put.all)
length(v.put.all$asof)

write.csv(v.call.all, "VCallDataT1T2-165days-2025.csv")
write.csv(v.put.all, "VPutDataT1T2-165days-2025.csv")
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


