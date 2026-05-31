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
#-----------------------------------------------------
summary(my_data$date) # all pricing dates in the data
table(my_data$date)

#dataOneDay$exdate <- droplevels(dataOneDay$exdate)
AsOfDates <- unique(my_data$date)

head(AsOfDates)

#datesExpiry <- levels(datesExpiry)
AsOfDates <- as.Date(AsOfDates, "%Y-%m-%d")
AsOfDates <- sort(AsOfDates)
head(AsOfDates,81)
head(AsOfDates,144)
tail(AsOfDates)
n.asof <- length(AsOfDates)
n.asof
# 165 dates
# 1:20 Jan 2025
# 61:81 Apr 2025
# 123:144 july 2025

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



#loop through all AsOfDates
for(ias in 1:n.asof){
#loop through all Jan.2025 dates 1:20 and April 61:81
#  for(ias in 123:144){
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

# restrict to expiries below 30 days
imax <- 1
for (id in 1:n.exp){
  if(Tdays[id] < 30) imax <- imax + 1
  }
imax

Tdays[imax]
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
imax

# run only over 30 days maturities
#for (i in 2:n.exp){
#  for (j in 2:n.exp){
for (i in 2:imax){
  for (j in 2:imax){
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
vrc <- b.sum/q.sum
vrc

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

#for (i in 2:n.exp){
#  for (j in 2:n.exp){
for (i in 2:imax){
  for (j in 2:imax){
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

vrp <- b.sum/q.sum

bp.day <- c(bp.day, b.sum)
qp.day <- c(qp.day, q.sum)
vrp.day <- c(vrp.day, vrp)

# end loop over AsOfDates
}


# write all data to a data.frame
v.rates.all <- data.frame("vio.C"=bc.day,"no.C"=qc.day,"vr.c"=vrc.day,"vio.P"=bp.day,"no.P"=qp.day, "vr.p"=vrp.day)
head(v.rates.all)
tail(v.rates.all)


write.csv(v.rates.all, "VRates165days2025maxmat30days.csv")

#average vio rates for Jan 2025
sum(v.rates.all$vio.C)
sum(v.rates.all$no.C)

sum(v.rates.all$vio.C)/sum(v.rates.all$no.C)
# vrCJan25) = 0.578%
#vrc(Jul25) =0.292%

sum(v.rates.all$vio.P)
sum(v.rates.all$no.P)

sum(v.rates.all$vio.P)/sum(v.rates.all$no.P)
# vrCJan25) = 4.15%
# vr(Jul25) = 5.39%

n.asof

vrc.30d <- vrc.day
vrp.30d <- vrp.day

vrc.apr.safe <- vrc.day
vrp.apr.safe <- vrp.day

vrc.apr.safe

#--------------------- Plot the violation ratios April 2025 ------------
vrates.2025 <- read.csv(file = 'results/VRates165days2025.csv')
head(vrates.2025)

vrc.all <- vrates.2025$vr.c
vrp.all <- vrates.2025$vr.p

plot(1:165, 100*vrc.30d, type = "p", pch=20, col="blue", xaxt = "none", xlab = "date",
     ylim=c(0,2),ylab="Violation Ratios [%]",main="TP2(blue) 2025: 30d (continuous), all (dashed)")
lines(1:165, 100*vrc.30d, type = "l", col="blue")
#lines(1:165, 100*vrc.all, type = "p", col="blue")
lines(1:165, 100*vrc.all, type = "l", lty=2, col="blue")

plot(1:165, 100*vrp.30d, type = "p", pch=20, col="red", xaxt = "none", xlab = "date",
     ylab="Violation Ratios [%]",main="RR2(red) 2025: 30d (continuous), all (dashed)")
lines(1:165, 100*vrp.30d, type = "l", col="red")
#lines(1:165, 100*vrp.all, type = "p", col="red")
lines(1:165, 100*vrp.all, type = "l", lty=2, col="red")
axis(side = 1, at = 1:165, labels = AsOfDates)

#----- Plot Jan 2025 ----------------------------
plot(1:20, 100*vrc.jan.safe, type = "p", pch=20, col="blue", xaxt = "none", xlab = "date",
     ylim=c(0,10),ylab="Violation Ratios [%]",main="RR2(red) 2025: 30d (continuous), all (dashed)")
lines(1:20, 100*vrc.jan.safe, type = "l", col="blue")
lines(1:20, 100*vrc.all[1:20], type = "p", col="blue")
lines(1:20, 100*vrc.all[1:20], type = "l", lty=2, col="blue")

lines(1:20, 100*vrp.jan.safe, type = "p", pch=20, col="red")
lines(1:20, 100*vrp.jan.safe, type = "l", col="red")
lines(1:20, 100*vrp.all[1:20], type = "p", col="red")
lines(1:20, 100*vrp.all[1:20], type = "l", lty=2, col="red")
axis(side = 1, at = 1:20, labels = AsOfDates[1:20])


#---- old plots --------------------------------
n.asof
#---- old plots --------------------------------
n.asof
length(vrc.day)
plot(1:165, 100*vrc.all, type = "p", pch=20, col="blue", xaxt = "none", xlab = "date",
     ylim=c(0,10),ylab="Violation Ratios [%]",main="SPY violation ratios 2025 - Calls(blue), Puts(red)")
lines(1:165, 100*vrc.all, type = "l", col="blue")
lines(1:165, 100*vrp.all, type = "l", col="red")
lines(1:165, 100*vrp.all, type = "p", pch=20, col="red")

#abline(h=0.408, lty=2, col="blue")
#abline(h=1.897, lty=2, col="red")

abline(h=1.090, lty=2, col="blue")
abline(h=1.43, lty=2, col="red")

#axis(side = 1, at = 1:n.asof, labels = AsOfDates)
axis(side = 1, at = 1:21, labels = AsOfDates[61:81])


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


