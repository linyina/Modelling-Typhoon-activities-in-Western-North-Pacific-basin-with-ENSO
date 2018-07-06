## Reading the data
SST<- read.table(file = "./Data/SST/oisst.v2 monthly SST.txt", header = TRUE, sep = "")

trial.ts <- ts(SST$NINO3.4, frequency=12,start = c(1982,1))
start(trial.ts)
end(trial.ts)
plot.ts(trial.ts, type="l", ylab = "Temperature", main="Sea surface temperature in Nino 3.4 index")

xyplot(SST$NINO3.4 ~ SST$YR | as.factor(SST$MON), type='l',
       xlab="Year",ylab="SST",
       main="SST in Nino3.4 index for different months")

plot.ts(SST$NINO3.4, type="l", ylab = "Temperature")
plot(SST$ANOM.3, type="l")


TNI<- read.table(file = "./Data/SST/TNI.txt", header = FALSE, sep = "")
SST$TNI<-c(0)
SST[SST$YR==1982,]$TNI<-c(TNI[TNI$V1==1982,][,-1])

for (i in 1982:2016){
  SST[SST$YR==i,]$TNI<-c(TNI[TNI$V1==i,][,-1])
}

str(SST)
SST$TNI<- as.numeric(SST$TNI)
