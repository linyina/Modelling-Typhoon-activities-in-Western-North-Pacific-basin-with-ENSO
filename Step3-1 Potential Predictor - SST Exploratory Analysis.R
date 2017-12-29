## Reading the data
SST<- read.table(file = "./Data/SST/oisst.v2 monthly SST.txt", header = TRUE, sep = "")


plot(SST$NINO3.4, type="l")
plot(SST$ANOM.3, type="l")
