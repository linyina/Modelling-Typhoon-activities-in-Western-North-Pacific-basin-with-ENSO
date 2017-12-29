## Construct Data for analysis


BST.mon.data<- data.frame(Year = BST.mon.TS$Year,
                          Month = BST.mon.TS$Month,
                          TS.freq = BST.mon.TS$freq,
                          TY.freq = BST.mon.TY$freq,
                          SuperTY.freq = BST.mon.SuperTY$freq)
str(BST.mon.data)
str(SST)

Nino<- data.frame(Year = SST$YR,
                  Month = SST$MON,
                  NINO3.4 = SST$NINO3.4,
                  ANOM3.4 = SST$ANOM.3)
BST.ana<- merge(Nino, BST.mon.data, sort = FALSE)
str(BST.ana)


write.table(BST.ana, "BST_analysis_data.dat", row.names = FALSE)


## Plotting
## Tropical Storm
par(mfrow=c(2,1))
plot(BST.ana$NINO3.4,BST.ana$TS.freq, xlab = "Monthly SST in region NINO3.4",
     ylab = "Numer of Tropical Storms +", main = "Original Scales")
plot(BST.ana$NINO3.4,BST.ana$TS.freq, xlab = "Monthly SST in region NINO3.4",
     ylab = "Numer of Tropical Storms +", log = "xy",main = "Logarithmic Scales")


