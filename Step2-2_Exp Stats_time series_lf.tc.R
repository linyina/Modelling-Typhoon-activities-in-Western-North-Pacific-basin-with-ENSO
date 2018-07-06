#######################################################
####  2.2 Exploratory Analysis
####
#######################################################
library(lattice)   # heatmap and xyplot
library(ggplot2)   # time series plot
library(MASS)
library(tidyr)  # time series plot
library(RColorBrewer) # Colours
library(reshape2) # melt
########################
if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)

## Part 1: Landfall Data
## a. Reconstruct data

## a.1: Cleaning
land.clean<- Landfall
land.clean[is.na(land.clean$`Total Times of Landfall`),]$`Total Times of Landfall` <- 0
land.clean$TC.freq <- rep(1, length(land.clean$Year))
land.clean[is.na(land.clean$`Serial Number`),]$`Serial Number` <- "000"
land.clean$`Landfall Intensity Category`<-as.factor(land.clean$`Landfall Intensity Category`)

land.clean$`Landfall Intensity Category` <- factor(land.clean$`Landfall Intensity Category`,
                                                   levels = c("","TD",
                                                              "TS","STS","TY", 
                                                              "STY","SuperTY"))
levels(land.clean$`Landfall Intensity Category`)
levels(land.clean$`Landfall Intensity Category`)<-c(0:6)
land.clean$`Landfall Intensity Category`<- as.numeric(as.character(land.clean$`Landfall Intensity Category`))
land.clean[is.na(land.clean$`Landfall Intensity Category`),]$`Landfall Intensity Category`<- 0
str(land.clean)

## a.2: Landfall Frequency
lf.temp<- data.frame(Year=rep(1949:2016, each=15), 
                          Province= rep(levels(Landfall$`Landfall Province`), 68))
lf.temp<- lf.temp[!(lf.temp$Province==""),]
lf.tc.freq<- aggregate(land.clean$TC.freq ~ land.clean$Year
                       + land.clean$`Landfall Province` + 
                         land.clean$`Landfall Intensity Category`, land.clean, sum)
names(lf.tc.freq)<- c("Year", "Province", "Intensity", "tc.freq")
lf.tc.freq<- merge(lf.temp, lf.tc.freq, all=TRUE)
lf.tc.freq[is.na(lf.tc.freq$tc.freq),]$tc.freq <- 0
lf.tc.freq[is.na(lf.tc.freq$Intensity),]$Intensity <- 0
lf.freq <- aggregate(land.clean$`Total Times of Landfall` ~ land.clean$Year
                     + land.clean$`Landfall Province` + 
                       land.clean$`Landfall Intensity Category`, land.clean, sum)
names(lf.freq)<- c("Year", "Province", "Intensity", "lf.freq")
lf.freq<- merge(lf.temp, lf.freq, all=TRUE)
lf.freq[is.na(lf.freq$lf.freq),]$lf.freq <- 0
lf.freq[is.na(lf.freq$Intensity),]$Intensity <- 0
lf.freq <- merge(lf.freq, lf.tc.freq, all = TRUE)


## a.3 lf.annual.tc.numbers
lf.tc.an.freq<- aggregate(lf.freq$tc.freq, 
                          by = list(Year=(lf.freq$Year)), 
                          FUN=sum)
lf.tc.an.prov.freq<- aggregate(tc.freq ~ Year + Province, lf.freq, sum)
## TS+
lf.TS <- lf.freq[lf.freq$Intensity>=2,]
lf.TS<- merge(lf.temp, lf.TS, all=TRUE)
lf.TS[is.na(lf.TS)] <- 0
lf.ts.an.freq<- aggregate(lf.TS$tc.freq,
                          by = list(Year=(lf.TS$Year)),
                          FUN=sum)
lf.ts.an.prov.freq<-aggregate(tc.freq ~ Year + Province, lf.TS, sum)
## TY+
lf.TY <- lf.freq[lf.freq$Intensity>=4,]
lf.TY<- merge(lf.temp, lf.TY, all=TRUE)
lf.TY[is.na(lf.TY)] <- 0
lf.ty.an.freq<- aggregate(lf.TY$tc.freq,
                          by = list(Year=(lf.TY$Year)),
                          FUN=sum)
lf.ty.an.prov.freq<-aggregate(tc.freq ~ Year + Province, lf.TY, sum)
## SuperTY+
lf.SuperTY <- lf.freq[lf.freq$Intensity>=6,]
lf.SuperTY<- merge(lf.temp, lf.SuperTY, all=TRUE)
lf.SuperTY[is.na(lf.SuperTY)] <- 0
lf.SuperTY.an.freq<- aggregate(lf.SuperTY$tc.freq,
                          by = list(Year=(lf.SuperTY$Year)),
                          FUN=sum)
lf.SuperTY.an.prov.freq<-aggregate(tc.freq ~ Year + Province, lf.SuperTY, sum)

## Combine into one dataframe
DT.lf.tc<- data.frame(lf.tc.an.freq,lf.ts.an.freq$x, lf.ty.an.freq$x, lf.SuperTY.an.freq$x)
names(DT.lf.tc)<-c("Year", "ALL", "TS+","TY+","SuperTY")


## b. Plotting
## b.1 Distribution
plot.new()
par(mfrow=c(2,2))
plot(density(lf.tc.an.freq$x),
     main = "Kernel Density of annual TC numbers making landfalls, 1949-2016",
     cex.main=0.8)
plot(density(lf.ts.an.freq$x),
     main = "Kernel Density of annual TS numbers making landfalls, 1949-2016",
     cex.main=0.8)
plot(density(lf.ty.an.freq$x),
     main = "Kernel Density of annual Typhoon numbers making landfalls, 1949-2016",
     cex.main=0.8)
plot(density(lf.SuperTY.an.freq$x),
     main = "Kernel Density of annual SuperTyphoon numbers making landfalls, 1949-2016",
     cex.main=0.8)
dev.copy(pdf,"Kernel Distribution of annual TC making lf.pdf",width=8,height=6)
dev.off()
par(mfrow=c(1,1))
## b.2 acf & pacf
plot.new()
par(mfrow=c(2,2))
acf(lf.tc.an.freq$x,lag.max=20)
acf(lf.ts.an.freq$x, lag.max = 20)
acf(lf.ty.an.freq$x,lag.max=20)
acf(lf.SuperTY.an.freq$x,lag.max=20)
dev.copy(pdf,"acf of annual TC making lf.pdf",width=8,height=6)
dev.off()
# We can't say there's any evidance for the serial correlation
# No significant autocorrelation at any lag
par(mfrow=c(1,1))
## b.3 Time Series plot
plot.new()
plot(lf.tc.an.freq$Year,lf.tc.an.freq$x,  xlab="Year", ylab= "Number of Tropical Cyclones making landfall",
     main="Annual Number of TC making landfall, 1949-2016", type = "l")
lo <- loess(lf.tc.an.freq$x ~ lf.tc.an.freq$Year)
lines(lf.tc.an.freq$Year, lo$fitted, col="blue")
dev.copy(pdf,"Annual Number of TC making landfall.pdf",width=8,height=6)
dev.off()
## b.4 Categorized by intensity
plot.new()
DT.lf.tc %>%
  gather(key,value,"ALL", "TS+" , "TY+", "SuperTY") %>%
  ggplot(aes(x=Year, y=value, colour=key)) +
  geom_line() + 
  ggtitle("Annual Number of TS+,TY+ and SuperTY, 1949-2016") +
  xlab("Year 1949-2016") + ylab("Frequency")+ 
  geom_smooth(method = "loess", se=FALSE,size = 1) +
  theme(plot.title = element_text(size = rel(1.5)))
dev.copy(pdf,"ALL_TC_Freq_by_Int lf.pdf",width=8,height=6)
dev.off()

## b.5 By province - xyplot
plot.new()
xyplot(lf.freq$tc.freq ~ lf.freq$Year | lf.freq$Province, 
       type="l",xlab="Year",ylab="Total times of TC making landfall",
       main="Annual Frequency of TC making landfall for different provinces")
dev.copy(pdf,"Frequency of TC_provinces.pdf",width=8,height=6)
dev.off()
plot.new()
xyplot(lf.ts.an.prov.freq$tc.freq ~ lf.ts.an.prov.freq$Year| lf.ts.an.prov.freq$Province,
       type="l",xlab="Year",ylab="Total times of TS making landfall",
       main="Annual Frequency of TS making landfall for different provinces")
dev.copy(pdf,"Frequency of TS_provinces.pdf",width=8,height=6)
dev.off()
plot.new()
xyplot(lf.ty.an.prov.freq$tc.freq ~ lf.ty.an.prov.freq$Year| lf.ty.an.prov.freq$Province,
       type="l",xlab="Year",ylab="Total times of TY making landfall",
       main="Annual Frequency of TY making landfall for different provinces")
dev.copy(pdf,"Frequency of TY_provinces.pdf",width=8,height=6)
dev.off()
plot.new()
xyplot(lf.SuperTY.an.prov.freq$tc.freq ~ lf.SuperTY.an.prov.freq$Year| lf.SuperTY.an.prov.freq$Province,
       type="l",xlab="Year",ylab="Total times of SuperTY making landfall",
       main="Annual Frequency of SuperTY making landfall for different provinces")
dev.copy(pdf,"Frequency of SuperTY_provinces.pdf",width=8,height=6)
dev.off()
## b.6 By Province - ALL TC
## Total Landfall
lf.tc.an.prov.freq$od.prov<-reorder(lf.tc.an.prov.freq$Province, lf.tc.an.prov.freq$tc.freq)
plot.new()
p <-  ggplot(data=lf.tc.an.prov.freq, aes(od.prov, Year)) + 
  geom_tile(aes(fill = tc.freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Provinces",y = "Year", title = "Annual Frequency of TC making Landfall 1949-2016 by Provinces")

p + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_TC_prov_total.pdf",width=8,height=6)
dev.off()

## TS+
lf.ts.an.prov.freq$od.prov<-reorder(lf.ts.an.prov.freq$Province, lf.ts.an.prov.freq$tc.freq)
plot.new()
p1 <-  ggplot(data=lf.ts.an.prov.freq, aes(od.prov, Year)) + 
  geom_tile(aes(fill = tc.freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Provinces",y = "Year", title = "Annual Frequency of Tropical Storms making Landfall 1949-2016 by Provinces")

p1 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_TS_prov.pdf",width=8,height=6)
dev.off()
## TY+
lf.ty.an.prov.freq$od.prov<-reorder(lf.ty.an.prov.freq$Province, lf.ty.an.prov.freq$tc.freq)
plot.new()
p2 <-  ggplot(data=lf.ty.an.prov.freq, aes(od.prov, Year)) + 
  geom_tile(aes(fill = tc.freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Provinces",y = "Year", title = "Annual Frequency of Typhoons making Landfall 1949-2016 by Provinces")

p2 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_TY_prov.pdf",width=8,height=6)
dev.off()
## SuperTY
lf.SuperTY.an.prov.freq$od.prov<-reorder(lf.SuperTY.an.prov.freq$Province, lf.SuperTY.an.prov.freq$tc.freq)
plot.new()
p3 <-  ggplot(data=lf.SuperTY.an.prov.freq, aes(od.prov, Year)) + 
  geom_tile(aes(fill = tc.freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Provinces",y = "Year", title = "Annual Frequency of Super Typhoons making Landfall 1949-2016 by Provinces")

p3 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_SuperTY_prov.pdf",width=8,height=6)
dev.off()

## Part 2: CMABST Data
## 1. Reconstructing Data
BST.clean<- CMABST
BST.clean[BST.clean$Intensity==9,]$Intensity <- 0
BST.temp <- data.frame(Year=rep(1949:2016, each=12), 
                       Month= rep(c(1:12), 68))

BST.freq <- data.frame(Year=BST.clean$Year,
                       Month=BST.clean$Month,
                       Intensity = BST.clean$Intensity,
                       SN=BST.clean$SerialNum)
BST.freq$Month <- as.integer(BST.freq$Month)

BST.SN.freq<- aggregate(Intensity ~ SN + Year, BST.freq, max)
BST.SN.freq$freq<- rep(1, length(BST.SN.freq$Year))

new.event<- c(TRUE, BST.clean$SerialNum[-1] != BST.clean$SerialNum[-nrow(BST.clean)])

BST.events<- data.frame(Year = BST.clean[new.event,]$Year,
                        Month = BST.clean[new.event,]$Month,
                        Date = BST.clean[new.event,]$Date,
                        SN = BST.clean[new.event,]$SerialNum,
                        Latitude = BST.clean[new.event,]$Latitude,
                        Longitude = BST.clean[new.event,]$Longitude)
BST.events$Month<- as.integer(BST.events$Month)

BST.freq<- merge(BST.SN.freq, BST.events)
BST.freq<- merge(BST.temp, BST.freq, all=TRUE)
BST.freq[is.na(BST.freq$freq),]$freq <- 0
BST.freq[is.na(BST.freq$Intensity),]$Intensity <- 0


## annual frequency TC and monthly TC
BST.annual<- aggregate(BST.freq$freq, by=list(Year=BST.freq$Year), sum)
names(BST.annual)<- c("Year", "Frequency")
BST.monthly <- aggregate(BST.freq$freq, by=list(Month=BST.freq$Month), sum)

## Tropical Storm (TS+)
BST.TS <- BST.freq[BST.freq$Intensity>=2,]
BST.TS<- merge(BST.TS, BST.temp, all=TRUE)
BST.TS[is.na(BST.TS$freq),]$freq <- 0
BST.TS[is.na(BST.TS$Intensity),]$Intensity <- 0
BST.an.TS <- aggregate(BST.TS$freq, by=list(Year=BST.TS$Year), sum)
BST.mon.TS <- aggregate(freq ~ Year + Month, BST.TS, sum)

## Typhoons (TY+)
BST.TY <- BST.freq[BST.freq$Intensity>=4,]
BST.TY <- merge(BST.TY, BST.temp, all=TRUE)
BST.TY[is.na(BST.TY$freq),]$freq <- 0
BST.TY[is.na(BST.TY$Intensity),]$Intensity <- 0
BST.an.TY <- aggregate(BST.TY$freq, by=list(Year=BST.TY$Year), sum)
BST.mon.TY <- aggregate(freq ~ Year + Month, BST.TY, sum)

## SuperTyphoons (superTY)
BST.SuperTY <- BST.freq[BST.freq$Intensity>=6,]
BST.SuperTY<- merge(BST.SuperTY, BST.temp, all=TRUE)
BST.SuperTY[is.na(BST.SuperTY$freq),]$freq <- 0
BST.SuperTY[is.na(BST.SuperTY$Intensity),]$Intensity <- 0
BST.an.SuperTY <- aggregate(BST.SuperTY$freq, by=list(Year=BST.SuperTY$Year), sum)
BST.mon.SuperTY <- aggregate(freq ~ Year + Month, BST.SuperTY, sum)

DT.BST.an<- data.frame(BST.annual, BST.an.TS$x, BST.an.TY$x, BST.an.SuperTY$x)
names(DT.BST.an)<-c("Year","ALL", "TS+","TY+","SuperTY")



## b. plot
plot.new()
## b.1 Seasonality
plot(BST.monthly$Month,BST.monthly$x, type="l",
     main = "Frequency of monthly TC numbers, 1949-2016",
     xlab= "Month",
     ylab = "Frequency")
dev.copy(pdf,"Seasonality of TC NWP.pdf",width=8,height=6)
dev.off()
## b.2 Distribution of annual TC numbers, 1949-2016
plot.new()
par(mfrow=c(2,2))
plot(density(BST.annual$Frequency),
     main="Kernel Density of annual TC in Northwestern Pacific basin, 1949-2016"
     ,cex.main =0.8)
plot(density(BST.an.TS$x),
     main="Kernel Density of annual TS+ in Northwestern Pacific basin, 1949-2016"
     ,cex.main =0.8)
plot(density(BST.an.TY$x),
     main="Kernel Density of annual TY+ in Northwestern Pacific basin, 1949-2016"
     ,cex.main =0.8)
plot(density(BST.an.SuperTY$x),
     main="Kernel Density of annual SuperTY in Northwestern Pacific basin, 1949-2016"
     ,cex.main =0.8)
dev.copy(pdf,"Kernel Distribution of annual TC in NWP.pdf",width=8,height=6)
dev.off()
## b.3 ACF PACF
par(mfrow=c(1,3))
acf(BST.an.TS$x, lag.max = 20, main="Autocorrelation for annual number of TS+", xlab="Lag(year)")
acf(BST.an.TY$x,lag.max=20,main="Autocorrelation for annual number of TY+", xlab="Lag(year)")
acf(BST.an.SuperTY$x,lag.max=20, main="Autocorrelation for annual number of SuperTY+", xlab="Lag(year)")
dev.copy(pdf,"NWP_ACF.pdf",width=12,height=4)
dev.off()
par(mfrow=c(1,1))
## b.4 Time Series Plot
plot.new()
plot(BST.annual$Year,BST.annual$Frequency,  
     xlab="Year", 
     ylab= "Total times of TC",
     main="Annual Number of TC in NWP, 1949-2016", type = "l")
lo <- loess(BST.annual$Frequency ~ BST.annual$Year)
lines(BST.annual$Year, lo$fitted, col="blue")
dev.copy(pdf,"total times of TC in NWP by year.pdf",width=8,height=6)
dev.off()
## Categorize by intensity
plot.new()
DT.BST.an %>%
  gather(key,value, "TS+" , "TY+", "SuperTY") %>%
  ggplot(aes(x=Year, y=value, colour=key)) +
  geom_line() + 
  ggtitle("Annual Number of TS+,TY+ and SuperTY in NWP, 1949-2016") +
  xlab("Year 1949-2016") + ylab("Frequency")+ 
  geom_smooth(method = "loess", se=FALSE,size = 1) +
  theme(plot.title = element_text(size = rel(1.5)))
dev.copy(pdf,"BST_NWP_Freq_Int by year.pdf",width=8,height=6)
dev.off()

## By month
plot.new()
xyplot(BST.mon.TS$freq ~ BST.mon.TS$Year | as.factor(BST.mon.TS$Month), 
       type="l",xlab="Year",ylab="Total times of Tropical Storms",
       main="Annual Frequency of TS+ for different months")
dev.copy(pdf,"Frequency of TS_months.pdf",width=8,height=6)
dev.off()
plot.new()
xyplot(BST.mon.TY$freq ~ BST.mon.TY$Year | as.factor(BST.mon.TY$Month), 
       type="l",xlab="Year",ylab="Total times of Typhoons",
       main="Annual Frequency of TY+ for different months")
dev.copy(pdf,"Frequency of TY_months.pdf",width=8,height=6)
dev.off()
plot.new()
xyplot(BST.mon.SuperTY$freq ~ BST.mon.SuperTY$Year | as.factor(BST.mon.SuperTY$Month), 
       type="l",xlab="Year",ylab="Total times of Super Typhoons",
       main="Annual Frequency of SuperTY for different months")
dev.copy(pdf,"Frequency of SuperTY_months.pdf",width=8,height=6)
dev.off()

## Part 4: Heatmap

## TS+
BST.mon.TS$od.prov<-reorder(BST.mon.TS$Month, BST.mon.TS$freq)
plot.new()
p1 <-  ggplot(data=BST.mon.TS, aes(od.prov, Year)) + 
  geom_tile(aes(fill = freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Months",y = "Year", title = "Annual Frequency of Tropical Storms in 1949-2016 of different months")

p1 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_TS_total.pdf",width=8,height=6)
dev.off()

## TY+
BST.mon.TY$od.prov<-reorder(BST.mon.TY$Month, BST.mon.TY$freq)
plot.new()
p2 <-  ggplot(data=BST.mon.TY, aes(od.prov, Year)) + 
  geom_tile(aes(fill = freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Months",y = "Year", title = "Annual Frequency of Typhoons in 1949-2016 of different months")

p2 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_TY_total.pdf",width=8,height=6)
dev.off()

## SuperTyphoon
BST.mon.SuperTY$od.prov<-reorder(BST.mon.SuperTY$Month, BST.mon.SuperTY$freq)
plot.new()
p3 <-  ggplot(data=BST.mon.SuperTY, aes(od.prov, Year)) + 
  geom_tile(aes(fill = freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Months",y = "Year", title = "Annual Frequency of SuperTyphoons in 1949-2016 of different months")

p3 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_SuperTyphoon_total.pdf",width=8,height=6)
dev.off()

## By year

## TS+
BST.mon.TS$od.year<-reorder(BST.mon.TS$Year, BST.mon.TS$freq)
plot.new()
p5 <-  ggplot(data=BST.mon.TS, aes(Year, Month)) + 
  geom_tile(aes(fill = freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Year",y = "Month", title = "Annual Frequency of Tropical Storms+ in 1949-2016")

p5 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 90,hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_TS_total_Year.pdf",width=8,height=6)
dev.off()

## TY+
BST.mon.TY$od.year<-reorder(BST.mon.TY$Year, BST.mon.TY$freq)
plot.new()
p6 <-  ggplot(data=BST.mon.TY, aes(Year, Month)) + 
  geom_tile(aes(fill = freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Year",y = "Month", title = "Annual Frequency of Typhoons+ in 1949-2016")

p6 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 90,hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_TY_total_Year.pdf",width=8,height=6)
dev.off()

## SuperTY
BST.mon.SuperTY$od.year<-reorder(BST.mon.SuperTY$Year, BST.mon.SuperTY$freq)
plot.new()
p7 <-  ggplot(data=BST.mon.SuperTY, aes(Year, Month)) + 
  geom_tile(aes(fill = freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Year",y = "Month", title = "Annual Frequency of SuperTY+ in 1949-2016")

p7 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 90,hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_SuperTY_total_Year.pdf",width=8,height=6)
dev.off()
