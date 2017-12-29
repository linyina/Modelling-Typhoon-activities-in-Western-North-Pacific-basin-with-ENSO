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
## a. Reconstruct data
## Landfall Frequency - by year

Landfall[is.na(Landfall$`Total Times of Landfall`),]$`Total Times of Landfall` <- 0
lf.freq<-aggregate(Landfall$`Total Times of Landfall`, 
                   by=list(Year=(Landfall$Year)), FUN=sum) #sum by year
names(lf.freq)<-c("Year", "Frequency")

## Landfall Frequency - by province
lf.freq.prov<- data.frame(Year=rep(1949:2016, each=15), 
                          Province= rep(levels(Landfall$`Landfall Province`), 68))

temp.DT<- data.frame(Year=Landfall$Year,
                     Province=Landfall$`Landfall Province`,
                     frequency=Landfall$`Total Times of Landfall`)
lf.freq.prov<- merge(lf.freq.prov, temp.DT, all=TRUE)
lf.freq.prov[is.na(lf.freq.prov$frequency),]$frequency <- 0

lf.freq.prov<-aggregate(lf.freq.prov$frequency ~ lf.freq.prov$Year
                        + lf.freq.prov$Province, lf.freq.prov, sum)
names(lf.freq.prov)<- c("Year", "Province", "Frequency")

lf.freq.prov<- lf.freq.prov[!(lf.freq.prov$Province==""),]

## Landfall - Intensity
## Reconstructing the Intensity category
land.clean<-Landfall
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

land.clean$`Landfall Province` <- factor(land.clean$`Landfall Province`, levels = c("Fujian" , "Guangdong", "Guangxi" , "Hainan" 
                                                                                    ,"Hong Kong" , "Jiangsu", "Liaoning", "Shandong", "Shanghai","Shanghai:Zhejiang" 
                                                                                    ,"Taiwan" ,"Tianjin","Zhejiang" , "Zhejiang:Fujian"))
levels(land.clean$`Landfall Province`)
int.temp<- data.frame(Year=rep(1949:2016, each=14), 
                      Province= rep(levels(land.clean$`Landfall Province`), 68))

## Tropical Storm +
lf.TS<- land.clean[land.clean$`Landfall Intensity Category`>=2,]
lf.TS<- data.frame(Year=lf.TS$Year,
                   Province= lf.TS$`Landfall Province`,
                   Intensity= lf.TS$`Landfall Intensity Category`,
                   Frequency= lf.TS$`Total Times of Landfall`)
lf.TS<- merge(int.temp, lf.TS, all=TRUE)
lf.TS[is.na(lf.TS$Frequency),]$Frequency <- 0
lf.TS.plus<- aggregate(lf.TS$Frequency, 
                       by=list(Year=(lf.TS$Year)), FUN=sum)
names(lf.TS.plus)<- c("Year", "Frequency")
lf.prov.TS.plus<- aggregate(lf.TS$Frequency ~ lf.TS$Year
                            + lf.TS$Province, lf.TS, sum)
names(lf.prov.TS.plus)<- c("Year", "Province", "Frequency")

## Typhoon +
lf.TY<- land.clean[land.clean$`Landfall Intensity Category`>=4,]
lf.TY<- data.frame(Year=lf.TY$Year,
                   Province= lf.TY$`Landfall Province`,
                   Intensity= lf.TY$`Landfall Intensity Category`,
                   Frequency= lf.TY$`Total Times of Landfall`)
lf.TY<- merge(int.temp, lf.TY, all=TRUE)
lf.TY[is.na(lf.TY$Frequency),]$Frequency <- 0
lf.TY.plus<- aggregate(lf.TY$Frequency, 
                       by=list(Year=(lf.TY$Year)), FUN=sum)
names(lf.TY.plus)<- c("Year", "Frequency")
lf.prov.TY.plus<- aggregate(lf.TY$Frequency ~ lf.TY$Year
                            + lf.TY$Province, lf.TY, sum)
names(lf.prov.TY.plus)<- c("Year", "Province", "Frequency")


## Super Typhoon  
lf.superTY<-land.clean[land.clean$`Landfall Intensity Category`>=6,]
lf.superTY<- data.frame(Year=lf.superTY$Year,
                        Province= lf.superTY$`Landfall Province`,
                        Intensity= lf.superTY$`Landfall Intensity Category`,
                        Frequency= lf.superTY$`Total Times of Landfall`)
lf.superTY<- merge(int.temp, lf.superTY, all=TRUE)
lf.superTY[is.na(lf.superTY$Frequency),]$Frequency <- 0
lf.superTY.plus<- aggregate(lf.superTY$Frequency, 
                            by=list(Year=(lf.superTY$Year)), FUN=sum)
names(lf.superTY.plus)<- c("Year", "Frequency")
lf.prov.superTY.plus<- aggregate(lf.superTY$Frequency ~ lf.superTY$Year
                                 + lf.superTY$Province, lf.superTY, sum)
names(lf.prov.superTY.plus)<- c("Year", "Province", "Frequency")

## Combine into one dataframe
DT.int<- data.frame(lf.TS.plus, lf.TY.plus$Frequency, lf.superTY.plus$Frequency)
names(DT.int)<-c("Year", "TS+","TY+","SuperTY")

## b. Distribution of annual landfall

plot.new()
par(mfrow=c(2,2))
## Distribution of annual TC numbers, 1949-2016
hist(lf.freq$Frequency, 
     main = "Distribution of annual landfall numbers, 1949-2016",
     cex.main =0.8,
     xlab = "Number of TC",
     ylab= "Frequency")
hist(lf.TS.plus$Frequency, 
     main = "Distribution of annual Tropical Storms numbers, 1949-2016",
     cex.main =0.8,
     xlab = "Number of TS",
     ylab= "Frequency")
hist(lf.TY.plus$Frequency, 
     main = "Distribution of annual Typhoons numbers, 1949-2016",
     cex.main =0.8,
     xlab = "Number of TY",
     ylab= "Frequency")
hist(lf.superTY.plus$Frequency, 
     main = "Distribution of annual SuperTyphoons numbers, 1949-2016",
     cex.main =0.8,
     xlab = "Number of SuperTY",
     ylab= "Frequency")
dev.copy(pdf,"Distribution of annual landfall.pdf",width=8,height=6)
dev.off()

## Kernel Density Plots
plot.new()
par(mfrow=c(2,2))
plot(density(lf.freq$Frequency), 
     main="Kernel Density of annual landfalls, 1949-2016"
     ,cex.main =0.8)
plot(density(lf.TS.plus$Frequency), 
     main="Kernel Density of annual Tropical Storms +, 1949-2016"
     ,cex.main =0.8)
plot(density(lf.TY.plus$Frequency), 
     main="Kernel Density of annual Typhoons +, 1949-2016"
     ,cex.main =0.8)
plot(density(lf.superTY.plus$Frequency), 
     main="Kernel Density of annual SuperTyphoons, 1949-2016"
     ,cex.main =0.8)
dev.copy(pdf,"Kernel Distribution of annual landfall.pdf",width=8,height=6)
dev.off()

par(mfrow=c(1,1))

## c. time series plot
## Q1: Does the frequency in landfall/ severe typhoon increase each year?
plot.new()
plot(lf.freq$Year,lf.freq$Frequency,  xlab="Year", ylab= "Total times of Landfall",
     main="Annual Number of landfall, 1949-2016", type = "l")
lo <- loess(lf.freq$Frequency ~ lf.freq$Year)
lines(lf.freq$Year, lo$fitted, col="blue")
dev.copy(pdf,"total times of landfall by year.pdf",width=8,height=6)
dev.off()

## Find out the year with maximum frequencies
lf.freq[lf.freq$Frequency==max(lf.freq$Frequency),]$Year


## Q2: Categorize by intensity
plot.new()
DT.int %>%
  gather(key,value, "TS+" , "TY+", "SuperTY") %>%
  ggplot(aes(x=Year, y=value, colour=key)) +
  geom_line() + 
  ggtitle("Annual Number of TS+,TY+ and SuperTY, 1949-2016") +
  xlab("Year 1949-2016") + ylab("Frequency")+ 
  geom_smooth(method = "loess", se=FALSE,size = 1) +
  theme(plot.title = element_text(size = rel(1.5)))
dev.copy(pdf,"LF_Freq_Int by year.pdf",width=8,height=6)
dev.off()

## Q3: Serial Correaltion? ACF and PACF

acf(lf.freq$Frequency,lag.max=20)
acf(lf.TS.plus$Frequency, lag.max = 20)
acf(lf.TY.plus$Frequency,lag.max=20)
acf(lf.superTY.plus$Frequency,lag.max=20)
# We can't say there's any evidance for the serial correlation
# No significant autocorrelation at any lag
pacf(lf.freq$Frequency,lag.max=20)
pacf(lf.TS.plus$Frequency,lag.max=20)
pacf(lf.TY.plus$Frequency,lag.max=20)
pacf(lf.superTY.plus$Frequency,lag.max=20)
# They suggest that the count from the previous years 
# is not important when modelling the number of storms

## Part 2: By province
## a. Total times of landfall
plot.new()
xyplot(lf.freq.prov$Frequency ~ lf.freq.prov$Year | lf.freq.prov$Province, 
       type="l",xlab="Year",ylab="Total times of landfall",
       main="Annual Frequency of landfall for different provinces")
dev.copy(pdf,"Frequency of landfall_provinces.pdf",width=8,height=6)
dev.off()
## b. By intensity
## Part 4: Heatmap
## Total Landfall
lf.freq.prov$od.prov<-reorder(lf.freq.prov$Province, lf.freq.prov$Frequency)
plot.new()
p <-  ggplot(data=lf.freq.prov, aes(od.prov, Year)) + 
  geom_tile(aes(fill = Frequency), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Provinces",y = "Year", title = "Annual Frequency of Landfall 1949-2016 by Provinces")

p + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_landfall_total.pdf",width=8,height=6)
dev.off()

## TS+
lf.prov.TS.plus$od.prov<-reorder(lf.prov.TS.plus$Province, lf.prov.TS.plus$Frequency)
plot.new()
p1 <-  ggplot(data=lf.prov.TS.plus, aes(od.prov, Year)) + 
  geom_tile(aes(fill = Frequency), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Provinces",y = "Year", title = "Annual Frequency of Landfall achieving TS 1949-2016 by Provinces")

p1 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_landfall_TS.pdf",width=8,height=6)
dev.off()

## TY+
lf.prov.TY.plus$od.prov<-reorder(lf.prov.TY.plus$Province, lf.prov.TY.plus$Frequency)
plot.new()
p2 <-  ggplot(data=lf.prov.TY.plus, aes(od.prov, Year)) + 
  geom_tile(aes(fill = Frequency), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Provinces",y = "Year", title = "Annual Frequency of Landfall achieving TY 1949-2016 by Provinces")

p2 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_landfall_TY.pdf",width=8,height=6)
dev.off()
## SuperTY
lf.prov.superTY.plus$od.prov<-reorder(lf.prov.superTY.plus$Province, lf.prov.superTY.plus$Frequency)
plot.new()
p3 <-  ggplot(data=lf.prov.superTY.plus, aes(od.prov, Year)) + 
  geom_tile(aes(fill = Frequency), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Provinces",y = "Year", title = "Annual Frequency of Landfall achieving superTY 1949-2016 by Provinces")

p3 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_landfall_superTY.pdf",width=8,height=6)
dev.off()



## Part 3: CMABST
## a. Reconstructing the data
## 
BST.int<- data.frame(Year = CMABST$Year, 
                     Month = CMABST$Month,
                     SN= substr(CMABST$SerialNum, start = 6, stop = 7),
                     Intensity = CMABST$Intensity,
                     WND = CMABST$WND,
                     Freq = rep(1, length(CMABST$Year)))
BST.int[BST.int$Intensity==9,]$Intensity <- NA
BST.int<-na.omit(BST.int)
BST.an.freq<-aggregate(BST.int$Freq, by=list(Year=(BST.int$Year)), FUN=sum)
names(BST.an.freq)<- c("Year", "Frequency")
BST.an.freq$Year<- as.numeric(levels(BST.an.freq$Year))
BST.mon.freq<-aggregate(BST.int$Freq, by=list(Month=(BST.int$Month)), FUN=sum)
names(BST.mon.freq)<- c("Month", "Frequency")
BST.mon.int<- aggregate(BST.int$Freq ~ BST.int$Year
                        + BST.int$Month + BST.int$Intensity, BST.int, sum)
BST.temp<- data.frame(Year=rep(1949:2016, each=12), 
                      Month= rep(c("01","02","03","04","05","06","07","08","09","10","11","12"), 68))
names(BST.mon.int)<-c("Year", "Month", "Intensity", "Freq")
### TS+
####
BST.TS <- BST.mon.int[BST.mon.int$Intensity>=2,]
names(BST.TS) <- c("Year", "Month", "Intensity", "Freq")
BST.mon.TS<- merge(BST.TS, BST.temp, all=TRUE)
BST.mon.TS[is.na(BST.mon.TS$Freq),]$Freq <- 0
BST.an.TS<- aggregate(BST.mon.TS$Freq, by= list(Year=(BST.mon.TS$Year)), FUN=sum)
### TY+
BST.TY <- BST.mon.int[BST.mon.int$Intensity>=4,]
names(BST.TS) <- c("Year", "Month", "Intensity", "Freq")
BST.mon.TY<- merge(BST.TY, BST.temp, all=TRUE)
BST.mon.TY[is.na(BST.mon.TY$Freq),]$Freq <- 0
BST.an.TY<- aggregate(BST.mon.TY$Freq, by= list(Year=(BST.mon.TY$Year)), FUN=sum)
### SuperTY+
BST.SuperTY <- BST.mon.int[BST.mon.int$Intensity>=6,]
names(BST.SuperTY) <- c("Year", "Month", "Intensity", "Freq")
BST.mon.SuperTY<- merge(BST.SuperTY, BST.temp, all=TRUE)
BST.mon.SuperTY[is.na(BST.mon.SuperTY$Freq),]$Freq <- 0
BST.an.SuperTY<- aggregate(BST.mon.SuperTY$Freq, by= list(Year=(BST.mon.SuperTY$Year)), FUN=sum)

DT.BST.an.int<- data.frame(BST.an.TS, BST.an.TY$x, BST.an.SuperTY$x)
names(DT.BST.an.int)<-c("Year", "TS+","TY+","SuperTY")
DT.BST.an.int$Year<- as.numeric(levels(DT.BST.an.int$Year))

## b. plots
######
# 1. Distribution
plot.new()
## 1.1 Seasonality
plot(BST.mon.freq$Frequency, type = "l",
     main = "Frequency of monthly TC numbers, 1949-2016",
     xlab= "Month",
     ylab = "Frequency")
dev.copy(pdf,"Seasonality of TC NWP.pdf",width=8,height=6)
dev.off()

## 1.2 Distribution of annual TC numbers, 1949-2016
par(mfrow=c(2,2))
plot(density(BST.an.freq$Frequency),
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

## 1.3 ACF PACF
par(mfrow=c(2,2))
acf(BST.an.freq$Frequency,lag.max=20)  # Strong lag
acf(BST.an.TS$x, lag.max = 20)
acf(BST.an.TY$x,lag.max=20)
acf(BST.an.SuperTY$x,lag.max=20)
dev.copy(pdf,"NWP_ACF.pdf",width=8,height=6)
dev.off()


pacf(BST.an.freq$Frequency,lag.max=20)
pacf(BST.an.TS$x,lag.max=20)
pacf(BST.an.TY$x,lag.max=20)
pacf(BST.an.SuperTY$x,lag.max=20)
dev.copy(pdf,"NWP_PACF.pdf",width=8,height=6)
dev.off()
## Not strong evidence?

# 2. Time series
par(mfrow=c(1,1))
plot.new()

plot(BST.an.freq$Year,BST.an.freq$Frequency,  
     xlab="Year", 
     ylab= "Total times of TC",
     main="Annual Number of TC in NWP, 1949-2016", type = "l")
lo <- loess(BST.an.freq$Frequency ~ BST.an.freq$Year)
lines(BST.an.freq$Year, lo$fitted, col="blue")
dev.copy(pdf,"total times of TC in NWP by year.pdf",width=8,height=6)
dev.off()

## 2.2 Categorize by intensity
plot.new()
DT.BST.an.int %>%
  gather(key,value, "TS+" , "TY+", "SuperTY") %>%
  ggplot(aes(x=Year, y=value, colour=key)) +
  geom_line() + 
  ggtitle("Annual Number of TS+,TY+ and SuperTY in NWP, 1949-2016") +
  xlab("Year 1949-2016") + ylab("Frequency")+ 
  geom_smooth(method = "loess", se=FALSE,size = 1) +
  theme(plot.title = element_text(size = rel(1.5)))
dev.copy(pdf,"BST_NWP_Freq_Int by year.pdf",width=8,height=6)
dev.off()

