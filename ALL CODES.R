####################################################################################
###        This is the complete code script for STAT3901 Project on modelling the 
###        Typhoon activities in Western North Pacific Basin.
###        Author: Miss. Yina Lin
###        Supervisor: Prof. Richard Chandler
####################################################################################

############################################################
####   STEP 1. Reading and recoding the data            ####
####                                                    ####
####   Aim: (1) Reading all the files                   ####
####        (2) Tidy the data                           ####
############################################################

#### A. - CMABST
#### 1.1 Reading

# Using list.files to read all the data
library(foreign)
file.list <- list.files(path = "./Data/CMABST", full.names=TRUE)

# Constructing a dataframe
BST.all.data <- data.frame()

for (current.files in file.list){
  
  # Read THE data
  tempData <- read.table(current.files, header=FALSE,  fill = T, sep="")
  # Combine the data in one dataframe
  BST.all.data <- rbind(BST.all.data, tempData)
  
}




## 1.2 Cleaning the data
##
## i Replacing
##
## a. Name 

a = 0
b = 1

BST.all.data[,8]=replace(BST.all.data[,8],BST.all.data[,8]=="",NA)

while (a < 68514){
  a = a + 1
  
  if (is.na(BST.all.data[a,8]) == TRUE){
    
    BST.all.data[a,8] <- BST.all.data[b,8]
    
  }
  else { b = a }
}

## b. Serial Number

BST.all.data <- within(BST.all.data, SerialNum<- BST.all.data[,4])
# wanted.data <- BST.all.data[,1] == 66666
# summary(BST.all.data$V4[wanted.data])


a = 0
b = 1

BST.all.data$SerialNum=replace(BST.all.data$SerialNum,BST.all.data$SerialNum>54,NA)

while (a < 68514){
  a = a + 1
  
  if (is.na(BST.all.data[a,10]) == TRUE){
    
    BST.all.data[a,10] <- BST.all.data[b,10]
    
  }
  else { b = a }
}



## Save the data.frame into file
## save(BST.all.data, file="ALL_CMABST.Rda")
## write.table(BST.all.data, "BST_All_Data.dat", row.names = FALSE)

## 1.2.2  Clean the headers

Head.line <- 1
i <- 0
BST.clean <- BST.all.data

while ( Head.line <= 68514 ){
  
  BST.clean <- BST.clean[- (Head.line - i),]
  Head.line <- Head.line + BST.all.data[Head.line, 3] + 1
  i <- i+1
  
}

## write.table(BST.clean, "BST_no_header.dat", row.names = FALSE)


## 1.2.3  Create the new data.frame

Year <- substr(BST.clean[,1], start = 1, stop = 4)
Month <- substr(BST.clean[,1], start = 5, stop = 6)
Date <- substr(BST.clean[,1], start = 7, stop = 8)
Hour <- substr(BST.clean[,1], start = 9, stop = 10)
Name <- as.character(BST.clean[,8])
Intensity <- BST.clean[,2]
Latitude <- BST.clean[,3]
Longitude <- BST.clean[,4]
Pres <- BST.clean[,5]
WND <- BST.clean[,6]
OWD <- BST.clean[,7]
SerialNum <- BST.clean$SerialNum
CMABST <- data.frame(Year, Month, Date, Hour,Name, SerialNum, Intensity, Latitude, Longitude, Pres, WND, OWD)

#### Format cleaning
CMABST$Latitude <- CMABST$Latitude/10
CMABST$Longitude <- CMABST$Longitude/10

summary(CMABST$Latitude)
summary(CMABST$Longitude)

CMABST$SerialNum = paste0(Year, "-",SerialNum,sep = "")

#### B - Landfall

Landfall <- read.csv("./Data/Landfall.csv")
names(Landfall) <- c("Year", "Serial Number", "Chinese ID", "Name", 
                     "Total Times of Landfall", "Order of landfall", "Landfall Province", 
                     "Landfall Intensity Category")
a = 0
b = 1
while (a <= 814){
  a = a + 1
  
  if (is.na(Landfall[a,1]) == TRUE){
    
    Landfall[a,1] <- Landfall[b,1]
    
  }
  else { b = a }
}

Landfall<- Landfall[-815,]

## Rename the level to clean the strange symbols
levels(Landfall$`Landfall Province`)[11] <- "Shanghai:Zhejiang"
levels(Landfall$`Landfall Province`)[15] <- "Zhejiang:Fujian"

#################################################
#### Step 2-1 Map
#################################################
######
## summary(CMABST$Latitude)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.50   14.10   19.10   20.72   25.60   60.50 
## summary(CMABST$Longitude)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 95.0   122.1   132.7   134.4   145.1   226.0 
## ylim = c(0,61), xlim = c(90, 230)
######
library(maps)
library(mapdata)
library(lattice)

if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)

map('worldHires', c('China','Taiwan','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                    'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230),
    mar = c(4.1, 4.1, par("mar")[3], 0.1))

wanted.rows <- CMABST$SerialNum == "1949-1"
lines(CMABST$Longitude[wanted.rows],CMABST$Latitude[wanted.rows],col="blue")
lines(CMABST$Longitude[CMABST$SerialNum == "1949-2"], CMABST$Latitude[CMABST$SerialNum == "1949-2"], col= "blue")

## Define a function of drawing maps to make things easier
plot.single <- function(x){
  if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)
  par(mar=c(5,6,4,2))
  map('worldHires', c('China','Taiwan','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                      'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230),
      mar = c(4.1, 4.1, par("mar")[3], 0.1))
  lines(x$Longitude, x$Latitude, col= rgb(0,0,0,alpha=0.3))
}

plot.single(CMABST[wanted.rows,])

## a. Transparent lines for all the data on the map

Serial.list <- levels(as.factor(CMABST$SerialNum))
i = 0

map('worldHires', c('China','Taiwan','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                    'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230),
    mar = c(4.1, 4.1, par("mar")[3], 0.1))

while (i <=2245){
  i = i+1
  wanted.data <- CMABST$SerialNum == Serial.list[i]
  lines(CMABST$Longitude[wanted.data],CMABST$Latitude[wanted.data], col= rgb(0,0,0,alpha=0.1))
}

## Using function to make things easier

plot.track<- function(x){
  par(mar=c(5,6,4,2))
  plot.list <- levels(as.factor(x$SerialNum))
  map('worldHires', c('China','Taiwan','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                      'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230),
      mar = c(4.1, 4.1, par("mar")[3], 0.1))
  
  i = 0
  
  while (i <=2245){
    i = i+1
    wanted.data <- x$SerialNum == plot.list[i]
    lines(CMABST$Longitude[wanted.data],CMABST$Latitude[wanted.data], col= rgb(0,0,0,alpha=0.1))
  }
  
}

plot.new()
plot.track(CMABST)
title(main="CMA tropical Cyclone tracks from 1949-2016")
dev.copy(pdf,"CMABST_all_tracks_b&w.pdf",width=8,height=6)
dev.off()

## b. Try by decades
wanted.dec1 <- CMABST[Year<=1958,]
wanted.dec2 <- CMABST[(Year>1958) & (Year<=1968),]
wanted.dec3 <- CMABST[(Year>1968) & (Year<=1978),]
wanted.dec4 <- CMABST[(Year>1978) & (Year<=1988),]
wanted.dec5 <- CMABST[(Year>1988) & (Year<=1998),]
wanted.dec6 <- CMABST[(Year>1998) & (Year<=2008),]
wanted.dec7 <- CMABST[(Year>2008) & (Year<=2018),]


plot.track(wanted.dec1) # 1949-1958
plot.track(wanted.dec2) # 1959-1968
plot.track(wanted.dec3) # 1969-1978
plot.track(wanted.dec4) # 1979-1988
plot.track(wanted.dec5) # 1989-1998
plot.track(wanted.dec6) # 1999-2008
plot.track(wanted.dec7) # 2008-2016


## Try by years
plot.track(CMABST[CMABST$Year==2016,])


## c. By Intensity
col.scale<- rev(c(grey(0,alpha = 0.3), NA, NA, heat.colors(7, alpha = 0.4)))
plot.cols<- function(x){
  par(mar=c(5,6,4,2))
  map('worldHires', c('China','Taiwan','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                      'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230),
      mar = c(4.1, 4.1, par("mar")[3], 0.1))
  points(x$Longitude[CMABST$Intensity==0], x$Latitude[CMABST$Intensity==0], col= col.scale[CMABST$Intensity + 1], pch=4)
  points(x$Longitude[CMABST$Intensity==1], x$Latitude[CMABST$Intensity==1], col= col.scale[CMABST$Intensity + 1], pch=4)
  points(x$Longitude[CMABST$Intensity==2], x$Latitude[CMABST$Intensity==2], col= col.scale[CMABST$Intensity + 1], pch=4)
  points(x$Longitude[CMABST$Intensity==3], x$Latitude[CMABST$Intensity==3], col= col.scale[CMABST$Intensity + 1], pch=18)
  points(x$Longitude[CMABST$Intensity==4], x$Latitude[CMABST$Intensity==4], col= col.scale[CMABST$Intensity + 1], pch=20)
  points(x$Longitude[CMABST$Intensity==5], x$Latitude[CMABST$Intensity==5], col= col.scale[CMABST$Intensity + 1], pch=19)
  points(x$Longitude[CMABST$Intensity==6], x$Latitude[CMABST$Intensity==6], col= col.scale[CMABST$Intensity + 1], pch=17)
  points(x$Longitude[CMABST$Intensity==9], x$Latitude[CMABST$Intensity==9], col= col.scale[CMABST$Intensity + 1], pch=1)
  legend("topright", inset=.001, title="Intensity Category", c("<TD","TD","TS","STS","TY","STY","SuperTY","ET"), 
         col = col.scale,
         pch=c(4,4,18,20,20,19,17,1),text.width = 3,cex=0.8,horiz=FALSE)
}
plot.cols(CMABST[wanted.rows,])
plot.cols(CMABST)
title(main="CMA tropical Cyclone tracks from 1949-2016")
dev.copy(pdf,"CMABST_all_tracks_col.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec1)
dev.copy(pdf,"CMABST_1949_1958.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec2)
dev.copy(pdf,"CMABST_1959_1968.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec3)
dev.copy(pdf,"CMABST_1969_1978.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec4)
title(main="CMA tropical Cyclone tracks from 1979-1988")
dev.copy(pdf,"CMABST_1979_1988.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec5)
title(main="CMA tropical Cyclone tracks from 1989-1998")
dev.copy(pdf,"CMABST_1989_1998.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec6)
title(main="CMA tropical Cyclone tracks from 1999-2008")
dev.copy(pdf,"CMABST_1999_2008.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec7)
title(main="CMA tropical Cyclone tracks from 2008-2016")
dev.copy(pdf,"CMABST_2009_2016.pdf",width=8,height=6)
dev.off()


#######################################################
####  Step 2-2 Exploratory Analysis
####
#######################################################
library(lattice)   # heatmap and xyplot
library(ggplot2)   # time series plot
library(MASS)
library(tidyr)  # time series plot
library(RColorBrewer) # Colours
library(reshape2) # melt
########################
#if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)
#quartz(height = 6, width = 8)
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
par(mfrow=c(2,2))
acf(BST.annual$Frequency,lag.max=20)  # Strong lag
acf(BST.an.TS$x, lag.max = 20)
acf(BST.an.TY$x,lag.max=20)
acf(BST.an.SuperTY$x,lag.max=20)
dev.copy(pdf,"NWP_ACF.pdf",width=8,height=6)
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
p5 <-  ggplot(data=BST.mon.TS, aes(Year, as.factor(Month))) + 
  geom_tile(aes(fill = freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Year",y = "Month", title = "Monthly Frequency of Tropical Storms+ in 1949-2016")

p5 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 90,hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_TS_total_Year.pdf",width=8,height=6)
dev.off()

## TY+
BST.mon.TY$od.year<-reorder(BST.mon.TY$Year, BST.mon.TY$freq)
plot.new()
p6 <-  ggplot(data=BST.mon.TY, aes(Year, as.factor(Month))) + 
  geom_tile(aes(fill = freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Year",y = "Month", title = "Monthly Frequency of Typhoons+ in 1949-2016")

p6 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 90,hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_TY_total_Year.pdf",width=8,height=6)
dev.off()

## SuperTY
BST.mon.SuperTY$od.year<-reorder(BST.mon.SuperTY$Year, BST.mon.SuperTY$freq)
plot.new()
p7 <-  ggplot(data=BST.mon.SuperTY, aes(Year, as.factor(Month))) + 
  geom_tile(aes(fill = freq), colour = "white") +
  scale_fill_gradient(low = "#FFFF80CC",high = "#FF0000CC")+
  labs(x = "Year",y = "Month", title = "Monthly Frequency of SuperTY+ in 1949-2016")

p7 + theme(plot.title = element_text(size = rel(1.2))) + theme(axis.text.x = element_text(angle = 90,hjust = 1, colour = "grey50"))
dev.copy(pdf,"Heatmap_SuperTY_total_Year.pdf",width=8,height=6)
dev.off()


############################
### Step3: Modelling
###########################

### 3-1: Predictors

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


### 3-2: Monthly Data construction
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
                  TNI = SST$TNI)
BST.ana<- merge(Nino, BST.mon.data, sort = FALSE)
str(BST.ana)


# write.table(BST.ana, "BST_analysis_data.dat", row.names = FALSE)


############################################
#### 3-3 Deseasonalize to find time lag ####
########################
## Step 1: Find the cross correlation between SST and the TS/TY/SuperTY
########################

## CCF:ts
ccf(BST.ana$NINO3.4,BST.ana$TS.freq)
plot(ccf(BST.ana$NINO3.4,BST.ana$TS.freq))
## CCF:ty
ccf(BST.ana$NINO3.4, BST.ana$TY.freq)
plot(ccf(BST.ana$NINO3.4, BST.ana$TY.freq))

ccf(BST.ana$NINO3.4, BST.ana$SuperTY.freq)
plot(ccf(BST.ana$NINO3.4, BST.ana$SuperTY.freq))

par(mfrow=c(3,1))
ccf(BST.ana$NINO3.4,BST.ana$TS.freq, main="CCFs for monthly average of Nino3.4 and frequency of TS+", xlab="Lag(Month)")
ccf(BST.ana$NINO3.4, BST.ana$TY.freq, main="CCFs for monthly average of Nino3.4 and frequency of TY+", xlab="Lag(Month)")
ccf(BST.ana$NINO3.4, BST.ana$SuperTY.freq, main="CCFs for monthly average of Nino3.4 and frequency of SuperTY", xlab="Lag(Month)")
dev.copy(pdf,"CCF before deseasonalize.pdf",width=8,height=6)
dev.off()
par(mfrow=c(1,1))

########################
## Step 2: It is quiet possible that the correlation is due to seasonality from the plot
##         so we should try the deseasonalized ccf to find proper lag
##         Method for deseasonalized - linear models - treat months as factor covariates
########################

seas.sst<- lm(BST.ana$NINO3.4 ~ as.factor(BST.ana$Month))
sst.des<- summary(seas.sst)$residual
seas.ts <- lm(BST.ana$TS.freq ~ as.factor(BST.ana$Month))
ts.des<- summary(seas.ts)$residual
seas.ty <- lm(BST.ana$TY.freq ~ as.factor(BST.ana$Month))
ty.des<- summary(seas.ty)$residual
seas.sty<- lm(BST.ana$SuperTY.freq ~ as.factor(BST.ana$Month))
superty.des<- summary(seas.sty)$residual
seas.tni<- lm(BST.ana$TNI ~ as.factor(BST.ana$Month))
sst.tni <- summary(seas.tni)$residual

#######################
## Step 3: CCF after deseasonalize

## sst.des vs. TS.des
print(ccf(sst.des, ts.des))
plot(ccf(sst.des, ts.des))  # lag -5

BST.ana$sst.lag.5<- c(rep(NA,5), BST.ana$NINO3.4[1:415])
BST.ana$TNI.lag.5<- c(rep(NA,5), BST.ana$TNI[1:415])

## sst.des vs. TY.des
print(ccf(sst.des, ty.des))
plot(ccf(sst.des, ty.des))
acf(sst.des)

## sst.des vs. superty.des
print(ccf(sst.des, superty.des))
plot(ccf(sst.des, superty.des)) # lag -1


BST.ana$sst.lag.1 <- c(NA, BST.ana$NINO3.4[1:419])
BST.ana$TNI.lag.1<- c(NA, BST.ana$TNI[1:419])

par(mfrow=c(3,1))
ccf(sst.des, ts.des, main="CCFs for deseasonalized monthly average of Nino3.4 and frequency of TS+", xlab="Lag(Month)")
ccf(sst.des, ty.des, main="CCFs for deseasonalized monthly average of Nino3.4 and frequency of TY+", xlab="Lag(Month)")
ccf(sst.des, superty.des, main="CCFs for deseasonalized monthly average of Nino3.4 and frequency of SuperTY", xlab="Lag(Month)")
dev.copy(pdf,"CCF after deseasonalize.pdf",width=8,height=6)
dev.off()
par(mfrow=c(1,1))

boxplot(BST.ana$NINO3.4~BST.ana$Month, xlab="Month",ylab="Degree")
###########################
### 3-4 Building monthly models  - poisson glm
###########################
library(DAAG)
quartz(height = 6, width = 8)
### Step 1. Create the dummy variables for representing the seasonality

BST.ana$dummy.p1 <- cos(pi*BST.ana$Month /6)
BST.ana$dummy.p2 <- sin(pi*BST.ana$Month /6)
str(BST.ana)
### Step 2. Before conducting time lag - using nino 3.4

BST.glm.ts.0<- glm(TS.freq ~ NINO3.4, data=BST.ana[6:420,],family=poisson(link = log))
summary(BST.glm.ts.0)
### 1: Using dummy seasonal covariates - cosine function
BST.glm.ts.1<- glm(TS.freq ~ NINO3.4 + dummy.p1 + dummy.p2, data=BST.ana[6:420,],family=poisson(link = log))
summary(BST.glm.ts.1)
anova(BST.glm.ts.0, BST.glm.ts.1, test="Chi")
BST.glm.ty.1<- glm(TY.freq ~ NINO3.4 + dummy.p1 + dummy.p2, data=BST.ana,family=poisson(link = log))
summary(BST.glm.ty.1)
BST.glm.superty.1<- glm(SuperTY.freq ~ NINO3.4 + dummy.p1 + dummy.p2, data=BST.ana[2:420,],family=poisson(link = log))
summary(BST.glm.superty.1)

### 1a: Adding TNI
BST.glm.ts.1a <- update(BST.glm.ts.1, .~. + TNI)
summary(BST.glm.ts.1a)
anova(BST.glm.ts.1, BST.glm.ts.1a, test = "Chi") # Better!
BST.glm.ty.1a<- update(BST.glm.ty.1, .~. + TNI)
summary(BST.glm.ty.1a)
anova(BST.glm.ty.1, BST.glm.ty.1a, test = "Chi") # Better!
BST.glm.superty.1a<- update(BST.glm.superty.1, .~. + TNI)
summary(BST.glm.superty.1a)
anova(BST.glm.superty.1, BST.glm.superty.1a, test = "Chi")

### 2: Using lagged version

BST.glm.ts.2<- glm(TS.freq ~ sst.lag.5 + TNI.lag.5+ dummy.p1 + dummy.p2, 
                   data=BST.ana[6:420,],family=poisson(link = log))
summary(BST.glm.ts.2)
anova(BST.glm.ts.1a, BST.glm.ts.2, test="Chi") # Slightly better

BST.glm.superty.2<- glm(SuperTY.freq ~ sst.lag.1 + TNI.lag.1+dummy.p1 + dummy.p2, 
                        data=BST.ana[2:420,],family=poisson(link = log))
summary(BST.glm.superty.2)
anova(BST.glm.superty.1a, BST.glm.superty.2, test="Chi") #Slightly better

### Step 3: interactions
BST.glm.ts.3<- update(BST.glm.ts.2, .~.+ sst.lag.5:dummy.p1 + sst.lag.5:dummy.p2)
summary(BST.glm.ts.3)
anova(BST.glm.ts.2, BST.glm.ts.3, test="Chi") # Good!
BST.glm.ts.3a<- update(BST.glm.ts.3, .~. + TNI.lag.5:dummy.p1 + TNI.lag.5:dummy.p2)
summary(BST.glm.ts.3a)
anova(BST.glm.ts.3, BST.glm.ts.3a, test="Chi") # Not significant!
BST.glm.ts.3b<- update(BST.glm.ts.3, .~. + TNI.lag.5:sst.lag.5)
summary(BST.glm.ts.3b)
anova(BST.glm.ts.3, BST.glm.ts.3b, test="Chi") 

BST.glm.ty.3<- update(BST.glm.ty.1a, .~.+ NINO3.4:dummy.p1 + NINO3.4:dummy.p2)
summary(BST.glm.ty.3)
anova(BST.glm.ty.1a, BST.glm.ty.3, test="Chi") # Not significant!
BST.glm.ty.3a<- update(BST.glm.ty.1a, .~. + TNI:dummy.p1 + TNI:dummy.p2)
summary(BST.glm.ty.3a)
anova(BST.glm.ty.1a, BST.glm.ty.3a, test="Chi") # Not significant!


BST.glm.superty.3<- update(BST.glm.superty.2, .~.+ 
                             sst.lag.1:dummy.p1 + sst.lag.1:dummy.p2)
summary(BST.glm.superty.3)
anova(BST.glm.superty.2, BST.glm.superty.3, test="Chi") # Good!
BST.glm.superty.3a<- update(BST.glm.superty.3, .~. + 
                              TNI.lag.1:dummy.p1 + TNI.lag.1:dummy.p2)
summary(BST.glm.superty.3a)
anova(BST.glm.superty.3, BST.glm.superty.3a, test="Chi") # Not significant!


####### Final model checking
disp(BST.glm.ts.1) # underdispersion
disp(BST.glm.ts.1a)
disp(BST.glm.ts.2)
disp(BST.glm.ts.3)# underdispersion
disp(BST.glm.ts.3a)
disp(BST.glm.ty.3)
disp(BST.glm.superty.3)
drop1(BST.glm.ts.3)
drop1(BST.glm.ty.3)
drop1(BST.glm.superty.3)

dev.res(BST.glm.ts.3)
dev.res(BST.glm.ty.3)
dev.res(BST.glm.superty.3)

anova(BST.glm.ts.3, test="Chi")


### Step 4: Com-Poisson - dealing with underdispersion

library(COMPoissonReg)
summary(BST.glm.ts.3)
BST.glm.ts.4 <- glm.cmp(TS.freq ~ sst.lag.5 + TNI.lag.5 + dummy.p1 + dummy.p2 + 
                          sst.lag.5:dummy.p1 + sst.lag.5:dummy.p2, 
                        data=BST.ana[6:420,])
summary(BST.glm.ts.4)
print("The COM-Poisson estimates for the beta vector are") 
print(coef(BST.glm.ts.4))
print("The COM-Poisson estimate for the dispersion parameter nu is")
print(nu(BST.glm.ts.4))
print("The associated standard errors for the betas in the constant dispersion case are")
print(sdev(BST.glm.ts.4))

summary(BST.glm.ty.3)
BST.glm.ty.4<- glm.cmp(TY.freq ~ NINO3.4 + dummy.p1 + dummy.p2 + TNI + 
                         NINO3.4:dummy.p1 + NINO3.4:dummy.p2,
                       data=BST.ana)

summary(BST.glm.ty.4)
print("The COM-Poisson estimates for the beta vector are") 
print(coef(BST.glm.ty.4))
print("The COM-Poisson estimate for the dispersion parameter nu is")
print(nu(BST.glm.ty.4))
print("The associated standard errors for the betas in the constant dispersion case are")
print(sdev(BST.glm.ty.4))

summary(BST.glm.superty.3)
BST.glm.superty.4<- glm.cmp(SuperTY.freq ~ sst.lag.1 + TNI.lag.1 + dummy.p1 + 
                              dummy.p2 + sst.lag.1:dummy.p1 + sst.lag.1:dummy.p2, 
                            data = BST.ana[2:420, ])
summary(BST.glm.superty.4)
print("The COM-Poisson estimates for the beta vector are") 
print(coef(BST.glm.superty.4))
print("The COM-Poisson estimate for the dispersion parameter nu is")
print(nu(BST.glm.superty.4))
print("The associated standard errors for the betas in the constant dispersion case are")
print(sdev(BST.glm.superty.4))


par(mfrow=c(1,3))
cook.plot(BST.glm.ts.2)
cook.plot(BST.glm.ty.1)
cook.plot(BST.glm.superty.2)
cook.plot(BST.glm.ts.3)
cook.plot(BST.glm.ty.3)
cook.plot(BST.glm.superty.3)

acf(BST.ana$TS.freq, main="ACF for TS", xlab="Lag(month)")
acf(BST.ana$NINO3.4, main="ACF for SST", xlab="Lag(month)")
acf(BST.ana$TNI, main="ACF for TNI", xlab="Lag(month)")

############################
### Step 4: Daily Modelling
############################
### 1. Reconstructing data
### Producing a calendar dataframe from 1982 to 2016
date<- seq(as.Date("1982-01-01"), as.Date("2016-12-31"), by="days")
df.daily<- data.frame(date=date,
                      Month=substr(date,6,7))
df.daily$Month<- as.numeric(df.daily$Month)
BST.date <- merge(BST.SN.freq[BST.SN.freq$Intensity>=2,], BST.events)     ## can do the events for Tropical cyclone + by filter the bst.sn.freq[Intensity>=2,]
BST.date <- BST.date[942:1834,]

BST.date$date <- as.Date(paste0(BST.date$Year,"-",BST.date$Month, "-",BST.date$Date))

BST.date <- merge(df.daily, BST.date, all=TRUE)

BST.date[is.na(BST.date$freq),]$freq<- 0
BST.date$Year <- substr(BST.date$date, start = 1, stop= 4)
BST.date$Date <- substr(BST.date$date, start = 9, stop= 10)
BST.date[is.na(BST.date$Intensity),]$Intensity<- 0
events <- aggregate(freq ~ date, BST.date, sum)
events.binary <- aggregate(freq ~ date, BST.date, max)

BST.daily <- data.frame(Date = events$date,
                        Year= substr(events$date, start = 1, stop= 4),
                        Month = substr(events$date, start = 6, stop= 7),
                        Day = substr(events$date, start = 9, stop= 10),
                        events.freq = events$freq,
                        events.lag.1 = c(0, events.binary$freq[1:12783]),
                        events.lag.2 = c(0,0, events.binary$freq[1:12782]))

BST.daily$Month <- as.integer(BST.daily$Month)



########

Nino34 <- data.frame(Year= BST.ana$Year,
                     Month=BST.ana$Month, 
                     Nino3.4= BST.ana$NINO3.4,
                     Nino3.4Lag5=BST.ana$sst.lag.5,
                     Nino3.4Lag1=BST.ana$sst.lag.1,
                     TNI= BST.ana$TNI,
                     TNILag5=BST.ana$TNI.lag.5,
                     TNILag1=BST.ana$TNI.lag.1)

BST.daily <- merge(Nino34,BST.daily, sort = FALSE)

DaysinMonth <- c(31,29,31,30,31,30,31,31,30,31,30,31)
Cumdays <- c(0, cumsum(DaysinMonth[-12]))
BST.daily$dayofyear <- Cumdays[BST.daily$Month] + as.integer(BST.daily$Day)
BST.daily$dummy.p1 <- cos(2*pi*BST.daily$dayofyear / 366)
BST.daily$dummy.p2 <- sin(2*pi*BST.daily$dayofyear / 366)

#######################################################################################
#####                   Step 4-2 Modelling on the daily data                      #####
#####                                                                             #####
##### Consider storm occurence on a daily basis. Let there be a probability p of  #####
##### observing a storm on any given day. If events on different days are indepe- #####
##### ent => the number of storms in a month will follow Bin (30,p) => This can   #####
##### be approximated by a Poisson distribution with mean 30p.                    #####
#######################################################################################

### Forward selection!
### First, conduct a model without seasonality
BST.daily.glm.0 <- glm(events.freq ~ Nino3.4 + events.lag.1 + events.lag.2,
                       data= BST.daily, family = poisson(link = "log"))
summary(BST.daily.glm.0)
anova(BST.daily.glm.0, test="Chi")

BST.daily.glm.0a<- update(BST.daily.glm.0, .~. + TNI)
anova(BST.daily.glm.0, BST.daily.glm.0a,test="Chi") # GOOD!

### Add seasonality dummy variables in it.
BST.daily.glm.1 <- update(BST.daily.glm.0a, .~. + dummy.p1+dummy.p2)
summary(BST.daily.glm.1)
anova(BST.daily.glm.0a, BST.daily.glm.1, test="Chi")  # Not surprisingly
disp(BST.daily.glm.1)  # Doing quiet well
cook.plot(BST.daily.glm.1)
dev.copy(pdf,"II_glm1_cookplot.pdf", height=6, width=8)
dev.off()
dev.res(BST.daily.glm.1)
plot(BST.daily.glm.1)


BST.daily.glm.2 <- glm(events.freq ~ Nino3.4Lag5 + TNILag5+ dummy.p1 + dummy.p2 + events.lag.1 + events.lag.2,
                       data= BST.daily, family = poisson(link = "log"))
summary(BST.daily.glm.2)
anova(BST.daily.glm.2, test="Chi")

plot(BST.daily.glm.2)
disp(BST.daily.glm.2)
dev.res(BST.daily.glm.2)
cook.plot(BST.daily.glm.2)
dev.copy(pdf,"II_glm2_cookplot.pdf", height=6, width=8)
dev.off()

##### interactions of sst and seasonality
BST.daily.glm.3 <- update(BST.daily.glm.2, .~. + Nino3.4Lag5:dummy.p1 + Nino3.4Lag5:dummy.p2)
summary(BST.daily.glm.3)

anova(BST.daily.glm.2, BST.daily.glm.3, test = "Chi")
disp(BST.daily.glm.3)
dev.res(BST.daily.glm.3)
cook.plot(BST.daily.glm.3)
dev.copy(pdf,"II_glm3_cookplot.pdf", height=6, width=8)
dev.off()
drop1(BST.daily.glm.3)
anova(BST.daily.glm.3, test="Chi")

#############################################
##### Model improving 
############################################
##### interactions of lag1 and lag2
BST.daily.glm.4<- update(BST.daily.glm.3, .~. + events.lag.1:events.lag.2)
summary(BST.daily.glm.4)
anova(BST.daily.glm.3, BST.daily.glm.4, test = "Chi")   # no use!


##### All combinations of interactions
BST.daily.glm.5<- glm(events.freq ~ (Nino3.4Lag5 + dummy.p1 + dummy.p2 + events.lag.1 + events.lag.2 + events.lag.3 +TNILag5)^2,
                      data= BST.daily, family = poisson(link = "log"))
summary(BST.daily.glm.5)
anova(BST.daily.glm.3, BST.daily.glm.5, test = "Chi") # Uh interesting. No use
drop1(BST.daily.glm.5)  # Nothing very important...

#### interactions of seasonality and lags
BST.daily.glm.6 <- update(BST.daily.glm.3, .~. + dummy.p1:events.lag.1 + dummy.p1:events.lag.2 
                          + dummy.p2:events.lag.1 + dummy.p2:events.lag.2)
summary(BST.daily.glm.6)
anova(BST.daily.glm.3, BST.daily.glm.6, test="Chi")  # .. no use!

#### Adding lag 3
BST.daily$events.lag.3<- c(0,BST.daily$events.lag.2[1:12783])
BST.daily.glm.lag3 <- update(BST.daily.glm.3, .~. + events.lag.3)
summary(BST.daily.glm.lag3)
anova(BST.daily.glm.3, BST.daily.glm.lag3, test = "Chi") # Good!
anova(BST.daily.glm.lag3, test="Chi")
disp(BST.daily.glm.lag3)

#############################################################################################
#####                  Step 4-3 Simulation for model checking and fit                   #####    
#####                                                                                   #####
##### log(mu)= beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3 + beta4 * x4 + beta5 * x5   #####
#####         + beta6 * x1x2 + beta7* x1x3                                              #####
#####    mu <- exp(beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3 + beta4* x4 + beta5*x5  #####
#####         + beta6*x1x2 + beta7*x1x3)                                                #####
#############################################################################################
set.seed(123456)
x.day <- data.frame(BST.daily[-(1:151), - (11:13)])   # Remove the NA for SST lag
x.day$events.lag.1<- 0
x.day$events.lag.2<- 0
x.day$events.lag.3<- 0
pred.core <- predict(BST.daily.glm.lag3, newdata = x.day)

n <- length(x.day$Day)
n.sims <- 100
y.lag1<- rep(0, n.sims)
y.lag2<- rep(0, n.sims)
y.lag3<- rep(0, n.sims)

y.hat<- exp(pred.core[1] + coef(BST.daily.glm.lag3)[6] * y.lag1 + coef(BST.daily.glm.lag3)[7] * y.lag2
            + coef(BST.daily.glm.lag3)[8]*y.lag3)
y.sim <- array (NA, c(n, n.sims)) 
y.sim[1,] <- rpois(n.sims, y.hat)
y.lag <- array (NA, c(n,n.sims))
y.lag[1,] <- y.sim[1,]
y.lag[1,][y.lag[1,]>=1]<-1

y.lag3 <- y.lag2
y.lag2 <- y.lag1
y.lag1 <- y.lag[1,]


i = 1

while (i < n){
  i = i+1
  y.hat <- exp(pred.core[i] + coef(BST.daily.glm.lag3)[6] * y.lag1 + coef(BST.daily.glm.lag3)[7] * y.lag2
               + coef(BST.daily.glm.lag3)[8]*y.lag3)
  y.sim[i,]<- rpois(n.sims, y.hat)
  y.lag[i,]<-y.sim[i,]
  y.lag[i,][y.lag[i,]>=1]<-1
  y.lag3 <- y.lag2
  y.lag2 <- y.lag1
  y.lag1 <- y.lag[i,]
}
### OMG!!!!! THAT'S AMAZINGLY FAST!!!!!!!!!!!!!
### A-M-A-Z-I-N-G-!-!-!-! 1 SEC ONLY!!!!!!!!!!!


###### Assessment
obs.data <- BST.daily[-(1:151),]
#plot(obs.data$Date,obs.data$events.freq)
#plot(y.sim[,1])  # WOW
#head(y.sim)


sim.test <- data.frame(Date = obs.data$Date,
                       Year = obs.data$Year,
                       Month = obs.data$Month,
                       Observed = obs.data$events.freq,
                       y.sim)

test.monthly <- aggregate(Observed ~ Year + Month, data= sim.test, FUN = sum)
test.monthly <- test.monthly[order(test.monthly$Year,test.monthly$Month),]    # reorder the month and year
test.monthly.sim <- aggregate(y.sim ~ Year + Month, data= sim.test, sum)
test.monthly.sim <- test.monthly.sim[order(test.monthly.sim$Year, test.monthly.sim$Month),]
test.monthly.sim$MIN<-apply(test.monthly.sim[,3:102], 1, min)
test.monthly.sim$lower <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.25)
test.monthly.sim$median <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.50)
test.monthly.sim$upper <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.75)
test.monthly.sim$MAX <- apply(test.monthly.sim[,3:102], 1, max)
test.monthly.sim$mean <- apply(test.monthly.sim[,3:102], 1, mean)


### Monthly
col.scale<- rev( heat.colors(5, alpha =1))

test.monthly.sim.Jan_May <- test.monthly.sim[test.monthly.sim$Month%in%(1:5),]
test.monthly.sim.June_Sep <- test.monthly.sim[test.monthly.sim$Month%in%(6:9),]
test.monthly.sim.Oct_Dec<- test.monthly.sim[test.monthly.sim$Month%in%(10:12),]

par(mfrow=c(3,1))
plot(ts(test.monthly[test.monthly$Month%in%(1:5),]$Observed, frequency = 5,start = c(1983,1)),
     type="l",ylim=c(0,5), lwd=3, xlab="Year (Jan-May)",
     ylab="Number", main = "Number of new TS events (monthly) in 1982-2016")

points(ts(test.monthly.sim.Jan_May$MIN, frequency = 5, start=c(1983,1)), col=col.scale[1], type='l', lwd=10)
points(ts(test.monthly.sim.Jan_May$lower, frequency = 5, start=c(1983,1)), col=col.scale[2], type='l', lwd=10)
points(ts(test.monthly.sim.Jan_May$median, frequency = 5, start=c(1983,1)), col=col.scale[3], type='l', lwd=10)
points(ts(test.monthly.sim.Jan_May$upper, frequency = 5, start=c(1983,1)), col=col.scale[4], type='l', lwd=10)
points(ts(test.monthly.sim.Jan_May$MAX, frequency = 5, start=c(1983,1)), col=col.scale[5], type='l', lwd=10)
points(ts(test.monthly.sim.Jan_May$mean, frequency = 5, start=c(1983,1)), type="l", lwd=3, col="blue")
points(ts(test.monthly[test.monthly$Month%in%(1:5),]$Observed, frequency = 5,start = c(1983,1)),
       type="l", lwd=5)


legend("topright", inset=.001, c("Observed","Min","0.25","0.5","0.75","Max","Mean"), 
       col = c("Black",col.scale,"blue"),
       lty=1,text.width = 0.5, cex=0.4,horiz=TRUE)


plot(ts(test.monthly[test.monthly$Month%in%(6:9),]$Observed, frequency = 4,start = c(1981,5)),
     type="l",ylim=c(0,15), lwd=3, xlab="Year (June-Sep)",
     ylab="Number", main = "Number of new TS events (monthly) in 1982-2016")

points(ts(test.monthly.sim.June_Sep$MIN, frequency = 4, start=c(1981,5)), col=col.scale[1], type='l', lwd=10)
points(ts(test.monthly.sim.June_Sep$lower, frequency = 4, start=c(1981,5)), col=col.scale[2], type='l', lwd=10)
points(ts(test.monthly.sim.June_Sep$median, frequency = 4, start=c(1981,5)), col=col.scale[3], type='l', lwd=10)
points(ts(test.monthly.sim.June_Sep$upper, frequency = 4, start=c(1981,5)), col=col.scale[4], type='l', lwd=10)
points(ts(test.monthly.sim.June_Sep$MAX, frequency = 4, start=c(1981,5)), col=col.scale[5], type='l', lwd=10)
points(ts(test.monthly.sim.June_Sep$mean, frequency = 4, start=c(1981,5)), type="l", lwd=3, col="blue")
points(ts(test.monthly[test.monthly$Month%in%(6:9),]$Observed, frequency = 4,start = c(1981,5)),
       type="l", lwd=5)
legend("topright", inset=.001, c("Observed","Min","0.25","0.5","0.75","Max","Mean"), 
       col = c("Black",col.scale,"blue"),
       lty=1,text.width = 0.5, cex=0.4,horiz=TRUE)

plot(ts(test.monthly[test.monthly$Month%in%(10:12),]$Observed, frequency = 3,start = c(1981,9)),
     type="l",ylim=c(0,12), lwd=3, xlab="Year (Oct-Dec)",
     ylab="Number", main = "Number of new TS events (monthly) in 1982-2016")
points(ts(test.monthly.sim.Oct_Dec$MIN, frequency = 3,start = c(1981,9)), col=col.scale[1], type='l', lwd=10)
points(ts(test.monthly.sim.Oct_Dec$lower, frequency = 3,start = c(1981,9)), col=col.scale[2], type='l', lwd=10)
points(ts(test.monthly.sim.Oct_Dec$median, frequency = 3,start = c(1981,9)), col=col.scale[3], type='l', lwd=10)
points(ts(test.monthly.sim.Oct_Dec$upper, frequency = 3,start = c(1981,9)), col=col.scale[4], type='l', lwd=10)
points(ts(test.monthly.sim.Oct_Dec$MAX, frequency = 3,start = c(1981,9)), col=col.scale[5], type='l', lwd=10)
points(ts(test.monthly.sim.Oct_Dec$mean, frequency = 3, start=c(1981,9)), type="l", lwd=3, col="blue")
points(ts(test.monthly[test.monthly$Month%in%(10:12),]$Observed, frequency = 3,start = c(1981,9)),
       type="l", lwd=5)
legend("topright", inset=.001, c("Observed","Min","0.25","0.5","0.75","Max","Mean"), 
       col = c("Black",col.scale,"blue"),
       lty=1,text.width = 0.5, cex=0.4,horiz=TRUE)

#dev.copy(pdf,"All2_checking_monthlysum.pdf", height=6, width=8)
#dev.off()

par(mfrow=c(1,1))
### Annually
test.annually <- aggregate(sim.test[,4:104], by= list(sim.test$Year), sum)
test.annually$MIN<- apply(test.annually[,3:102], 1, min)
test.annually$lower <- apply(test.annually[,3:102], 1, quantile, probs= 0.25)
test.annually$median <- apply(test.annually[,3:102], 1, quantile, probs= 0.50)
test.annually$upper <- apply(test.annually[,3:102], 1, quantile, probs= 0.75)
test.annually$MAX <- apply(test.annually[,3:102], 1, max)
test.annually$mean <- apply(test.annually[,3:102], 1, mean)
names(test.annually)[1]<- "Year"

#### This one looks good!
plot(test.annually$Year, test.annually$Observed, type="o", lwd=5, ylim=c(10,50),xlab="Year",
     ylab="Number", main = "The annual number of TS events in 1982-2016")
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$MIN,rev(test.annually$lower)), col = col.scale[1], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$lower,rev(test.annually$median)), col = col.scale[2], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$median,rev(test.annually$upper)), col = col.scale[3], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$upper,rev(test.annually$MAX)), col = col.scale[4], border = NA)
points(test.annually$Year,test.annually$mean, type="l", lwd=3, col="blue")
points(test.annually$Year, test.annually$Observed, type="o", lwd=5)
legend("topright", inset=.001, title="Quantiles", c("Observed","Min","0.25","0.5","0.75","Max","Mean"), 
       col = c("Black",col.scale,"blue"),
       lty=1,text.width = 1,cex=0.5,horiz=FALSE)
#dev.copy(pdf,"TS2_checking_yearsum.pdf", height=6, width=8)
#dev.off()



##### probability integral tranform
pit.hist<- rowMeans(y.sim < sim.test$Observed)
hist(pit.hist)

#dev.copy(pdf,"Daily PIT.pdf",width=8,height=6)
#dev.off()

## monthly
par(mfrow=c(2,2))
pit.mon.hist<- rowMeans(test.monthly.sim[,3:102] <= test.monthly$Observed)
hist(pit.mon.hist, main="PIT of monthly simulation: All", xlab="Quantiles '<='")
pit.Jan.hist<- rowMeans(test.monthly.sim[test.monthly.sim$Month %in% (1:5),][,3:102] <= test.monthly[test.monthly$Month %in% (1:5),]$Observed)
hist(pit.Jan.hist, main = "PIT of monthly simulation: Jan-May",xlab="Quantiles '<='")
pit.June.hist<- rowMeans(test.monthly.sim[test.monthly.sim$Month %in% (6:9),][,3:102] <= test.monthly[test.monthly$Month %in% (6:9),]$Observed)
hist(pit.June.hist, main = "PIT of monthly simulation: June-Sep",xlab="Quantiles '<='")
pit.Oct.hist<- rowMeans(test.monthly.sim[test.monthly.sim$Month %in% (10:12),][,3:102] <= test.monthly[test.monthly$Month %in% (10:12),]$Observed)
hist(pit.Oct.hist, main = "PIT of monthly simulation: Oct-Dec",xlab="Quantiles '<='")
#dev.copy(pdf,"TS2 Monthly PIT.pdf",width=8,height=6)
#dev.off()
par(mfrow=c(1,1))

## annual
pit.annual.hist <- rowMeans(test.annually[,3:102] <= test.annually$Observed)
hist(pit.annual.hist, breaks = c(0,0.2,0.4,0.6,0.8,1), main = "PIT: simulations <= observation", xlab="Percentiles")
#dev.copy(pdf,"TS2 Annual PIT.pdf",width=8,height=6)
#dev.off()

##### Pt - daily

Pt <- rowMeans(y.sim > 0)

## REC
Pt.group <- cut(Pt,breaks=seq(0,1,0.05),include.lowest = TRUE, right=FALSE)
Obs.tab <- tapply(sim.test$Observed, INDEX = Pt.group, FUN=function(x) {sum(x>0)})
Exp.tab <- tapply(Pt, INDEX = Pt.group, FUN=function(x) {sum(x)})

rbind(Obs.tab,Exp.tab)

for (i in seq(0,1,0.1)){
  cat("Expected:",sum(Pt[Pt >=i & Pt < i+0.1]),"\n")
  cat("Observed:",sum(sim.test[Pt >= i & Pt < i+0.1,]$Observed>0),"\n")
}

sum(sim.test$Observed > 0)
sum(Pt)

##
##sum(Pt[Pt >= 0 & Pt < 0.1])
##sum(sim.test[Pt >= 0 & Pt < 0.1,]$Observed)
##sum(Pt[Pt >=0.1 & Pt < 0.2])
##sum(sim.test[Pt >= 0.1 & Pt < 0.2,]$Observed)
##sum(Pt[Pt >=0.2 & Pt < 0.3])
##sum(sim.test[Pt >= 0.2 & Pt < 0.3,]$Observed)
##sum(Pt[Pt >=0.3 & Pt < 0.4])
##sum(sim.test[Pt >= 0.3 & Pt < 0.4,]$Observed)
##

barplot(sim.test$Observed, beside = T)
help(barplot)
barplot(y.sim)

########### REconstructing data for intensity >=4
events.binary.2<- BST.date[BST.date$Intensity>=4,]
BST.date.2<-merge(BST.SN.freq[BST.SN.freq$Intensity>=4,], BST.events)
BST.date.2$date <- as.Date(paste0(BST.date.2$Year,"-",BST.date.2$Month, "-",BST.date.2$Date))
BST.date.2<-BST.date.2[600:1124,]
BST.date.2 <- merge(df.daily, BST.date.2, all=TRUE)
BST.date.2[is.na(BST.date.2$freq),]$freq<- 0
BST.date.2$Year <- substr(BST.date.2$date, start = 1, stop= 4)
BST.date.2$Date <- substr(BST.date.2$date, start = 9, stop= 10)
events.2 <- aggregate(freq ~ date, BST.date.2, sum)
events.binary.2 <- aggregate(freq ~ date, BST.date.2, max)

BST.daily.2<- data.frame(Date = events.2$date,
                         Year= substr(events.2$date, start = 1, stop= 4),
                         Month = substr(events.2$date, start = 6, stop= 7),
                         Day = substr(events.2$date, start = 9, stop= 10),
                         events.freq = events.2$freq,
                         events.lag.1 = c(0, events.binary.2$freq[1:12783]),
                         events.lag.2 = c(0,0, events.binary.2$freq[1:12782]))
BST.daily.2$Month <- as.integer(BST.daily.2$Month)
BST.daily.2 <- merge(Nino34,BST.daily.2, sort = FALSE)
BST.daily.2$dayofyear <- Cumdays[BST.daily.2$Month] + as.integer(BST.daily.2$Day)
BST.daily.2$dummy.p1 <- cos(2*pi*BST.daily.2$dayofyear / 366)
BST.daily.2$dummy.p2 <- sin(2*pi*BST.daily.2$dayofyear / 366)


############## Modelling again
BST.daily2.glm0<-glm(formula = events.freq ~ Nino3.4 + TNI + dummy.p1 + dummy.p2 + 
                       events.lag.1 + events.lag.2, family = poisson(link = "log"), 
                     data = BST.daily.2)
summary(BST.daily2.glm0)
### Adding nino3.4Lag5 and dummy.p1 interaction
BST.daily2.glm1<-update(BST.daily2.glm0,.~. + Nino3.4:dummy.p1 + Nino3.4:dummy.p2)
summary(BST.daily2.glm1)
anova(BST.daily2.glm0, BST.daily2.glm1, test = "Chi")

## Combination of all interactions
BST.daily2.glm2<-glm(events.freq ~ (Nino3.4 + TNI+dummy.p1 + dummy.p2 + events.lag.1 + events.lag.2)^2,
                     data= BST.daily.2, family = poisson(link = "log"))
summary(BST.daily2.glm2)
anova(BST.daily2.glm1, BST.daily2.glm2, test = "Chi") # Still No use
drop1(BST.daily2.glm2)  # No use



#### Adding lag 3
BST.daily.2$events.lag.3<- c(0,BST.daily.2$events.lag.2[1:12783])
BST.daily2.glm3 <- update(BST.daily2.glm1, .~. + events.lag.3)
summary(BST.daily2.glm3)
anova(BST.daily2.glm1, BST.daily2.glm3, test = "Chi") # good!


####### lag 3 added

x.day <- data.frame(BST.daily.2[-(1:151), (1:8)])   # Remove the NA for SST lag
x.day$dummy.p1 <- BST.daily.2$dummy.p1[-(1:151)]
x.day$dummy.p2 <- BST.daily.2$dummy.p2[-(1:151)]
x.day$events.lag.1<- 0
x.day$events.lag.2<- 0
x.day$events.lag.3<- 0

pred.core <- predict(BST.daily2.glm3, newdata = x.day)

n <- length(x.day$Day)
n.sims <- 100
y.lag1<- rep(0, n.sims)
y.lag2<- rep(0, n.sims)
y.lag3<- rep(0, n.sims)

y.hat<- exp(pred.core[1] + coef(BST.daily2.glm3)[6] * y.lag1 + coef(BST.daily2.glm3)[7] * y.lag2 +
              coef(BST.daily2.glm3)[8] * y.lag3)
y.sim <- array (NA, c(n, n.sims)) 
y.sim[1,] <- rpois(n.sims, y.hat)
y.lag <- array (NA, c(n,n.sims))
y.lag[1,] <- y.sim[1,]
y.lag[1,][y.lag[1,]>=1]<-1
y.lag3 <- y.lag2
y.lag2 <- y.lag1
y.lag1 <- y.sim[1,]


i = 1

while (i < n){
  i = i+1
  y.hat <- exp(pred.core[i] + coef(BST.daily2.glm3)[6] * y.lag1 + coef(BST.daily2.glm3)[7] * y.lag2+
                 coef(BST.daily2.glm3)[8] * y.lag3)
  y.sim[i,]<- rpois(n.sims, y.hat)
  y.lag[i,]<- y.sim[i,]
  y.lag[i,][y.lag[i,]>=1]<-1
  y.lag3 <- y.lag2
  y.lag2 <- y.lag1
  y.lag1 <- y.sim[i,]
}



###### Assessment
obs.data <- BST.daily.2[-(1:151),]
plot(obs.data$Date,obs.data$events.freq)
plot(y.sim[,1])  # WOW
head(y.sim)


sim.test <- data.frame(Date = obs.data$Date,
                       Year = obs.data$Year,
                       Month = obs.data$Month,
                       Observed = obs.data$events.freq,
                       y.sim)

test.monthly <- aggregate(Observed ~ Year + Month, data= sim.test, FUN = sum)
test.monthly <- test.monthly[order(test.monthly$Year,test.monthly$Month),]    # reorder the month and year
test.monthly.sim <- aggregate(y.sim ~ Year + Month, data= sim.test, sum)
test.monthly.sim <- test.monthly.sim[order(test.monthly.sim$Year, test.monthly.sim$Month),]
test.monthly.sim$MIN<-apply(test.monthly.sim[,3:102], 1, min)
test.monthly.sim$lower <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.25)
test.monthly.sim$median <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.50)
test.monthly.sim$upper <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.75)
test.monthly.sim$MAX <- apply(test.monthly.sim[,3:102], 1, max)
test.monthly.sim$mean <- apply(test.monthly.sim[,3:102], 1, mean)

### Monthly
col.scale<- rev( heat.colors(5, alpha =1))
plot(ts(test.monthly$Observed, frequency = 12,start = c(1982,6)),type="l",ylim=c(0,15), lwd=5, xlab="Year",
     ylab="Number of days", main = "Number of days (monthly) that has a new TS event in 1982-2016")
polygon(ts(test.monthly.sim$MIN, frequency = 12,start = c(1982,6)), 
        col=col.scale[1], border = FALSE)
polygon(ts(test.monthly.sim$lower, frequency = 12,start = c(1982,6)), 
        col=col.scale[2], border = FALSE)
polygon(ts(test.monthly.sim$median, frequency = 12,start = c(1982,6)), 
        col=col.scale[3], border = FALSE)
polygon(ts(test.monthly.sim$upper, frequency = 12,start = c(1982,6)), 
        col=col.scale[4], border = FALSE)
polygon(ts(test.monthly.sim$MAX, frequency = 12,start = c(1982,6)), 
        col=col.scale[5], border = FALSE)
points(ts(test.monthly$Observed, frequency = 12,start = c(1982,6)),type="b", lwd=3)
#dev.copy(pdf,"TS3_checking_monthlysum.pdf", height=6, width=8)
#dev.off()

### Annually
test.annually <- aggregate(sim.test[,4:104], by= list(sim.test$Year), sum)
test.annually$MIN<- apply(test.annually[,3:102], 1, min)
test.annually$lower <- apply(test.annually[,3:102], 1, quantile, probs= 0.25)
test.annually$median <- apply(test.annually[,3:102], 1, quantile, probs= 0.50)
test.annually$upper <- apply(test.annually[,3:102], 1, quantile, probs= 0.75)
test.annually$MAX <- apply(test.annually[,3:102], 1, max)
test.annually$mean <- apply(test.annually[,3:102], 1, mean)
names(test.annually)[1]<- "Year"



#### This one looks good!
plot(test.annually$Year, test.annually$Observed, type="o", lwd=5, ylim=c(0,30),xlab="Year",
     ylab="Number of days", main = "Number of days that has a new TY event in 1982-2016")
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$MIN,rev(test.annually$lower)), col = col.scale[1], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$lower,rev(test.annually$median)), col = col.scale[2], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$median,rev(test.annually$upper)), col = col.scale[3], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$upper,rev(test.annually$MAX)), col = col.scale[4], border = NA)
points(test.annually$Year,test.annually$mean, type="l", lwd=3, col="blue")
points(test.annually$Year, test.annually$Observed, type="o", lwd=5)
legend("topright", inset=.001, title="Quantiles", c("Observed","Min","0.25","0.5","0.75","Max","Mean"), 
       col = c("Black",col.scale,"blue"),
       lty=1,text.width = 0.5,cex=0.4,horiz=FALSE)
dev.copy(pdf,"TY3_checking_yearsum.pdf", height=6, width=8)
dev.off()



##### probability integral tranform
pit.hist<- rowMeans(y.sim < sim.test$Observed)
hist(pit.hist)

dev.copy(pdf,"Daily PIT.pdf",width=8,height=6)
dev.off()

## monthly
#pit.mon.hist<- rowMeans(test.monthly.sim[,3:102] < test.monthly$Observed)
#hist(pit.mon.hist)

pit.mon.hist<- rowMeans(test.monthly.sim[test.monthly.sim$Month %in% (6:11),][,3:102] < test.monthly[test.monthly$Month %in% (6:11),]$Observed)
hist(pit.mon.hist)

dev.copy(pdf,"TS3 Monthly PIT.pdf",width=8,height=6)
dev.off()

## annual
pit.annual.hist <- rowMeans(test.annually[,3:102] < test.annually$Observed)
hist(pit.annual.hist, breaks = c(0,0.2,0.4,0.6,0.8,1))
dev.copy(pdf,"TS3 Annual PIT.pdf",width=8,height=6)
dev.off()

##### Pt - daily

Pt <- rowMeans(y.sim > 0)

## REC
Pt.group <- cut(Pt,breaks=seq(0,1,0.05),include.lowest = TRUE, right=FALSE)
Obs.tab <- tapply(sim.test$Observed, INDEX = Pt.group, FUN=function(x) {sum(x>0)})
Exp.tab <- tapply(Pt, INDEX = Pt.group, FUN=function(x) {sum(x)})

rbind(Obs.tab,Exp.tab)

for (i in seq(0,1,0.1)){
  cat("Expected:",sum(Pt[Pt >=i & Pt < i+0.1]),"\n")
  cat("Observed:",sum(sim.test[Pt >= i & Pt < i+0.1,]$Observed>0),"\n")
}

sum(sim.test$Observed > 0)
sum(Pt)



##### lag 6 added
x.day <- data.frame(BST.daily.2[-(1:151), (1:7)])   # Remove the NA for SST lag
x.day$dummy.p1 <- BST.daily.2$dummy.p1[-(1:151)]
x.day$dummy.p2 <- BST.daily.2$dummy.p2[-(1:151)]
x.day$events.lag.1<- 0
x.day$events.lag.2<- 0
x.day$events.lag.3<- 0
x.day$events.lag.4 <- 0
x.day$events.lag.5 <- 0
x.day$events.lag.6 <- 0

pred.core <- predict(BST.daily2.glm7, newdata = x.day)

n <- length(x.day$Day)
n.sims <- 100
y.lag1<- rep(0, n.sims)
y.lag2<- rep(0, n.sims)
y.lag3<- rep(0, n.sims)
y.lag4<- rep(0, n.sims)
y.lag5<- rep(0, n.sims)
y.lag6<- rep(0, n.sims)

y.hat<- exp(pred.core[1] + coef(BST.daily2.glm7)[5] * y.lag1 + coef(BST.daily2.glm7)[6] * y.lag2 
            + coef(BST.daily2.glm7)[7] * y.lag3 + coef(BST.daily2.glm7)[8] * y.lag4 +
              coef(BST.daily2.glm7)[9] * y.lag5 + coef(BST.daily2.glm7)[10] * y.lag6)
y.sim <- array (NA, c(n, n.sims)) 
y.sim[1,] <- rpois(n.sims, y.hat)

y.lag6 <- y.lag5
y.lag5 <- y.lag4
y.lag4 <- y.lag3
y.lag3 <- y.lag2
y.lag2 <- y.lag1
y.lag1 <- y.sim[1,]


i = 1

while (i < n){
  i = i+1
  y.hat <- exp(pred.core[i] + coef(BST.daily2.glm7)[5] * y.lag1 + coef(BST.daily2.glm7)[6] * y.lag2 
               + coef(BST.daily2.glm7)[7] * y.lag3 + coef(BST.daily2.glm7)[8] * y.lag4 +
                 coef(BST.daily2.glm7)[9] * y.lag5 + coef(BST.daily2.glm7)[10] * y.lag6)
  y.sim[i,]<- rpois(n.sims, y.hat)
  y.lag6 <- y.lag5
  y.lag5 <- y.lag4
  y.lag4 <- y.lag3
  y.lag3 <- y.lag2
  y.lag2 <- y.lag1
  y.lag1 <- y.sim[i,]
}



###### Assessment
obs.data <- BST.daily.2[-(1:151),]
plot(obs.data$Date,obs.data$events.freq)
plot(y.sim[,1])  # WOW
head(y.sim)

sim.test<- data.frame("Max" = apply (y.sim, 1, max),
                      "90%" = apply(y.sim, 1, quantile, probs= 0.9),
                      "80%" = apply(y.sim, 1, quantile, probs= 0.8),
                      "70%" = apply(y.sim, 1, quantile, probs= 0.7),
                      "60%" = apply(y.sim, 1, quantile, probs= 0.6),
                      "50%" = apply(y.sim, 1, quantile, probs= 0.5),
                      "40%" = apply(y.sim, 1, quantile, probs= 0.4),
                      "30%" = apply(y.sim, 1, quantile, probs= 0.3),
                      "20%" = apply(y.sim, 1, quantile, probs= 0.2),
                      "10%" = apply(y.sim, 1, quantile, probs= 0.1),
                      "Min" = apply(y.sim, 1, min),
                      "Mean" = apply(y.sim, 1, mean))

sim.test <- data.frame(Date = obs.data$Date,
                       Year = obs.data$Year,
                       Month = obs.data$Month,
                       Observed = obs.data$events.freq,
                       y.sim,
                       sim.test)

test.monthly <- aggregate(Observed ~ Year + Month, data= sim.test, FUN = sum)
test.monthly <- test.monthly[order(test.monthly$Year,test.monthly$Month),]    # reorder the month and year
test.monthly.sim <- aggregate(y.sim ~ Year + Month, data= sim.test, sum)
test.monthly.sim <- test.monthly.sim[order(test.monthly.sim$Year, test.monthly.sim$Month),]
test.monthly.sim$MIN<-apply(test.monthly.sim[,3:102], 1, min)
test.monthly.sim$lower <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.25)
test.monthly.sim$median <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.50)
test.monthly.sim$upper <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.75)
test.monthly.sim$MAX <- apply(test.monthly.sim[,3:102], 1, max)
test.monthly.sim$mean <- apply(test.monthly.sim[,3:102], 1, mean)

### Monthly
col.scale<- rev( heat.colors(5, alpha =1))
plot(ts(test.monthly$Observed, frequency = 12,start = c(1982,6)),type="l",ylim=c(0,15), lwd=5, xlab="Year",
     ylab="Number of days", main = "Number of days (monthly) that has a new TS event in 1982-2016")
polygon(ts(test.monthly.sim$MIN, frequency = 12,start = c(1982,6)), 
        col=col.scale[1], border = FALSE)
polygon(ts(test.monthly.sim$lower, frequency = 12,start = c(1982,6)), 
        col=col.scale[2], border = FALSE)
polygon(ts(test.monthly.sim$median, frequency = 12,start = c(1982,6)), 
        col=col.scale[3], border = FALSE)
polygon(ts(test.monthly.sim$upper, frequency = 12,start = c(1982,6)), 
        col=col.scale[4], border = FALSE)
polygon(ts(test.monthly.sim$MAX, frequency = 12,start = c(1982,6)), 
        col=col.scale[5], border = FALSE)
points(ts(test.monthly$Observed, frequency = 12,start = c(1982,6)),type="b", lwd=3)
dev.copy(pdf,"TS6_checking_monthlysum.pdf", height=6, width=8)
dev.off()

### Annually
test.annually <- aggregate(sim.test[,4:104], by= list(sim.test$Year), sum)
test.annually$MIN<- apply(test.annually[,3:102], 1, min)
test.annually$lower <- apply(test.annually[,3:102], 1, quantile, probs= 0.25)
test.annually$median <- apply(test.annually[,3:102], 1, quantile, probs= 0.50)
test.annually$upper <- apply(test.annually[,3:102], 1, quantile, probs= 0.75)
test.annually$MAX <- apply(test.annually[,3:102], 1, max)
test.annually$mean <- apply(test.annually[,3:102], 1, mean)
names(test.annually)[1]<- "Year"


#### This one looks good!
plot(test.annually$Year, test.annually$Observed, type="o", lwd=5, ylim=c(10,50),xlab="Year",
     ylab="Number of days", main = "Number of days that has a new TS event in 1982-2016")
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$MIN,rev(test.annually$lower)), col = col.scale[1], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$lower,rev(test.annually$median)), col = col.scale[2], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$median,rev(test.annually$upper)), col = col.scale[3], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$upper,rev(test.annually$MAX)), col = col.scale[4], border = NA)
points(test.annually$Year,test.annually$mean, type="l", lwd=3, col="blue")
points(test.annually$Year, test.annually$Observed, type="o", lwd=5)
legend("topright", inset=.001, title="Quantiles", c("Observed","Min","0.25","0.5","0.5","Max","Mean"), 
       col = c("Black",col.scale,"blue"),
       lty=1,text.width = 1,cex=0.5,horiz=FALSE)
dev.copy(pdf,"TS6_checking_yearsum.pdf", height=6, width=8)
dev.off()



##### probability integral tranform


pit.mon.hist<- rowMeans(test.monthly.sim[test.monthly.sim$Month %in% (6:11),][,3:102] < test.monthly[test.monthly$Month %in% (6:11),]$Observed)
hist(pit.mon.hist)

dev.copy(pdf,"TS6 Monthly PIT.pdf",width=8,height=6)
dev.off()

## annual
pit.annual.hist <- rowMeans(test.annually[,3:102] < test.annually$Observed)
hist(pit.annual.hist, breaks = c(0,0.2,0.4,0.6,0.8,1))
dev.copy(pdf,"TS6 Annual PIT.pdf",width=8,height=6)
dev.off()

##### Pt - daily
Pt <- rowMeans(y.sim > 0)

## REC
Pt.group <- cut(Pt,breaks=seq(0,1,0.05),include.lowest = TRUE, right=FALSE)
Obs.tab <- tapply(sim.test$Observed, INDEX = Pt.group, FUN=function(x) {sum(x>0)})
Exp.tab <- tapply(Pt, INDEX = Pt.group, FUN=function(x) {sum(x)})

rbind(Obs.tab,Exp.tab)

for (i in seq(0,1,0.1)){
  cat("Expected:",sum(Pt[Pt >=i & Pt < i+0.1]),"\n")
  cat("Observed:",sum(sim.test[Pt >= i & Pt < i+0.1,]$Observed>0),"\n")
}

sum(sim.test$Observed > 0)
sum(Pt)














