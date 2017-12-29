##############################################################################
###                                                                        ###
###                       This is for STAT3901 PROJECT                     ###
###                   Title: 
###                                                                        ###
###                                                                        ###
###                                                                        ###
##############################################################################

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
wanted.data <- BST.all.data[,1] == 66666
summary(BST.all.data$V4[wanted.data])


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
write.table(BST.all.data, "BST_All_Data.dat", row.names = FALSE)

## 1.2.2  Clean the headers

Head.line <- 1
i <- 0
BST.clean <- BST.all.data

while ( Head.line <= 68514 ){
  
  BST.clean <- BST.clean[- (Head.line - i),]
  Head.line <- Head.line + BST.all.data[Head.line, 3] + 1
  i <- i+1
  
}

write.table(BST.clean, "BST_no_header.dat", row.names = FALSE)


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
while (a < 816){
  a = a + 1
  
  if (is.na(Landfall[a,1]) == TRUE){
    
    Landfall[a,1] <- Landfall[b,1]
    
  }
  else { b = a }
}

## Intensity category


###############################################################
####   STEP 2. EXPLORATORY STATISTICS                      ####
####                                                       ####
####   Aim: (1) Identify any Important features            ####
####
####
############################################################

#### 2.1 Map

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
  if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)
  par(mar=c(5,6,4,2))
  plot.list <- levels(as.factor(x$SerialNum))
  map('worldHires', c('China','Taiwan','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                      'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230),
      mar = c(4.1, 4.1, par("mar")[3], 0.1))
  
  i = 0
  
  while (i <=2245){
    i = i+1
    wanted.data <- x$SerialNum == Serial.list[i]
    lines(CMABST$Longitude[wanted.data],CMABST$Latitude[wanted.data], col= rgb(0,0,0,alpha=0.1))
  }
  
}

plot.track(CMABST)
dev.copy(pdf,"CMABST_all_tracks_b&w.pdf",width=8,height=6)


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
  if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)
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
dev.copy(pdf,"CMABST_1979_1988.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec5)
dev.copy(pdf,"CMABST_1989_1998.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec6)
dev.copy(pdf,"CMABST_1999_2008.pdf",width=8,height=6)
dev.off()
plot.cols(wanted.dec7)
dev.copy(pdf,"CMABST_2009_2016.pdf",width=8,height=6)
dev.off()


#######################################################
####  2.2 Time series plot
####
#######################################################
library(lattice)   # heatmap and xyplot
library(ggplot2)   # time series plot
library(MASS)
library(tidyr)  # time series plot
library(RColorBrewer) # Colours
library(reshape2) # melt
########################
## a. Landfall
## Q1: Does the frequency in landfall/ severe typhoon increase each year?
## New dataset of total times of landfall each year
land.clean <- na.omit(Landfall) ## Omit the missing values
sum(land.clean$`Total Times of Landfall`)
lf.freq<-aggregate(land.clean$`Total Times of Landfall`, by=list(Year=(land.clean$Year)), FUN=sum) #sum by year
if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)
plot.ts(lf.freq$x,  xlab="Year", ylab= "Total times of Landfall",
        main="Total times of landfall by year",xaxt = "n")
axis(1, at=c(1:68),labels=c(1949:2016))
dev.copy(pdf,"total times of landfall by year.pdf",width=8,height=6)
dev.off()
## Find out the year with maximum frequencies
lf.freq[lf.freq$x==max(lf.freq$x),]$Year


## Q2: Adding more factors: Does freqency in tropical cyclones increase each year?
## Total number of tropical cyclones per year
CMA.SN<- data.frame(Year=substr(CMABST$SerialNum, start = 1, stop = 4), 
                    SN=substr(CMABST$SerialNum, start = 6, stop = 7))
CMA.SN$SN<-as.character(CMA.SN$SN)
CMA.SN$SN<-as.numeric(CMA.SN$SN)
CMA.freq<- aggregate(as.numeric(CMA.SN$SN),by=list(Year=as.numeric(CMA.SN$Year)), FUN=max)
DT<- data.frame(lf.freq$Year, lf.freq$x,CMA.freq$x)
names(DT)<-c("Year", "lf.freq", "CMA.freq")
plot.new()
DT %>%
  gather(key,value, lf.freq, CMA.freq) %>%
  ggplot(aes(x=Year, y=value, colour=key)) +
  geom_line() + 
  ggtitle("Freqency of tropical cyclones and landfall each year in China from 1949-2016") +
  xlab("Year 1949-2016") + ylab("Frequency")
dev.copy(pdf,"lf and cma.freq by year.pdf",width=8,height=6)
dev.off()

## Q3: Does the intensity of landfall and TC increase?
## a. Intensity of landfall
##
## Reconstructing the Intensity category
levels(land.clean$`Landfall Intensity Category`)[land.clean$`Landfall Intensity Category`==""]<-"0"
levels(land.clean$`Landfall Intensity Category`)[land.clean$`Landfall Intensity Category`=="TD"]<-"1"
levels(land.clean$`Landfall Intensity Category`)[land.clean$`Landfall Intensity Category`=="TS"]<-"2"
land.clean[land.clean$`Landfall Intensity Category`=="TS",]$`Landfall Intensity Category` <- "2"
levels(land.clean$`Landfall Intensity Category`)[land.clean$`Landfall Intensity Category`=="STS"]<-"3"
levels(land.clean$`Landfall Intensity Category`)[land.clean$`Landfall Intensity Category`=="TY"]<-"4"
levels(land.clean$`Landfall Intensity Category`)[land.clean$`Landfall Intensity Category`=="STY"]<-"5"
levels(land.clean$`Landfall Intensity Category`)[land.clean$`Landfall Intensity Category`=="SuperTY"]<-"6"
## lf Mean
lf.mean.int<-aggregate(as.numeric(land.clean$`Landfall Intensity Category`), 
                       by=list(Year=(land.clean$Year)), FUN=mean)
## lf max
lf.max.int <- aggregate(as.numeric(land.clean$`Landfall Intensity Category`), 
                        by=list(Year=(land.clean$Year)), FUN=max)
## b. Intensity of TC
##
CMA.int<- data.frame(CMABST$Year, CMABST$Intensity)
CMA.int[CMA.int$CMABST.Intensity==9,]$CMABST.Intensity <- NA
CMA.int<-na.omit(CMA.int)
CMA.mean.int<-aggregate(CMA.int$CMABST.Intensity, by=list(Year=(CMA.int$CMABST.Year)), FUN=mean)
CMA.max.int<-aggregate(CMA.int$CMABST.Intensity, by=list(Year=(CMA.int$CMABST.Year)), FUN=max)


DT.int<- data.frame(lf.mean.int, CMA.mean.int$x, lf.max.int$x, CMA.max.int$x)
names(DT.int)<-c("Year", "lf.mean.int","CMA.mean.int", "lf.max.int", "CMA.max.int")
plot.new()
DT.int %>%
  gather(key,value, lf.mean.int,CMA.mean.int, lf.max.int, CMA.max.int) %>%
  ggplot(aes(x=Year, y=value, colour=key)) +
  geom_line() + 
  ggtitle("Intensity of landfall and TC each year in China from 1949-2016") +
  xlab("Year 1949-2016") + ylab("Intensity")
dev.copy(pdf,"LF CMA intensity by year.pdf",width=8,height=6)
dev.off()

## Q4: Which provinces has the most frequent landfall?
tapply(Landfall$`Total Times of Landfall`, INDEX = Landfall$`Landfall Province`, FUN = sum)
lf.prov<-aggregate(land.clean$`Total Times of Landfall` ~ 
                     land.clean$Year + land.clean$`Landfall Province`, land.clean, sum)
names(lf.prov)<-c("Year","Province","Landfall times")
lf.prov$Province <- factor(lf.prov$Province, levels = c("Hainan","Fujian",
                                                        "Zhejiang","Shandong","Guangdong", 
                                                        "Hong Kong","Jiangsu","Liaoning","Taiwan",
                                                        "Shanghai","Guangxi"))
plot.new()
xyplot(lf.prov$`Landfall times` ~ lf.prov$Year | lf.prov$Province, 
       type="l",xlab="Year",ylab="Total times of landfall",
       main="Frequency of landfall by year for different provinces")
dev.copy(pdf,"Frequency of landfall_provinces.pdf",width=8,height=6)
dev.off()

## B: Heatmap
## Intensity of landfall by provinces
lf.mean.int.prov<-aggregate(as.numeric(land.clean$`Landfall Intensity Category`) ~ 
                              land.clean$Year + land.clean$`Landfall Province`, land.clean, mean)
names(lf.mean.int.prov)<- c("Year", "Province", "Mean Intensity")
## Creating a new data.frame
DT.prov<- data.frame(Year=rep(1949:2016, each=15), Province= rep(levels(lf.mean.int.prov$Province), 68), 
                     MeanInt=rep(0, 1020))

i = 1948
while (i <= 2016) {
  i = i+1
  wanted.data<-lf.mean.int.prov[lf.mean.int.prov$Year == i,]
  for (j in 1:nrow(wanted.data)){
    DT.prov[(DT.prov$Year == wanted.data$Year[j]) 
            & (DT.prov$Province == wanted.data$Province[j]),]$MeanInt <- 
      wanted.data$"Mean Intensity"[j]
  }
  
}

DT.prov<-DT.prov[!(DT.prov$Province==""),]
DT.prov<-DT.prov[!(DT.prov$Province=="Shanghai鈥揨hejiang"),]
DT.prov<-DT.prov[!(DT.prov$Province=="Zhejiang鈥揊ujian"),]

DT.prov$MI <- cut(DT.prov$MeanInt,breaks = c(-Inf,0:6,Inf),right = FALSE)
hm.col<- rev(heat.colors(6, alpha = 0.8))

DT.prov$od.prov<-reorder(DT.prov$Province, DT.prov$MeanInt)

plot.new()

p <- ggplot(data=DT.prov, aes(od.prov, Year)) + geom_tile(aes(fill = MI),
                                                          colour = "white")+ scale_fill_manual(values = hm.col)+ labs(x = "Provinces",y = "Year", 
                                                                                                                      title = "Mean Intensity of Landfall each year from 1949-2016 by Provinces")

p + theme(plot.title = element_text(size = rel(1.5))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "grey50"))


dev.copy(pdf,"Heatmap_landfall_provinces.pdf",width=8,height=6)
dev.off()
