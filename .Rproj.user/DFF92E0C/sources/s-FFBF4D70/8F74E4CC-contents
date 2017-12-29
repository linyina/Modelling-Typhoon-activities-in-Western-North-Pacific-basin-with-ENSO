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

