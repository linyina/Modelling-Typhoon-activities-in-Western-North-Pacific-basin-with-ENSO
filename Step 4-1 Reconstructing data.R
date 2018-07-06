
### Producing a calendar dataframe from 1982 to 2016
date<- seq(as.Date("1982-01-01"), as.Date("2016-12-31"), by="days")

BST.date <- merge(BST.SN.freq[BST.SN.freq$Intensity>=2,], BST.events)     ## can do the events for Tropical cyclone + by filter the bst.sn.freq[Intensity>=2,]
BST.date <- BST.date[480:945,]

as.Date(paste0(BST.freq$Year,"-",BST.freq$Month, "-",BST.freq$Date))

BST.date$date <- as.Date(paste0(BST.date$Year,"-",BST.date$Month, "-",BST.date$Date))

BST.date <- merge(df.daily, BST.date, all=TRUE)

BST.date[is.na(BST.date$freq),]$freq<- 0
BST.date$Year <- substr(BST.date$date, start = 1, stop= 4)
BST.date$Date <- substr(BST.date$date, start = 9, stop= 10)

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
                     TNI= BST.ana$TNI,
                     TNILag5=BST.ana$TNI.lag.5)

BST.daily <- merge(Nino34,BST.daily, sort = FALSE)

DaysinMonth <- c(31,29,31,30,31,30,31,31,30,31,30,31)
Cumdays <- c(0, cumsum(DaysinMonth[-12]))
BST.daily$dayofyear <- Cumdays[BST.daily$Month] + as.integer(BST.daily$Day)
BST.daily$dummy.p1 <- cos(2*pi*BST.daily$dayofyear / 366)
BST.daily$dummy.p2 <- sin(2*pi*BST.daily$dayofyear / 366)
