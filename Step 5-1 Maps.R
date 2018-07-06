library(maps)
library(mapdata)
library(lattice)
library(RColorBrewer)

quartz(width = 8, height = 6)
BST.track<- BST.events[-(1:1220),]
pch.scale<- c(19,16,20,16,19)
pch.scale<- c(19,19,16,17,17)
col.scale<- c(rev(brewer.pal(5,'RdYlBu')))
col.scale<- c(heat.colors(5, alpha = 0.5))
col.scale<- c("#2C7BB610", "#ABD9E910", "#FFFFBF10", "#FDAE6110", "#D7191C10")

rgbcols <- col2rgb(col.scale)
col.scale <- rgb(rgbcols[1,],rgbcols[2,],rgbcols[3,],alpha=0.5,maxColorValue = 255)

##  El Nino 1983; 1987; 1988; 1992; 1995; 1998; 2003; 2007; 2010
##  La Nina 1989; 1999; 2000; 2008; 2011; 2012
##  Neutral 1982; 1984; 1985; 1986; 1990; 1991; 1993; 1994; 1996; 2001; 2002; 2004; 2005; 2006; 2009; 2013; 

##  El Nino - weak: 2004-05; 2006-07; 2014-15
##          - Moderate: 1986-87; 1994-95; 2002-03; 2009-10
##          - Strong:  1987-88; 1991-92
##  Very strong: 1982-1983;1997-98; 2015-16(!)
##  La Nina - weak: 1983-84; 1984-85; 2000-01; 2005-06; 2008-09; 2016-17
##          - Moderate: 1995-96; 2011-12
##          - Strong: 1988-89; 1999-00; 2007-08; 2010-11


###### First method: Use a ENSO indicator to draw ENSO years for different phases

wanted.data <- BST.track$Year== 1989
wanted.Nino <- (BST.track$Year == 1983) | (BST.track$Year == 1987) | 
  (BST.track$Year == 1988) | (BST.track$Year == 1992) | (BST.track$Year == 1995) |
  (BST.track$Year == 1998) | (BST.track$Year == 2003) | (BST.track$Year == 2007) |
  (BST.track$Year == 2010)

wanted.Nina <- (BST.track$Year == 1989) | (BST.track$Year == 1999) | (BST.track$Year == 2000) |
  (BST.track$Year == 2008) | (BST.track$Year == 2011) | (BST.track$Year == 2012) 



strong.nino<- (BST.track$Year == 1982)| (BST.track$Year == 1987) | (BST.track$Year == 1991) |  
  (BST.track$Year == 1997)|(BST.track$Year == 2015)
moderate.nino<-(BST.track$Year == 1986)|(BST.track$Year == 1994)|(BST.track$Year == 2002)|
  (BST.track$Year == 2004)|(BST.track$Year == 2005)|(BST.track$Year == 2006)|(BST.track$Year == 2009)
  
neutral<-(BST.track$Year == 1985)|(BST.track$Year == 1989)|(BST.track$Year == 1990)|
  (BST.track$Year == 1992)|(BST.track$Year == 1993)|(BST.track$Year == 1996)|
  (BST.track$Year == 2001)|(BST.track$Year == 2003)|(BST.track$Year == 2008)|(BST.track$Year == 2012)|
  (BST.track$Year == 2013)|(BST.track$Year == 2014)

moderate.nina<-(BST.track$Year == 1983)|(BST.track$Year == 1984)|(BST.track$Year == 1995)|
  (BST.track$Year == 1989)|(BST.track$Year == 2000)|(BST.track$Year == 2011)|(BST.track$Year == 2016)

strong.nina<-(BST.track$Year == 1988)|(BST.track$Year == 1998)|(BST.track$Year == 1999)|
  (BST.track$Year == 2007)|(BST.track$Year == 2010)


### El Nino years and La Nina years are hard to define


map('worldHires', c('China','Taiwan','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                    'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230),
    mar = c(4.1, 4.1, par("mar")[3], 0.1))

points(BST.track[wanted.Nino,]$Longitude, BST.track[wanted.Nino,]$Latitude, col = col.scale[5],pch=pch.scale[5])
points(BST.track[wanted.Nina,]$Longitude, BST.track[wanted.Nina,]$Latitude, col = col.scale[1],pch=pch.scale[1])


map('worldHires', c('China','Taiwan','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                    'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230),
    mar = c(4.1, 4.1, par("mar")[3], 0.1))

points(BST.track[strong.nino,]$Longitude, BST.track[strong.nino,]$Latitude, col= col.scale[5], pch=pch.scale[5])
points(BST.track[moderate.nino,]$Longitude, BST.track[moderate.nino,]$Latitude, col= col.scale[4], pch=pch.scale[4])
points(BST.track[neutral,]$Longitude, BST.track[neutral,]$Latitude, col= col.scale[3], pch=pch.scale[3])
points(BST.track[moderate.nina,]$Longitude, BST.track[moderate.nina,]$Latitude, col= col.scale[2], pch=pch.scale[2])
points(BST.track[strong.nina,]$Longitude, BST.track[strong.nina,]$Latitude, col= col.scale[1], pch=pch.scale[1])
legend("topright", inset=.001, title="ENSO Phase", c("Strong Nina","Moderate Nina","Neutral","Moderate Nino","Strong Nino"), 
       col = col.scale,
       pch=pch.scale,text.width = 10,cex=0.5,horiz=FALSE)

## REC

points(BST.track[strong.nino,]$Longitude, BST.track[strong.nino,]$Latitude, col= col.scale[5], pch=16,cex=3)
points(BST.track[moderate.nino,]$Longitude, BST.track[moderate.nino,]$Latitude, col= col.scale[4], pch=16,cex=3)
points(BST.track[neutral,]$Longitude, BST.track[neutral,]$Latitude, col= col.scale[3], pch=16,cex=3)
points(BST.track[moderate.nina,]$Longitude, BST.track[moderate.nina,]$Latitude, col= col.scale[2], pch=16,cex=3)
points(BST.track[strong.nina,]$Longitude, BST.track[strong.nina,]$Latitude, col= col.scale[1], pch=16,cex=3)
legend("topright", inset=.001, title="ENSO Phase", c("Strong Nina","Moderate Nina","Neutral","Moderate Nino","Strong Nino"), 
       col = col.scale,
       pch=pch.scale,text.width = 10,cex=0.5,horiz=FALSE)


dev.copy(pdf,"Genesis_map_ENSO.pdf",width=8,height=6)
dev.off()
dev.copy(pdf,"Genesis_map_Neutral_years.pdf",width=8,height=6)
dev.off()
###### Second method: 
Use SST directly

View(BST.track)
str(BST.track)
str(Nino)
Nino$Year<-as.factor(Nino$Year)

BST.track<- merge(BST.track, Nino, all=TRUE)
BST.track$Indicator<- c(0)

BST.track[BST.track$ANOM3.4 >=1.5,]$Indicator<- 5
BST.track[BST.track$ANOM3.4 < 1.5 & BST.track$ANOM3.4 >=0.5,]$Indicator <- 4
BST.track[BST.track$ANOM3.4 < 0.5 & BST.track$ANOM3.4 >-0.5,]$Indicator <- 3
BST.track[BST.track$ANOM3.4 <= -0.5 & BST.track$ANOM3.4 >-1.5,]$Indicator <- 2
BST.track[BST.track$ANOM3.4 <= -1.5,]$Indicator <- 1


map('worldHires', c('China','Taiwan','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                    'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230),
    mar = c(4.1, 4.1, par("mar")[3], 0.1))
points(BST.track$Longitude, BST.track$Latitude, col=col.scale[BST.track$Indicator],
       pch=pch.scale[BST.track$Indicator])
legend("topright", inset=.001, title="ENSO Phase", c("Strong Nina","Moderate Nina","Neutral","Moderate Nino","Strong Nino"), 
       col = col.scale,
       pch=pch.scale,text.width = 10,cex=0.5,horiz=FALSE)
dev.copy(pdf,"Genesis_map_sst.pdf",width=8,height=6)
dev.off()
