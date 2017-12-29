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
