##### Map with City Labels
####
par(mar=rep(0,4))
dat = read.csv(text = "City,jd,wd
               Beijing,116.4666667,39.9
               Shanghai,121.4833333,31.23333333
               Tianjin,117.1833333,39.15
               Chongqing,106.5333333,29.53333333
               Harbin,126.6833333,45.75
               Changchun,125.3166667,43.86666667
               Shenyang,123.4,41.83333333
               Hohhot,111.8,40.81666667
               Shijiazhuang,114.4666667,38.03333333
               Taiyuan,112.5666667,37.86666667
               Jinan,117,36.63333333
               Zhengzhou,113.7,34.8
               Xi'an,108.9,34.26666667
               Lanzhou,103.8166667,36.05
               Yinchuan,106.2666667,38.33333333
               Xining,101.75,36.63333333
               Urumqi,87.6,43.8
               Hefei,117.3,31.85
               Nanjing,118.8333333,32.03333333
               Hangzhou,120.15,30.23333333
               Changsha,113,28.18333333
               Nanchang,115.8666667,28.68333333
               Wuhan,114.35,30.61666667
               Chengdu,104.0833333,30.65
               Guiyang,106.7,26.58333333
               Fuzhou,119.3,26.08333333
               Taibei,121.5166667,25.05
               Guangzhou,113.25,23.13333333
               Haikou,110.3333333,20.03333333
               Nanning,108.3333333,22.8
               Kunming,102.6833333,25
               Lhasa,91.16666667,29.66666667
               Hongkong,114.1666667,22.3
               Macau,113.5,22.2")

library(maps)
library(mapdata)
map("china", col = "brown", ylim = c(18, 54), panel.first = grid())
points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                    0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                        4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
title(main="Map of China")


#### Map of Worldhires

library(maptools)
map("china")

map('worldHires', c('China','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                    'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230))

dat = read.shape('bou2_4p.shp')



#### GoogleVis

CMABST$Location <- paste0(CMABST$Latitude, ":", CMABST$Longitude)


G1 <- gvisGeoChart(CMABST, locationvar='Location',
                   options=list(region='CN'))
plot(G1) 




G2 <- gvisGeoChart(CMABST, locationvar='Location', 
                   options=list(region='CN',
                                displayMode="regions",
                                resolution="provinces",
                                colorAxis="{colors: ['yellow','red']}" ))
plot(G2) 


######
## summary(CMABST$Latitude)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.50   14.10   19.10   20.72   25.60   60.50 
## summary(CMABST$Longitude)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 95.0   122.1   132.7   134.4   145.1   226.0 
## ylim = c(0,61), xlim = c(90, 230)
######

par(mar=c(5,6,4,2))
map('worldHires', c('China','Vietnam','Philippines', 'North Korea', 'South Korea', 'Japan', 
                    'Laos', 'Cambodia', 'Thailand', 'Myanmar'), ylim = c(0,61), xlim = c(90, 230))
