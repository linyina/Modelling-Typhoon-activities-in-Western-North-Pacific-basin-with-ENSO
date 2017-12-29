quartz(width = 8, height = 6)
quartz.save(file, type = "png", device = dev.cur(), dpi = 100, ...)

## 1. deseasonalize
## A. Richard's method - lm: residual
seas.sst<- lm(BST.ana$NINO3.4 ~ as.factor(BST.ana$Month))
sst.res<- summary(seas.sst)$residual
seas.ts <- lm(BST.ana$TS.freq ~ as.factor(BST.ana$Month))
ts.res<- summary(seas.ts)$residual
seas.ty <- lm(BST.ana$TY.freq ~ as.factor(BST.ana$Month))
ty.res<- summary(seas.ty)$residual
seas.sty<- lm(BST.ana$SuperTY.freq ~ as.factor(BST.ana$Month))
superty.res<- summary(seas.sty)$residual


plot(sst.res)
plot(ts.res)

## B. decompose method: moving average
library(TTR)
# a. make it in time series format
## tropical storm +
trial.ts <- ts(BST.ana$TS.freq, frequency=12,start = c(1982,1))
start(trial.ts)
end(trial.ts)
plot.ts(trial.ts)
trial.ty<- ts(BST.ana$TY.freq, frequency = 12, start = c(1982,1))
trial.sty<- ts(BST.ana$SuperTY.freq, frequency = 12, start = c(1982,1))
trial.sst<- ts(BST.ana$NINO3.4, frequency = 12, start = c(1982,1))

### b. centered moving average - detect the trend
library(forecast)
trend_ts = ma(trial.ts, order = 12, centre = T)
plot(as.ts(trial.ts))
plot(as.ts(trend_ts))
trend_ty = ma(trial.ty, order = 12, centre = T)
plot(as.ts(trial.ty))
plot(trend_ty)
trend_sty= ma(trial.sty, order = 12, centre = T)
plot(as.ts(trial.sty))
plot(trend_sty)
trend_sst= ma(trial.sst, order = 12, centre = T)
plot(as.ts(trial.sst))
plot(trend_sst)

### c. decompose
### 1st thing to do: recognising the type of the model
### additive model - since the seasonal variation not necessarily increase with time

help(stl)

stl_ts <- stl(trial.ts, "periodic")
stl_ty <- stl(trial.ty, "periodic")
stl_sty <- stl(trial.sty, "periodic")
stl_sst <- stl(trial.sst, "periodic")
plot(stl_ts)
plot(stl_ty)
plot(stl_sty)
plot(stl_sst)


BST.ana <- within(BST.ana, ts.stl <- BST.ana$TS.freq - stl_ts$time.series[,1])
BST.ana <- within(BST.ana, ts.stl <- stl_ts$time.series[,2])
BST.ana[BST.ana$ts.stl<=0,]$ts.stl <- 0
BST.ana <- within(BST.ana, ty.stl <- BST.ana$TY.freq - stl_ty$time.series[,1])
BST.ana <- within(BST.ana, sty.stl <- BST.ana$SuperTY.freq - stl_sty$time.series[,1])
BST.ana <- within(BST.ana, sst.stl <- BST.ana$NINO3.4 - stl_sst$time.series[,1])

### other resources from anomoly.io
seasonal_stl_ts   <- stl_ts$time.series[,1]
trend_stl_ts     <- stl_ts$time.series[,2]
random_stl_ts  <- stl_ts$time.series[,3]
stl_ts$time.series
plot(ts_beer)
plot(as.ts(seasonal_stl_beer))
plot(trend_stl_beer)
plot(random_stl_beer)



## 2. lag ---- ccf 
help(ccf)
ccf.ts<-ccf(BST.ana$NINO3.4,BST.ana$TS.freq)
ccf.ts
ccf.ts$lag[ccf.ts$acf==max(abs(ccf.ts$acf))]  # lag 8
plot(ccf.ts)
ccf.ty<-ccf(BST.ana$NINO3.4, BST.ana$TY.freq)
ccf.ty
ccf.ty$lag[ccf.ty$acf==max(abs(ccf.ty$acf))]  # lag 8
plot(ccf.ty)
ccf.sty<-ccf(BST.ana$NINO3.4, BST.ana$SuperTY.freq)
ccf.sty
ccf.sty$lag[ccf.sty$acf==max(abs(ccf.sty$acf))]   # lag 7
plot(ccf.sty)

sst.lag <- c(rep(NA,8), BST.ana$NINO3.4)
sst.lag <- sst.lag[1:420]
sst.lag.sty <- c(rep(NA,7), BST.ana$NINO3.4)
sst.lag.sty <- sst.lag[1:420]
BST.ana$sst.lag.sty<- sst.lag.sty
## deseasonalize after lag
seas.sst.lag<- lm(BST.ana$sst.lag ~ as.factor(BST.ana$Month))
sst.lag.res<- summary(seas.sst.lag)$residual
seas.sst.lag7<- lm(BST.ana$sst.lag.sty ~ as.factor(BST.ana$Month))
sst.lag7.res<- summary(seas.sst.lag7)$residual
BST.ana <- within(BST.ana, sst.lag8.adj <- BST.ana$sst.lag - sst.lag.res)
BST.ana <- within(BST.ana, sst.lag7.adj <- BST.ana$sst.lag.sty - sst.lag7.res)
