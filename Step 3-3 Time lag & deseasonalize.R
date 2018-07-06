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

## sst.des vs. superty.des
print(ccf(sst.des, superty.des))
plot(ccf(sst.des, superty.des)) # lag -1


BST.ana$sst.lag.1 <- c(NA, BST.ana$NINO3.4[1:419])
BST.ana$TNI.lag.1<- c(NA, BST.ana$TNI[1:419])

par(mfrow=c(3,1))
ccf(sst.des, ts.des, main="CCFs for deseasonalized monthly average of Nino3.4 and frequency of TS+", xlab="Lag(Month)")
ccf(sst.des, ty.des, main="CCFs for deseasonalized monthly average of Nino3.4 and frequency of TY+", xlab="Lag(Month)")
ccf(sst.des, superty.des, main="CCFs for deseasonalized monthly average of Nino3.4 and frequency of TS+", xlab="Lag(Month)")
dev.copy(pdf,"CCF after deseasonalize.pdf",width=8,height=6)
dev.off()
par(mfrow=c(1,1))
