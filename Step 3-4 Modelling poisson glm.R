###########################
### 3-4 Building monthly models  - poisson glm
###########################
library(DAAG)
quartz(height = 6, width = 8)
### Step 1. Create the dummy variables for representing the seasonality

BST.ana$dummy.p1 <- cos(pi*BST.ana$Month /6)
BST.ana$dummy.p2 <- sin(pi*BST.ana$Month /6)

### Step 2. Before conducting time lag - using nino 3.4
### 1: Using dummy seasonal covariates - cosine function
BST.glm.ts.1<- glm(TS.freq ~ NINO3.4 + dummy.p1 + dummy.p2, data=BST.ana[6:420,],family=poisson(link = log))
summary(BST.glm.ts.1)
BST.glm.ty.1<- glm(TY.freq ~ NINO3.4 + dummy.p1 + dummy.p2, data=BST.ana,family=poisson(link = log))
summary(BST.glm.ty.1)
BST.glm.superty.1<- glm(SuperTY.freq ~ NINO3.4 + dummy.p1 + dummy.p2, data=BST.ana[2:420,],family=poisson(link = log))
summary(BST.glm.superty.1)

### 1a: Adding TNI
BST.glm.ts.1a <- update(BST.glm.ts.1, .~. + TNI)
summary(BST.glm.ts.1a)
anova(BST.glm.ts.1, BST.glm.ts.1a, test = "Chi") # Slightly better
BST.glm.ty.1a<- update(BST.glm.ty.1, .~. + TNI)
summary(BST.glm.ty.1a)
anova(BST.glm.ty.1, BST.glm.ty.1a, test = "Chi") # Slightly better
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
anova(BST.glm.superty.1a, BST.glm.superty.2, test="Chi") #SLIGHTTTTTly better

### Step 3: interactions
BST.glm.ts.3<- update(BST.glm.ts.2, .~.+ sst.lag.5:dummy.p1 + sst.lag.5:dummy.p2)
summary(BST.glm.ts.3)
anova(BST.glm.ts.2, BST.glm.ts.3, test="Chi") # Gooood!
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


