### 3-4 Building models  - poisson glm
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

# underdispersion
disp(BST.glm.ts.1)
disp(BST.glm.ty.1)
disp(BST.glm.superty.1)

dev.res(BST.glm.ts.1)
dev.res(BST.glm.ty.1)
dev.res(BST.glm.superty.1)

### !Diagnostics: assumptions checking!
drop1(BST.glm.ts.1, test = "Chi")
drop1(BST.glm.ty.1, test = "Chi")
drop1(BST.glm.superty.1, test = "Chi")
plot.glm(BST.glm.ts.1)
par(mfrow=c(3,1))
cook.plot(BST.glm.ts.1)
cook.plot(BST.glm.ty.1)
cook.plot(BST.glm.superty.1)
dev.copy(pdf, "BST.glm.1.cook.pdf", width=6, height=8)
dev.off()
par(mfrow=c(1,1))
diag.model(BST.glm.ts.1)
diag.model(BST.glm.ty.1)
diag.model(BST.glm.superty.1)

print("The standard Poisson estimates for the beta vector are")
print(coef(BST.glm.ts.1))


### Step 2: with time lag
BST.glm.ts.2<- glm(TS.freq ~ sst.lag.5 + dummy.p1 + dummy.p2, data=BST.ana[6:420,],family=poisson(link = log))
summary(BST.glm.ts.2)

BST.glm.superty.2 <- glm(SuperTY.freq ~ sst.lag.1 + dummy.p1 + dummy.p2, data=BST.ana[2:420,],family=poisson(link = log))
summary(BST.glm.superty.2)   

disp(BST.glm.ts.2)
dev.res(BST.glm.ts.2)
anova(BST.glm.ts.1, BST.glm.ts.2)
Checking(BST.glm.ts.2, 4)
cook.plot(BST.glm.ts.2)
drop1(BST.glm.ts.2)
diag.model(BST.glm.ts.2)

disp(BST.glm.ty.1)
dev.res(BST.glm.ty.1)
drop1(BST.glm.ty.1)
Checking(BST.glm.ty.1, 4)
cook.plot(BST.glm.ty.1)
diag.model(BST.glm.ty.1)

disp(BST.glm.superty.2)
dev.res(BST.glm.superty.2)
anova(BST.glm.superty.1, BST.glm.superty.2)
drop1(BST.glm.superty.2)
Checking(BST.glm.superty.2,4)
cook.plot(BST.glm.superty.2)
diag.model(BST.glm.superty.2)

par(mfrow=c(2,1))
cook.plot(BST.glm.ts.2)
cook.plot(BST.glm.superty.2)
dev.copy(pdf, "BST.glm.2.cook.pdf", width=6, height=8)
dev.off()
par(mfrow=c(1,1))

### Step 3: interactions      -  not factor

BST.glm.ts.3 <- update(BST.glm.ts.2, .~. + sst.lag.5:dummy.p1 + sst.lag.5:dummy.p2)
summary(BST.glm.ts.3)
anova(BST.glm.ts.2, BST.glm.ts.3, test = "Chi")
dev.res(BST.glm.ts.3)
drop1(BST.glm.ts.3)
disp(BST.glm.ts.3)

BST.glm.ty.3 <- update(BST.glm.ty.1,  .~. + NINO3.4:dummy.p1 + NINO3.4:dummy.p2 )
summary(BST.glm.ty.3)
anova(BST.glm.ts.1, BST.glm.ts.3, test = "Chi")
dev.res(BST.glm.ty.3)
drop1(BST.glm.ty.3)
disp(BST.glm.ty.3)

BST.glm.superty.3 <- update(BST.glm.superty.2, .~. + sst.lag.1:dummy.p1 + sst.lag.1:dummy.p2)
summary(BST.glm.superty.3)
anova(BST.glm.superty.2, BST.glm.superty.3, test= "Chi")   # oops
disp(BST.glm.superty.3)
dev.res(BST.glm.superty.3)
drop1(BST.glm.superty.3)
disp(BST.glm.superty.3)


### Step 3a: Com-Poisson
library(COMPoissonReg)
BST.glm.ts.3a <- glm.cmp(TS.freq ~ sst.lag.5 + dummy.p1 + dummy.p2 + sst.lag.5:dummy.p1 + 
                           sst.lag.5:dummy.p2, data=BST.ana[6:420,])
summary(BST.glm.ts.3a)
print("The COM-Poisson estimates for the beta vector are") 
print(coef(BST.glm.ts.3a))
print("The COM-Poisson estimate for the dispersion parameter nu is")
print(nu(BST.glm.ts.3a))
print("The associated standard errors for the betas in the constant dispersion case are")
print(sdev(BST.glm.ts.3a))

BST.glm.ty.3a <- glm.cmp(TY.freq ~ NINO3.4 + dummy.p1 + dummy.p2 + NINO3.4:dummy.p1 + 
                           NINO3.4:dummy.p2, data=BST.ana)
summary(BST.glm.ty.3a)


BST.glm.superty.3a <- glm.cmp(SuperTY.freq ~ sst.lag.1 + dummy.p1 + dummy.p2 + 
                                sst.lag.1:dummy.p1 + sst.lag.1:dummy.p2, 
                              data = BST.ana[2:420, ])
summary(BST.glm.superty.3a)
