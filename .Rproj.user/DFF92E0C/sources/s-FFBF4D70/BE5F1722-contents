library(AER)
library(pscl)

diag.model <- function(model){
  # 1. Usual goodness-of-fit
  anova(model, test = "Chi")
  cat("The p-value given the residual deviance statistic is:", dev.res(model), "\n")
  # 2. Check for over/underdispersion
  dispersiontest(model)
  cat("The dispersion of the model is:",disp(model), "\n")
  par(mfrow=c(2,1))
  # 3. Influential and leverage points
  influencePlot(model)  # if many points are highly influential - poisson bad model
  cat("The levarages of the model are:", "\n")
  Lev(model, model$df.null - model$df.residual + 1)
  # 5. plot the residuals
  res <- residuals(model, type="deviance")
  plot(predict(model), res)
  abline(h=0, lty=2)
  par(mfrow=c(1,1))
  
}

diag.model(BST.glm.ts.3)
diag.model(BST.glm.ty.1)
diag.model(BST.glm.superty.3)

# 4. Check for zero inflation
library(pscl)
mod2 <- zeroinfl(TS.freq ~ sst.lag.7 + dummy.p1 + dummy.p2, data=BST.ana, dist = "poisson")
AIC(BST.glm.ts.3, mod2)

# 6. Diagnostic plots
plot(log(TS.freq) ~ sst.lag.7, data=BST.ana) 
prs  <- predict(BST.glm.ts.3, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ BST.ana[is.na(BST.ana$sst.lag.7)==FALSE,]$sst.lag.7, col="red")
points(pris$lwr  ~ BST.ana[is.na(BST.ana$sst.lag.7)==FALSE,]$sst.lag.7, col="pink", pch=19)
points(pris$upr  ~ BST.ana[is.na(BST.ana$sst.lag.7)==FALSE,]$sst.lag.7, col="blue", pch=19)



### cooks distance plot
cook.plot <- function(model,x=NULL,n.id=NULL,lab.id=NULL,cex.id=1,
                      xlab="Observation number",
                      ylab="Cook's distance",ylim=NULL,...) {
  cook <- cooks.distance(model)
  n <- length(model$fitted.values); k <- length(coef(model))
  cook.thresh <- 8/(n-(2*k))
  if (is.null(x)) x <- 1:n
  if (is.null(ylim)) ylim <- c(0,max(c(1.1*cook,cook.thresh)))
  if (is.null(n.id)) n.id <- sum(cook > cook.thresh)
  plot(x,cook,type="h",xlab=xlab,ylab=ylab,ylim=ylim,...)
  abline(cook.thresh,0,lty=2,col=grey(0.4))
  if (n.id > 0) {
    if (is.null(lab.id)) text.labs <- x else text.labs <- lab.id 
    largest <- order(cook,decreasing=TRUE)[1:n.id]
    text(x[largest],cook[largest],text.labs[largest],cex=cex.id,pos=3)
  }
}


### (1) Leverage
Lev <- function (model,x){
  hii<-hatvalues(model)
  p=x
  n=803
  Lev.log<- hii>2*(p+1)/n
  print(Lev.log[Lev.log==TRUE])
}
# (2) DFFITS
Dff<- function(model,x){
  dff<-dffits(model)
  p=x
  n=803
  Dff.log<- dff>2*sqrt((p+1)/n)
  print(Dff.log[Dff.log==TRUE])
}

#(3) Cook's distance
Cook<- function(model,x){
  cook<- cooks.distance(model)
  Cook.log<- cook> 8/(803-2*x)
  print(Cook.log[Cook.log==TRUE])
}

#(4) Vif
Ref.vif<-function(model){
  Vif<-vif(model, digits=3)
  print(Vif[Vif>=10])
}

Checking<- function(model,x){
  plot(model, which=1:4)
  cat("\n Levarage:\n")
  Lev(model,x)
  cat("\n DFFITS:\n")
  Dff(model,x)
  cat("\n Cook's distance:\n")
  Cook(model,x)
  cat("\n VIF \n")
  Ref.vif(model)
}

#(5) dispersion check - Pearson residual
disp<- function(model){
  sum( resid(model,type="pearson")^2 ) / model$df.residual 
}


# (6) goodness of fit
dev.res<-function(model){
  pchisq(deviance(model),df.residual(model), lower.tail = FALSE)
}


############################## Whole process #################################
#### Diagnostic

# 1. Usual goodness-of-fit
anova(BST.glm.ts.1, test = "Chi")
# 2. Check for over/underdispersion
library(AER)
dispersiontest(BST.glm.ts.1)
disp(BST.glm.ts.1)

# 3. Influential and leverage points
influencePlot(BST.glm.ts.1)  # if many points are highly influential - poisson bad model
Lev(BST.glm.ts.1, 4)

# 4. Check for zero inflation
library(pscl)
mod2 <- zeroinfl(TS.freq ~ sst.lag.7 + dummy.p1 + dummy.p2, data=BST.ana, dist = "poisson")
AIC(BST.glm.ts.1,BST.glm.ts.3, mod2)

mod3 <- zeroinfl(TY.freq ~ NINO3.4 + dummy.p1 + dummy.p2, data=BST.ana, dist = "poisson")
AIC(BST.glm.ty.1, mod3)

mod4 <- zeroinfl(SuperTY.freq ~ sst.lag.1 + dummy.p1 + dummy.p2, data=BST.ana, dist = "poisson")

# 5. plot the residuals
res <- residuals(BST.glm.ts.1, type="deviance")
plot(predict(BST.glm.ts.1), res)
abline(h=0, lty=2)

# 6. Diagnostic plots
plot(log(TS.freq) ~ sst.lag.7, data=BST.ana) 
prs  <- predict(BST.glm.ts.3, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ BST.ana[is.na(BST.ana$sst.lag.7)==FALSE,]$sst.lag.7, col="red")
points(pris$lwr  ~ BST.ana[is.na(BST.ana$sst.lag.7)==FALSE,]$sst.lag.7, col="pink", pch=19)
points(pris$upr  ~ BST.ana[is.na(BST.ana$sst.lag.7)==FALSE,]$sst.lag.7, col="blue", pch=19)
