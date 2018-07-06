####### lag 3 added

x.day <- data.frame(BST.daily[-(1:151), (1:7)])   # Remove the NA for SST lag
x.day$dummy.p1 <- BST.daily$dummy.p1[-(1:151)]
x.day$dummy.p2 <- BST.daily$dummy.p2[-(1:151)]
x.day$events.lag.1<- 0
x.day$events.lag.2<- 0
x.day$events.lag.3<- 0

pred.core <- predict(BST.daily.glm.7, newdata = x.day)

n <- length(x.day$Day)
n.sims <- 100
y.lag1<- rep(0, n.sims)
y.lag2<- rep(0, n.sims)
y.lag3<- rep(0, n.sims)

y.hat<- exp(pred.core[1] + coef(BST.daily.glm.7)[5] * y.lag1 + coef(BST.daily.glm.7)[6] * y.lag2 +
              coef(BST.daily.glm.7)[7] * y.lag3)
y.sim <- array (NA, c(n, n.sims)) 
y.sim[1,] <- rpois(n.sims, y.hat)

y.lag <- array (NA, c(n,n.sims))
y.lag[1,] <- y.sim[1,]
y.lag[1,][y.lag[1,]>=1]<-1

y.lag3 <- y.lag2
y.lag2 <- y.lag1
y.lag1 <- y.lag[1,]


i = 1

while (i < n){
  i = i+1
  y.hat <- exp(pred.core[i] + coef(BST.daily.glm.7)[5] * y.lag1 + coef(BST.daily.glm.7)[6] * y.lag2+
                 coef(BST.daily.glm.7)[7] * y.lag3)
  y.sim[i,]<- rpois(n.sims, y.hat)
  y.lag[i,]<- y.sim[i,]
  y.lag[i,][y.lag[i,]>=1]<-1
  y.lag3 <- y.lag2
  y.lag2 <- y.lag1
  y.lag1 <- y.lag[i,]
}



###### Assessment
obs.data <- BST.daily[-(1:151),]
plot(obs.data$Date,obs.data$events.freq)
plot(y.sim[,1])  # WOW
head(y.sim)

sim.test<- data.frame("Max" = apply (y.sim, 1, max),
                      "90%" = apply(y.sim, 1, quantile, probs= 0.9),
                      "80%" = apply(y.sim, 1, quantile, probs= 0.8),
                      "70%" = apply(y.sim, 1, quantile, probs= 0.7),
                      "60%" = apply(y.sim, 1, quantile, probs= 0.6),
                      "50%" = apply(y.sim, 1, quantile, probs= 0.5),
                      "40%" = apply(y.sim, 1, quantile, probs= 0.4),
                      "30%" = apply(y.sim, 1, quantile, probs= 0.3),
                      "20%" = apply(y.sim, 1, quantile, probs= 0.2),
                      "10%" = apply(y.sim, 1, quantile, probs= 0.1),
                      "Min" = apply(y.sim, 1, min),
                      "Mean" = apply(y.sim, 1, mean))

sim.test <- data.frame(Date = obs.data$Date,
                       Year = obs.data$Year,
                       Month = obs.data$Month,
                       Observed = obs.data$events.freq,
                       y.sim,
                       sim.test)

test.monthly <- aggregate(Observed ~ Year + Month, data= sim.test, FUN = sum)
test.monthly <- test.monthly[order(test.monthly$Year,test.monthly$Month),]    # reorder the month and year
test.monthly.sim <- aggregate(y.sim ~ Year + Month, data= sim.test, sum)
test.monthly.sim <- test.monthly.sim[order(test.monthly.sim$Year, test.monthly.sim$Month),]
test.monthly.sim$MIN<-apply(test.monthly.sim[,3:102], 1, min)
test.monthly.sim$lower <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.25)
test.monthly.sim$median <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.50)
test.monthly.sim$upper <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.75)
test.monthly.sim$MAX <- apply(test.monthly.sim[,3:102], 1, max)
test.monthly.sim$mean <- apply(test.monthly.sim[,3:102], 1, mean)

### Monthly
col.scale<- rev( heat.colors(5, alpha =1))
plot(ts(test.monthly$Observed, frequency = 12,start = c(1982,6)),type="l",ylim=c(0,15), lwd=5, xlab="Year",
     ylab="Number of days", main = "Number of days (monthly) that has a new TS event in 1982-2016")
polygon(ts(test.monthly.sim$MIN, frequency = 12,start = c(1982,6)), 
        col=col.scale[1], border = FALSE)
polygon(ts(test.monthly.sim$lower, frequency = 12,start = c(1982,6)), 
        col=col.scale[2], border = FALSE)
polygon(ts(test.monthly.sim$median, frequency = 12,start = c(1982,6)), 
        col=col.scale[3], border = FALSE)
polygon(ts(test.monthly.sim$upper, frequency = 12,start = c(1982,6)), 
        col=col.scale[4], border = FALSE)
polygon(ts(test.monthly.sim$MAX, frequency = 12,start = c(1982,6)), 
        col=col.scale[5], border = FALSE)
points(ts(test.monthly$Observed, frequency = 12,start = c(1982,6)),type="b", lwd=3)
dev.copy(pdf,"All3_checking_monthlysum.pdf", height=6, width=8)
dev.off()

### Annually
test.annually <- aggregate(sim.test[,4:104], by= list(sim.test$Year), sum)
test.annually$MIN<- apply(test.annually[,3:102], 1, min)
test.annually$lower <- apply(test.annually[,3:102], 1, quantile, probs= 0.25)
test.annually$median <- apply(test.annually[,3:102], 1, quantile, probs= 0.50)
test.annually$upper <- apply(test.annually[,3:102], 1, quantile, probs= 0.75)
test.annually$MAX <- apply(test.annually[,3:102], 1, max)
test.annually$mean <- apply(test.annually[,3:102], 1, mean)
names(test.annually)[1]<- "Year"


#### This one looks good!
plot(test.annually$Year, test.annually$Observed, type="o", lwd=5, ylim=c(10,50),xlab="Year",
     ylab="Number of days", main = "Number of days that has a new TS event in 1982-2016")
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$MIN,rev(test.annually$lower)), col = col.scale[1], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$lower,rev(test.annually$median)), col = col.scale[2], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$median,rev(test.annually$upper)), col = col.scale[3], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$upper,rev(test.annually$MAX)), col = col.scale[4], border = NA)
points(test.annually$Year,test.annually$mean, type="l", lwd=3, col="blue")
points(test.annually$Year, test.annually$Observed, type="o", lwd=5)
legend("topright", inset=.001, title="Quantiles", c("Observed","Min","0.25","0.5","0.75","Max","Mean"), 
       col = c("Black",col.scale,"blue"),
       lty=1,text.width = 1,cex=0.5,horiz=FALSE)
dev.copy(pdf,"All3_checking_yearsum.pdf", height=6, width=8)
dev.off()



##### probability integral tranform
pit.hist<- rowMeans(y.sim < sim.test$Observed)
hist(pit.hist)

dev.copy(pdf,"Daily PIT.pdf",width=8,height=6)
dev.off()

## monthly
#pit.mon.hist<- rowMeans(test.monthly.sim[,3:102] < test.monthly$Observed)
#hist(pit.mon.hist)

pit.mon.hist<- rowMeans(test.monthly.sim[test.monthly.sim$Month %in% (6:11),][,3:102] < test.monthly[test.monthly$Month %in% (6:11),]$Observed)
hist(pit.mon.hist)

dev.copy(pdf,"All3 Monthly PIT.pdf",width=8,height=6)
dev.off()

## annual
pit.annual.hist <- rowMeans(test.annually[,3:102] < test.annually$Observed)
hist(pit.annual.hist, breaks = c(0,0.2,0.4,0.6,0.8,1))
dev.copy(pdf,"All3 Annual PIT.pdf",width=8,height=6)
dev.off()

##### Pt - daily

Pt <- rowMeans(y.sim > 0)

## REC
Pt.group <- cut(Pt,breaks=seq(0,1,0.05),include.lowest = TRUE, right=FALSE)
Obs.tab <- tapply(sim.test$Observed, INDEX = Pt.group, FUN=function(x) {sum(x>0)})
Exp.tab <- tapply(Pt, INDEX = Pt.group, FUN=function(x) {sum(x)})

rbind(Obs.tab,Exp.tab)

for (i in seq(0,1,0.1)){
  cat("Expected:",sum(Pt[Pt >=i & Pt < i+0.1]),"\n")
  cat("Observed:",sum(sim.test[Pt >= i & Pt < i+0.1,]$Observed>0),"\n")
}

sum(sim.test$Observed > 0)
sum(Pt)



##### lag 6 added
x.day <- data.frame(BST.daily[-(1:151), (1:7)])   # Remove the NA for SST lag
x.day$dummy.p1 <- BST.daily$dummy.p1[-(1:151)]
x.day$dummy.p2 <- BST.daily$dummy.p2[-(1:151)]
x.day$events.lag.1<- 0
x.day$events.lag.2<- 0
x.day$events.lag.3<- 0
x.day$events.lag.4 <- 0
x.day$events.lag.5 <- 0
x.day$events.lag.6 <- 0

pred.core <- predict(BST.daily.glm.10, newdata = x.day)

n <- length(x.day$Day)
n.sims <- 100
y.lag1<- rep(0, n.sims)
y.lag2<- rep(0, n.sims)
y.lag3<- rep(0, n.sims)
y.lag4<- rep(0, n.sims)
y.lag5<- rep(0, n.sims)
y.lag6<- rep(0, n.sims)

y.hat<- exp(pred.core[1] + coef(BST.daily.glm.10)[5] * y.lag1 + coef(BST.daily.glm.10)[6] * y.lag2 
            + coef(BST.daily.glm.10)[7] * y.lag3 + coef(BST.daily.glm.10)[8] * y.lag4 +
              coef(BST.daily.glm.10)[9] * y.lag5 + coef(BST.daily.glm.10)[10] * y.lag6)
y.sim <- array (NA, c(n, n.sims)) 
y.sim[1,] <- rpois(n.sims, y.hat)

y.lag6 <- y.lag5
y.lag5 <- y.lag4
y.lag4 <- y.lag3
y.lag3 <- y.lag2
y.lag2 <- y.lag1
y.lag1 <- y.sim[1,]


i = 1

while (i < n){
  i = i+1
  y.hat <- exp(pred.core[i] + coef(BST.daily.glm.10)[5] * y.lag1 + coef(BST.daily.glm.10)[6] * y.lag2 
               + coef(BST.daily.glm.10)[7] * y.lag3 + coef(BST.daily.glm.10)[8] * y.lag4 +
                 coef(BST.daily.glm.10)[9] * y.lag5 + coef(BST.daily.glm.10)[10] * y.lag6)
  y.sim[i,]<- rpois(n.sims, y.hat)
  y.lag6 <- y.lag5
  y.lag5 <- y.lag4
  y.lag4 <- y.lag3
  y.lag3 <- y.lag2
  y.lag2 <- y.lag1
  y.lag1 <- y.sim[i,]
}
### OMG!!!!! THAT'S AMAZINGLY FAST!!!!!!!!!!!!!
### A-M-A-Z-I-N-G-!-!-!-! 1 SEC ONLY!!!!!!!!!!!


###### Assessment
obs.data <- BST.daily[-(1:151),]
plot(obs.data$Date,obs.data$events.freq)
plot(y.sim[,1])  # WOW
head(y.sim)

sim.test<- data.frame("Max" = apply (y.sim, 1, max),
                      "90%" = apply(y.sim, 1, quantile, probs= 0.9),
                      "80%" = apply(y.sim, 1, quantile, probs= 0.8),
                      "70%" = apply(y.sim, 1, quantile, probs= 0.7),
                      "60%" = apply(y.sim, 1, quantile, probs= 0.6),
                      "50%" = apply(y.sim, 1, quantile, probs= 0.5),
                      "40%" = apply(y.sim, 1, quantile, probs= 0.4),
                      "30%" = apply(y.sim, 1, quantile, probs= 0.3),
                      "20%" = apply(y.sim, 1, quantile, probs= 0.2),
                      "10%" = apply(y.sim, 1, quantile, probs= 0.1),
                      "Min" = apply(y.sim, 1, min),
                      "Mean" = apply(y.sim, 1, mean))

sim.test <- data.frame(Date = obs.data$Date,
                       Year = obs.data$Year,
                       Month = obs.data$Month,
                       Observed = obs.data$events.freq,
                       y.sim,
                       sim.test)

test.monthly <- aggregate(Observed ~ Year + Month, data= sim.test, FUN = sum)
test.monthly <- test.monthly[order(test.monthly$Year,test.monthly$Month),]    # reorder the month and year
test.monthly.sim <- aggregate(y.sim ~ Year + Month, data= sim.test, sum)
test.monthly.sim <- test.monthly.sim[order(test.monthly.sim$Year, test.monthly.sim$Month),]
test.monthly.sim$MIN<-apply(test.monthly.sim[,3:102], 1, min)
test.monthly.sim$lower <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.25)
test.monthly.sim$median <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.50)
test.monthly.sim$upper <- apply(test.monthly.sim[,3:102], 1, quantile, probs= 0.75)
test.monthly.sim$MAX <- apply(test.monthly.sim[,3:102], 1, max)
test.monthly.sim$mean <- apply(test.monthly.sim[,3:102], 1, mean)

### Monthly
col.scale<- rev( heat.colors(5, alpha =1))
plot(ts(test.monthly$Observed, frequency = 12,start = c(1982,6)),type="l",ylim=c(0,15), lwd=5, xlab="Year",
     ylab="Number of days", main = "Number of days (monthly) that has a new TS event in 1982-2016")

polygon(ts(test.monthly.sim$MIN, frequency = 12,start = c(1982,6)), 
        col=col.scale[1], border = FALSE)
polygon(ts(test.monthly.sim$lower, frequency = 12,start = c(1982,6)), 
        col=col.scale[2], border = FALSE)
polygon(ts(test.monthly.sim$median, frequency = 12,start = c(1982,6)), 
        col=col.scale[3], border = FALSE)
polygon(ts(test.monthly.sim$upper, frequency = 12,start = c(1982,6)), 
        col=col.scale[4], border = FALSE)
polygon(ts(test.monthly.sim$MAX, frequency = 12,start = c(1982,6)), 
        col=col.scale[5], border = FALSE)
points(ts(test.monthly$Observed, frequency = 12,start = c(1982,6)),type="b", lwd=3)
dev.copy(pdf,"All6_checking_monthlysum.pdf", height=6, width=8)
dev.off()

### Annually
test.annually <- aggregate(sim.test[,4:104], by= list(sim.test$Year), sum)
test.annually$MIN<- apply(test.annually[,3:102], 1, min)
test.annually$lower <- apply(test.annually[,3:102], 1, quantile, probs= 0.25)
test.annually$median <- apply(test.annually[,3:102], 1, quantile, probs= 0.50)
test.annually$upper <- apply(test.annually[,3:102], 1, quantile, probs= 0.75)
test.annually$MAX <- apply(test.annually[,3:102], 1, max)
test.annually$mean <- apply(test.annually[,3:102], 1, mean)
names(test.annually)[1]<- "Year"


#### This one looks good!
plot(test.annually$Year, test.annually$Observed, type="o", lwd=5, ylim=c(10,50),xlab="Year",
     ylab="Number of days", main = "Number of days that has a new TS event in 1982-2016")
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$MIN,rev(test.annually$lower)), col = col.scale[1], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$lower,rev(test.annually$median)), col = col.scale[2], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$median,rev(test.annually$upper)), col = col.scale[3], border = NA)
polygon(c(test.annually$Year,rev(test.annually$Year)),
        c(test.annually$upper,rev(test.annually$MAX)), col = col.scale[4], border = NA)
points(test.annually$Year,test.annually$mean, type="l", lwd=3, col="blue")
points(test.annually$Year, test.annually$Observed, type="o", lwd=5)
legend("topright", inset=.001, title="Quantiles", c("Observed","Min","0.25","0.5","0.5","Max","Mean"), 
       col = c("Black",col.scale,"blue"),
       lty=1,text.width = 1,cex=0.5,horiz=FALSE)
dev.copy(pdf,"All6_checking_yearsum.pdf", height=6, width=8)
dev.off()



##### probability integral tranform
pit.hist<- rowMeans(y.sim < sim.test$Observed)
hist(pit.hist)

dev.copy(pdf,"Daily PIT.pdf",width=8,height=6)
dev.off()

## monthly
#pit.mon.hist<- rowMeans(test.monthly.sim[,3:102] < test.monthly$Observed)
#hist(pit.mon.hist)

pit.mon.hist<- rowMeans(test.monthly.sim[test.monthly.sim$Month %in% (6:11),][,3:102] < test.monthly[test.monthly$Month %in% (6:11),]$Observed)
hist(pit.mon.hist)

dev.copy(pdf,"All6 Monthly PIT.pdf",width=8,height=6)
dev.off()

## annual
pit.annual.hist <- rowMeans(test.annually[,3:102] < test.annually$Observed)
hist(pit.annual.hist, breaks = c(0,0.2,0.4,0.6,0.8,1))
dev.copy(pdf,"All6 Annual PIT.pdf",width=8,height=6)
dev.off()

##### Pt - daily

Pt <- rowMeans(y.sim > 0)

## REC
Pt.group <- cut(Pt,breaks=seq(0,1,0.05),include.lowest = TRUE, right=FALSE)
Obs.tab <- tapply(sim.test$Observed, INDEX = Pt.group, FUN=function(x) {sum(x>0)})
Exp.tab <- tapply(Pt, INDEX = Pt.group, FUN=function(x) {sum(x)})

rbind(Obs.tab,Exp.tab)

for (i in seq(0,1,0.1)){
  cat("Expected:",sum(Pt[Pt >=i & Pt < i+0.1]),"\n")
  cat("Observed:",sum(sim.test[Pt >= i & Pt < i+0.1,]$Observed>0),"\n")
}

sum(sim.test$Observed > 0)
sum(Pt)