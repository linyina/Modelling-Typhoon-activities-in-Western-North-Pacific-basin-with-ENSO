########### REconstructing data for intensity >=2
events.binary.2<- BST.date[BST.date$Intensity>=2,]
BST.date.2<-merge(BST.SN.freq[BST.SN.freq$Intensity>=2,], BST.events)
BST.date.2$date <- as.Date(paste0(BST.date.2$Year,"-",BST.date.2$Month, "-",BST.date.2$Date))
BST.date.2<-BST.date.2[942:1834,]
BST.date.2 <- merge(df.daily, BST.date.2, all=TRUE)
BST.date.2[is.na(BST.date.2$freq),]$freq<- 0
BST.date.2$Year <- substr(BST.date.2$date, start = 1, stop= 4)
BST.date.2$Date <- substr(BST.date.2$date, start = 9, stop= 10)
events.2 <- aggregate(freq ~ date, BST.date.2, sum)
events.binary.2 <- aggregate(freq ~ date, BST.date.2, max)

BST.daily.2<- data.frame(Date = events.2$date,
                          Year= substr(events.2$date, start = 1, stop= 4),
                          Month = substr(events.2$date, start = 6, stop= 7),
                          Day = substr(events.2$date, start = 9, stop= 10),
                          events.freq = events.2$freq,
                          events.lag.1 = c(0, events.binary.2$freq[1:12783]),
                          events.lag.2 = c(0,0, events.binary.2$freq[1:12782]))
BST.daily.2$Month <- as.integer(BST.daily.2$Month)
BST.daily.2 <- merge(Nino34,BST.daily.2, sort = FALSE)
BST.daily.2$dayofyear <- Cumdays[BST.daily.2$Month] + as.integer(BST.daily.2$Day)
BST.daily.2$dummy.p1 <- cos(2*pi*BST.daily.2$dayofyear / 366)
BST.daily.2$dummy.p2 <- sin(2*pi*BST.daily.2$dayofyear / 366)


############## Modelling again
BST.daily2.glm0<-glm(formula = events.freq ~ Nino3.4Lag5 + dummy.p1 + dummy.p2 + 
      events.lag.1 + events.lag.2, family = poisson(link = "log"), 
    data = BST.daily.2)
summary(BST.daily2.glm0)
### Adding nino3.4Lag5 and dummy.p1 interaction
BST.daily2.glm1<-glm(formula = events.freq ~ Nino3.4Lag5 + dummy.p1 + dummy.p2 + 
      events.lag.1 + events.lag.2 + Nino3.4Lag5:dummy.p1 + Nino3.4Lag5:dummy.p2, 
    family = poisson(link = "log"), data = BST.daily.2)
summary(BST.daily2.glm1)
anova(BST.daily2.glm0, BST.daily2.glm1, test = "Chi")

## Combination of all interactions
BST.daily2.glm2<-glm(events.freq ~ (Nino3.4Lag5 + dummy.p1 + dummy.p2 + events.lag.1 + events.lag.2)^2,
    data= BST.daily.2, family = poisson(link = "log"))
summary(BST.daily2.glm2)
anova(BST.daily2.glm1, BST.daily2.glm2, test = "Chi") # Still No use
drop1(BST.daily2.glm2)  # Nino3.4Lag5:events.lag.2

BST.daily2.glm2a<- update(BST.daily2.glm1, .~. + TNI)
summary(BST.daily2.glm2a)
anova(BST.daily2.glm1,BST.daily2.glm2a, test='Chi')
drop1(BST.daily2.glm2a)

BST.daily2.glm2b<- update(BST.daily2.glm2a, .~. + TNI:dummy.p1 + TNI:dummy.p2)
summary(BST.daily2.glm2b)
anova(BST.daily2.glm2a,BST.daily2.glm2b, test='Chi')
drop1(BST.daily2.glm2b)

BST.daily2.glm3<- update(BST.daily2.glm1, .~. + Nino3.4Lag5:events.lag.2)
summary(BST.daily2.glm3)
anova(BST.daily2.glm1, BST.daily2.glm3, test="Chi") # .. No



#### Adding lag 3
BST.daily.2$events.lag.3<- c(0,BST.daily.2$events.lag.2[1:12783])
BST.daily2.glm4 <- update(BST.daily2.glm2b, .~. + events.lag.3)
summary(BST.daily2.glm4)
anova(BST.daily2.glm1, BST.daily2.glm4, test = "Chi") # good!

#### Adding lag 4.5.6.7..
BST.daily.2$events.lag.4<- c(0,BST.daily.2$events.lag.3[1:12783])
BST.daily2.glm5 <- update(BST.daily2.glm4, .~. + events.lag.4)
summary(BST.daily2.glm5)
anova(BST.daily2.glm4, BST.daily2.glm5, test = "Chi")  # Not significant

BST.daily.2$events.lag.5<- c(0,BST.daily.2$events.lag.4[1:12783])
BST.daily2.glm6 <- update(BST.daily2.glm5, .~. + events.lag.5)
summary(BST.daily2.glm6)
anova(BST.daily2.glm5, BST.daily2.glm6, test = "Chi") # Good!

BST.daily.2$events.lag.6<- c(0,BST.daily.2$events.lag.5[1:12783])
BST.daily2.glm7 <- update(BST.daily2.glm6, .~. + events.lag.6)
summary(BST.daily2.glm7)
anova(BST.daily2.glm5, BST.daily2.glm7, test = "Chi") # Good

BST.daily.2$events.lag.7<- c(0,BST.daily.2$events.lag.6[1:12783])
BST.daily2.glm8 <- update(BST.daily2.glm7, .~. + events.lag.7)
summary(BST.daily2.glm8)
anova(BST.daily2.glm8, BST.daily2.glm7, test = "Chi")  # No use

#### Let's try stopping till lag.3 - BST.daily2.glm4
#### And till lag.6 - BST.daily2.glm7  for simulation


####### lag 3 added

x.day <- data.frame(BST.daily.2[-(1:151), (1:8)])   # Remove the NA for SST lag
x.day$dummy.p1 <- BST.daily.2$dummy.p1[-(1:151)]
x.day$dummy.p2 <- BST.daily.2$dummy.p2[-(1:151)]
x.day$events.lag.1<- 0
x.day$events.lag.2<- 0
x.day$events.lag.3<- 0

pred.core <- predict(BST.daily2.glm4, newdata = x.day)

n <- length(x.day$Day)
n.sims <- 100
y.lag1<- rep(0, n.sims)
y.lag2<- rep(0, n.sims)
y.lag3<- rep(0, n.sims)

y.hat<- exp(pred.core[1] + coef(BST.daily2.glm4)[5] * y.lag1 + coef(BST.daily2.glm4)[6] * y.lag2 +
              coef(BST.daily2.glm4)[8] * y.lag3)
y.sim <- array (NA, c(n, n.sims)) 
y.sim[1,] <- rpois(n.sims, y.hat)
y.lag <- array (NA, c(n,n.sims))
y.lag[1,] <- y.sim[1,]
y.lag[1,][y.lag[1,]>=1]<-1
y.lag3 <- y.lag2
y.lag2 <- y.lag1
y.lag1 <- y.sim[1,]


i = 1

while (i < n){
  i = i+1
  y.hat <- exp(pred.core[i] + coef(BST.daily2.glm4)[5] * y.lag1 + coef(BST.daily2.glm4)[6] * y.lag2+
                 coef(BST.daily2.glm4)[7] * y.lag3)
  y.sim[i,]<- rpois(n.sims, y.hat)
  y.lag[i,]<- y.sim[i,]
  y.lag[i,][y.lag[i,]>=1]<-1
  y.lag3 <- y.lag2
  y.lag2 <- y.lag1
  y.lag1 <- y.sim[i,]
}



###### Assessment
obs.data <- BST.daily.2[-(1:151),]
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
dev.copy(pdf,"TS3_checking_monthlysum.pdf", height=6, width=8)
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
       lty=1,text.width = 0.5,cex=0.4,horiz=FALSE)
dev.copy(pdf,"TS3_checking_yearsum.pdf", height=6, width=8)
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

dev.copy(pdf,"TS3 Monthly PIT.pdf",width=8,height=6)
dev.off()

## annual
pit.annual.hist <- rowMeans(test.annually[,3:102] < test.annually$Observed)
hist(pit.annual.hist, breaks = c(0,0.2,0.4,0.6,0.8,1))
dev.copy(pdf,"TS3 Annual PIT.pdf",width=8,height=6)
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
x.day <- data.frame(BST.daily.2[-(1:151), (1:7)])   # Remove the NA for SST lag
x.day$dummy.p1 <- BST.daily.2$dummy.p1[-(1:151)]
x.day$dummy.p2 <- BST.daily.2$dummy.p2[-(1:151)]
x.day$events.lag.1<- 0
x.day$events.lag.2<- 0
x.day$events.lag.3<- 0
x.day$events.lag.4 <- 0
x.day$events.lag.5 <- 0
x.day$events.lag.6 <- 0

pred.core <- predict(BST.daily2.glm7, newdata = x.day)

n <- length(x.day$Day)
n.sims <- 100
y.lag1<- rep(0, n.sims)
y.lag2<- rep(0, n.sims)
y.lag3<- rep(0, n.sims)
y.lag4<- rep(0, n.sims)
y.lag5<- rep(0, n.sims)
y.lag6<- rep(0, n.sims)

y.hat<- exp(pred.core[1] + coef(BST.daily2.glm7)[5] * y.lag1 + coef(BST.daily2.glm7)[6] * y.lag2 
            + coef(BST.daily2.glm7)[7] * y.lag3 + coef(BST.daily2.glm7)[8] * y.lag4 +
              coef(BST.daily2.glm7)[9] * y.lag5 + coef(BST.daily2.glm7)[10] * y.lag6)
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
  y.hat <- exp(pred.core[i] + coef(BST.daily2.glm7)[5] * y.lag1 + coef(BST.daily2.glm7)[6] * y.lag2 
               + coef(BST.daily2.glm7)[7] * y.lag3 + coef(BST.daily2.glm7)[8] * y.lag4 +
                 coef(BST.daily2.glm7)[9] * y.lag5 + coef(BST.daily2.glm7)[10] * y.lag6)
  y.sim[i,]<- rpois(n.sims, y.hat)
  y.lag6 <- y.lag5
  y.lag5 <- y.lag4
  y.lag4 <- y.lag3
  y.lag3 <- y.lag2
  y.lag2 <- y.lag1
  y.lag1 <- y.sim[i,]
}



###### Assessment
obs.data <- BST.daily.2[-(1:151),]
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
dev.copy(pdf,"TS6_checking_monthlysum.pdf", height=6, width=8)
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
dev.copy(pdf,"TS6_checking_yearsum.pdf", height=6, width=8)
dev.off()



##### probability integral tranform


pit.mon.hist<- rowMeans(test.monthly.sim[test.monthly.sim$Month %in% (6:11),][,3:102] < test.monthly[test.monthly$Month %in% (6:11),]$Observed)
hist(pit.mon.hist)

dev.copy(pdf,"TS6 Monthly PIT.pdf",width=8,height=6)
dev.off()

## annual
pit.annual.hist <- rowMeans(test.annually[,3:102] < test.annually$Observed)
hist(pit.annual.hist, breaks = c(0,0.2,0.4,0.6,0.8,1))
dev.copy(pdf,"TS6 Annual PIT.pdf",width=8,height=6)
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