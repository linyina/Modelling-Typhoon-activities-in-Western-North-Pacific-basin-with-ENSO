#######################################################################################
#####                   Step 4-2 Modelling on the daily data                      #####
#####                                                                             #####
##### Consider storm occurence on a daily basis. Let there be a probability p of  #####
##### observing a storm on any given day. If events on different days are indepe- #####
##### ent => the number of storms in a month will follow Bin (30,p) => This can   #####
##### be approximated by a Poisson distribution with mean 30p.                    #####
#######################################################################################

### Forward selection!
### First, conduct a model without seasonality
BST.daily.glm.0 <- glm(events.freq ~ Nino3.4 + events.lag.1 + events.lag.2,
                       data= BST.daily, family = poisson(link = "log"))
summary(BST.daily.glm.0)
anova(BST.daily.glm.0, test="Chi")

BST.daily.glm.0a<- update(BST.daily.glm.0, .~. + TNI)
anova(BST.daily.glm.0, BST.daily.glm.0a,test="Chi") # GOOD!

### Add seasonality dummy variables in it.
BST.daily.glm.1 <- update(BST.daily.glm.0a, .~. + dummy.p1+dummy.p2)
summary(BST.daily.glm.1)
anova(BST.daily.glm.0a, BST.daily.glm.1, test="Chi")  # Not surprisingly
disp(BST.daily.glm.1)  # Doing quiet well
cook.plot(BST.daily.glm.1)
dev.copy(pdf,"II_glm1_cookplot.pdf", height=6, width=8)
dev.off()
dev.res(BST.daily.glm.1)
plot(BST.daily.glm.1)


BST.daily.glm.2 <- glm(events.freq ~ Nino3.4Lag5 + TNILag5+ dummy.p1 + dummy.p2 + events.lag.1 + events.lag.2,
                       data= BST.daily, family = poisson(link = "log"))
summary(BST.daily.glm.2)
anova(BST.daily.glm.2, test="Chi")

plot(BST.daily.glm.2)
disp(BST.daily.glm.2)
dev.res(BST.daily.glm.2)
cook.plot(BST.daily.glm.2)
dev.copy(pdf,"II_glm2_cookplot.pdf", height=6, width=8)
dev.off()

##### interactions of sst and seasonality
BST.daily.glm.3 <- update(BST.daily.glm.2, .~. + Nino3.4Lag5:dummy.p1 + Nino3.4Lag5:dummy.p2)
summary(BST.daily.glm.3)

anova(BST.daily.glm.2, BST.daily.glm.3, test = "Chi")
disp(BST.daily.glm.3)
dev.res(BST.daily.glm.3)
cook.plot(BST.daily.glm.3)
dev.copy(pdf,"II_glm3_cookplot.pdf", height=6, width=8)
dev.off()
drop1(BST.daily.glm.3)


#############################################
##### Model improving 
############################################
##### interactions of lag1 and lag2
BST.daily.glm.4<- update(BST.daily.glm.3, .~. + events.lag.1:events.lag.2)
summary(BST.daily.glm.4)
anova(BST.daily.glm.3, BST.daily.glm.4, test = "Chi")   # no use!


##### All combinations of interactions
BST.daily.glm.5<- glm(events.freq ~ (Nino3.4Lag5 + dummy.p1 + dummy.p2 + events.lag.1 + events.lag.2 + TNILag5)^2,
                      data= BST.daily, family = poisson(link = "log"))
summary(BST.daily.glm.5)
anova(BST.daily.glm.3, BST.daily.glm.5, test = "Chi") # Uh interesting. No use
drop1(BST.daily.glm.5)  # Nothing very important...

#### interactions of seasonality and lags
BST.daily.glm.6 <- update(BST.daily.glm.3, .~. + dummy.p1:events.lag.1 + dummy.p1:events.lag.2 
                          + dummy.p2:events.lag.1 + dummy.p2:events.lag.2)
summary(BST.daily.glm.6)
anova(BST.daily.glm.3, BST.daily.glm.6, test="Chi")  # .. no use!

#### Adding lag 3
BST.daily$events.lag.3<- c(0,BST.daily$events.lag.2[1:12783])
BST.daily.glm.lag3 <- update(BST.daily.glm.3, .~. + events.lag.3)
summary(BST.daily.glm.lag3)
anova(BST.daily.glm.3, BST.daily.glm.lag3, test = "Chi") # Not very good!

#### Adding lag 4.5.6.7..
BST.daily$events.lag.4<- c(0,BST.daily$events.lag.3[1:12783])
BST.daily.glm.8 <- update(BST.daily.glm.7, .~. + events.lag.4)
summary(BST.daily.glm.8)
anova(BST.daily.glm.7, BST.daily.glm.8, test = "Chi")  # could be used but not significant

BST.daily$events.lag.5<- c(0,BST.daily$events.lag.4[1:12783])
BST.daily.glm.9 <- update(BST.daily.glm.8, .~. + events.lag.5)
summary(BST.daily.glm.9)
anova(BST.daily.glm.8, BST.daily.glm.9, test = "Chi") # Good!

BST.daily$events.lag.6<- c(0,BST.daily$events.lag.5[1:12783])
BST.daily.glm.10 <- update(BST.daily.glm.9, .~. + events.lag.6)
summary(BST.daily.glm.10)
anova(BST.daily.glm.9, BST.daily.glm.10, test = "Chi") # 0.05 level

BST.daily$events.lag.7<- c(0,BST.daily$events.lag.6[1:12783])
BST.daily.glm.11 <- update(BST.daily.glm.10, .~. + events.lag.7)
summary(BST.daily.glm.11)
anova(BST.daily.glm.10, BST.daily.glm.11, test = "Chi")  # No use

#### Let's try stopping till lag.3 - BST.daily.glm.7
#### And till lag.6 - BST.daily.glm.10  for simulation
