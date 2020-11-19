toyota <- read.csv(file.choose())
View(toyota)
Corolla<-toyota[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
summary(Corolla)
cor(Corolla)
cor2pcor(cor(Corolla))
model.corolla <- lm(Price~.,data=Corolla)
summary(model.corolla)
# cc and doors are insignificant

model.corollacc <- lm(Price~cc,data=Corolla)
summary(model.corollacc) #became significant

model.corolladoors <- lm(Price~Doors,data=Corolla)
summary(model.corolladoors) #became insignificant

model.corolla2 <- lm(Price~cc+Doors,data= Corolla)
summary(model.corolla2)# became insignificant

vif(model.corolla)
#vif<10,no multicollinearity exists

avPlots(model.corolla,id.n=2,id.cex=0.7)

influence.measures(model.corolla)
library(car)
## plotting Influential measures 

windows()
influenceIndexPlot(model.corolla,id.n=3) # index plots for infuence measures
influencePlot(model.corolla,id.n=3)
model_1<-lm(Price~.-Doors,data=Corolla[-81,])
summary(model_1)

model_2 <-lm(Price~.-Doors,data=Corolla[-c(81,222),])
 
summary(model_2)                                        

model_3<-lm(Price~.-Doors,data=Corolla[-c(222,81,961),])
summary(model_3)

finalmodel <- model_3 # value of R squared is high.
summary(finalmodel)
windows()

plot(finalmodel)




hist(residuals(finalmodel))









