#loading data
df <- read.csv(file.choose())
View(df)
#the first column denotes the index, so we do not need it
df$X <- NULL
View(df)
#converting into dummy variables
c <- ifelse(df$cd=="yes",1,0)
df <- cbind(df,c)

df <- df[,-6]


m <- ifelse(df$multi=="yes",1,0)
df <- cbind(df,m)
pr <- ifelse(df$premium=="yes",1,0)
df <- cbind(df,pr)
df$multi <- NULL
df$premium <- NULL
View(df)

summary(df)
pairs(df)
cor(df)
library(corpcor)
cor2pcor(cor(df))
model.price <- lm(price~.,data=df)
summary(model.price) #each component is significant and so the model is best.
#the model is price =308+9.32(speed)+0.78(hd)+48.25(ram)+123.1(screen)+0.66(ads)-51.85(trend)+60.91(cc)+104.32(multi)-509.22(premium)
library(mvinfluence)
vif(model.price) # all vif<10, so multicollinearity doesnot exist
avPlots(model.price)
windows()
influenceIndexPlot(model.price,id.n=3) # index plots for infuence measures
influencePlot(model.price,id.n=3)
model1 <- lm(price~.,data = df[-1441,])
summary(model1)
model2 <- lm(price~.,data = df[-c(1441,1701),])
summary(model2)
model3 <- lm(price~.,data = df[-c(1441,1701,3784),])
summary(model3)
model4 <- lm(price~.,data = df[-c(1441,1701,3784,4478),])
summary(model4)
finalmodel <- model2

plot(model2) 

#so the final model is model2,since the R squared value is high.
hist(residuals(finalmodel))


