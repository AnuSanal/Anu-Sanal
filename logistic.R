getwd()
setwd("D:/downloads")
#loading data
df <- read.csv2('bank-full.csv')
View(df)
str(df)
is.na(df)
sum(is.na(df))
summary(df)
dim(df)
model <- glm(y~.,data = df,family = "binomial")
exp(coef(model))
prob <- predict(model,df,type = "response")
summary(model)
finpred <- ifelse(prob>0.5,1,0)
finpred
library(caret)
con <- confusionMatrix(as.factor(finpred),as.factor(df$y))
con
confusion <- table(prob>0.5,df$y)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")
df[,"prob"] <- prob
df[,"pred_values"] <- pred_values
df[,"yes_no"] <- yes_no

View(df)


table(df$y,df$pred_values)
library(ROCR)
rocrpred<-prediction(prob,df$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
