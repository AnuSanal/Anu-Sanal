affair <- read.csv(file.choose())
a <- affair$affairs
Affair <- ifelse(a!=0,1,0)
View(Affair)

affair <- cbind(affair,Affair)
View(affair)
affair$affairs <- NULL
View(affair)
str(affair)
affair$gender <- as.integer(affair$gender)
affair$yearsmarried <- as.integer(affair$yearsmarried)
affair$children <- as.integer(affair$children)
affair$age <- as.integer(affair$age)
affair$Affair <- as.factor(affair$Affair)
str(affair)
is.na(affair)
sum(is.na(affair))
summary(affair)
dim(affair)
model <- glm(Affair~.,data = affair,family = "binomial")
#The fitted model is, 
#Affair =log(0.77390)+ log (0.28(gender)) - log (0.045(age))+ log (0.09(years married))- log (0.37(children)) - log (0.32(religiousness)) + log (0.02(education)) + log (0.030(occupation)) - log (0.0468(rating))

exp(coef(model))
prob <- predict(model,affair,type = "response")
summary(model)
finpred <- ifelse(prob>0.5,1,0)
finpred
library(caret)
con <- confusionMatrix(as.factor(finpred),affair$Affair)
con
confusion <- table(prob>0.5,affair$Affair)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")
affair[,"prob"] <- prob
affair[,"pred_values"] <- pred_values
affair[,"yes_no"] <- yes_no

View(affair)


table(affair$Affair,affair$pred_values)
library(ROCR)
rocrpred<-prediction(prob,affair$Affair)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
##now we get the classified model. To increase the accuracy and reduce AIC value, we can apply transformations such as additive, multiplicative and then analysis can be carried out. The model with highest accuracy and least AIC value may be considered as the best model.
