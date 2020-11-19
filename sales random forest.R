  #reading the data set 
company_data <- read.csv(file.choose())
View(company_data)
str(company_data)


  #introducing dummy variables

urban <- ifelse(company_data$Urban == "Yes",1,0)
company_data <- cbind(company_data,urban)
company_data$Urban <- NULL

US <- ifelse(company_data$US=="Yes",1,0)
company_data$US <- NULL
company_data <- cbind(company_data,US)

library(fastDummies)
library(knitr)
dum<- fastDummies::dummy_cols(company_data,select_columns = "ShelveLoc")
knitr::kable(dum)
company_data <- dum
company_data$ShelveLoc <- NULL
View(company_data)

summary(company_data$Sales)

  #Making the desired column as categorical varibale

Sales <- ifelse(company_data$Sales < 7.496,"Bad","Good") #since 7.496 is mean value
company_data$Sales <- NULL
company_data <- cbind(Sales,company_data)


  #normalizing the values

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
  
company_n <- as.data.frame(lapply(company_data[,2:13], norm))

Sales <- company_data$Sales
company_data <- cbind(Sales,company_n)

library(randomForest)
company_forest <- randomForest(Sales~.,data= company_data,importance =TRUE)
plot(company_forest)
legend("topright",colnames(company_forest$err.rate),col=2:8,cex=0.8,fill=1:3)

acc_company <- mean(company_data$Sales==predict(company_forest))
acc_company  #82% accuracy
varImpPlot(company_forest)

  

  #by spliting the given data set into train and test data and then applies random forest method
company_train <- company_data[1:350,]
company_test <- company_data[351:400,]


fit.forest <- randomForest(Sales~.,data= company_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(company_train$Sales==predict(fit.forest,company_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,company_train)
library(caret)


# Confusion Matrix
confusionMatrix(company_train$Sales, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=company_test)
mean(pred_test==company_test$Sales) # Accuracy = 82 % 


# Confusion Matrix 

confusionMatrix(company_test$Sales, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)




