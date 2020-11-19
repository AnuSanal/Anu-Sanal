fraud_check <- read.csv(file.choose())
View(fraud_check)
taxincome <- ifelse(fraud_check$Taxable.Income <=30000 ,"Risky","good")
fraud <- cbind(fraud_check,taxincome)
fraud$Taxable.Income <- NULL
dim(fraud)
View(fraud)
fraud_norm <- scale(fraud[,3:4])
fraud1 <- cbind(fraud_norm,fraud[1:2])

fraud <- cbind(fraud1,fraud[,5:6])
fraud_train <- fraud[1:300,]
fraud_test <- fraud[301:600,]
library(randomForest)
fraud_rf <- randomForest(taxincome~.,data= fraud_train)
plot(fraud_rf)
pred_rf <- predict(fraud_rf,fraud_test)
plot(pred_rf)



varImpPlot(fraud_rf)
