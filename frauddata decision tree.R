fraud <- read.csv(file.choose())
View(fraud)

taxincome <- ifelse(fraud$Taxable.Income <=30000 ,"Risky","good")
fraud <- cbind(fraud,taxincome)
fraud$Taxable.Income <- NULL
dim(fraud)
fraud_train <- fraud[1:300,]
fraud_test <- fraud[301:600,]
library(C50)
fraudc5.0_train <- C5.0(fraud_train[,-6],fraud_train$taxincome)

plot(fraudc5.0_train)
pred_train <- predict(fraudc5.0_train,fraud_train)


mean(fraud_train$taxincome==pred_train) #76.6% accuracy

library(tree)
# Building a model on training data 
fraud_tree <- tree(taxincome~.,data=fraud_train)
plot(fraud_tree)
text(fraud_tree,pretty = 0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(fraud_tree,newdata=fraud_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(fraud_tree,newdata=fraud_test)

# for (i in 1:nrow(pred_tree)){
#   pred_tree[i,"final"]<-ifelse(pred_tree[i,"setosa"]>0.5,"setosa",ifelse(pred_tree[i,"versicolor"]>0.5,"versicolor","virginica"))
# }
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]


mean(pred_tree$final==fraud_test$taxincome) # Accuracy = 94.66%
CrossTable(fraud_test$taxincome,pred_tree$final)
