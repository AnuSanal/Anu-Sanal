#read the data set
zoo <- read.csv(file.choose())
View(zoo)

dim(zoo)
zoo_train <- zoo[1:90,]
zoo_test <- zoo[91:101,]

library("class")
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,52,2))
{
  train_zoo_pred <- knn(train=zoo_train,test=zoo_train,cl=zoo_train,k=i)
  train_acc <- c(train_acc,mean(train_zoo_pred==zoo_train))
  test_zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train, k=i)
  test_acc <- c(test_acc,mean(test_zoo_pred==zoo_test))
}
