glass <- read.csv(file.choose())
View(glass)
summary(glass)
dim(glass)
table(glass$Type)
round(prop.table(table(glass$Type))*100,1)
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
glass_train <- glass_n[1:140,]
glass_test <- glass_n[141:214,]
glass_train_labels <- glass[1:140,10]
glass_test_labels <- glass[141:214,10]
library("class")
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_glass_pred <- knn(train=glass_train,test=glass_train,cl=glass_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==glass_train_labels))
  test_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==glass_test_labels))
}
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)
