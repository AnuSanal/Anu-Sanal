SalaryData_Train <- read.csv(file.choose())
summary(SalaryData_Train)
SalaryData_Test <- read.csv(file.choose())
summary(SalaryData_Test)
View(SalaryData_Train)
View(SalaryData_Test)
str(SalaryData_Test)
str(SalaryData_Train)

library(mlbench)

barplot(table(as.factor(SalaryData_Train[,14]),as.factor(SalaryData_Train[,12])),legend=c("<=50k",">50k"))
barplot(table(as.factor(SalaryData_Train[,14]),as.factor(SalaryData_Train[,1])),legend=c("<=50k",">50k"))

plot(as.factor(SalaryData_Train[SalaryData_Train$Salary ==">50k",2]))
plot(as.factor(SalaryData_Train[SalaryData_Train$Salary =="<=50k",2]))

boxplot(SalaryData_Train$age)
boxplot(SalaryData_Train$educationno)



library(e1071)
model_salary <- naiveBayes(SalaryData_Train$Salary~.,data = SalaryData_Train)
pred_salary <- predict(model_salary,SalaryData_Test)
table(pred_salary)
plot(pred_salary)
table(SalaryData_Test$Salary)
mean(pred_salary==SalaryData_Test$Salary)

acc<-NULL


for (i in 1:20){train<-order(runif(350))
set.seed(350)
test<--train
training<-SalaryData_Train[train,]
testing<-SalaryData_Test[test,]
model<-naiveBayes(training$Salary~.,data=training[,-14])
pred<-predict(model,testing[,-14])
acc<-c(acc,mean(pred==testing[,14]))

}
acc


