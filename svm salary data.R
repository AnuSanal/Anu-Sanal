salary_train <- read.csv(file.choose())
View(salary_train)
salary_test <- read.csv(file.choose())
View(salary_test)
dim(salary_test)
str(salary_test)
salary_test$education <- NULL

salary_test$maritalstatus <- NULL
salary_test$relationship <- NULL
salary_test$race <- NULL
salary_test$native <- NULL
library(fastDummies)
library(knitr)
salary_test <- fastDummies::dummy_cols(salary_test,select_columns = "workclass")
knitr::kable(salary_test)
salary_test <- fastDummies::dummy_cols(salary_test,select_columns = "occupation")
knitr::kable(salary_test)
salary_test <- fastDummies::dummy_cols(salary_test,select_columns = "sex")
knitr::kable(salary_test)
View(salary_test)
str(salary_test)

salary_test$workclass <- NULL
salary_test$occupation <- NULL
salary_test$sex <- NULL

salary_train$education <- NULL

salary_train$maritalstatus <- NULL
salary_train$relationship <- NULL
salary_train$race <- NULL
salary_train$native <- NULL

salary_train <- fastDummies::dummy_cols(salary_train,select_columns = "workclass")
knitr::kable(salary_train )
salary_train <- fastDummies::dummy_cols(salary_train,select_columns = "occupation")
knitr::kable(salary_train )
salary_train  <- fastDummies::dummy_cols(salary_train,select_columns = "sex")
knitr::kable(salary_train )
View(salary_train )
str(salary_train )

salary_train $workclass <- NULL
salary_train $occupation <- NULL
salary_train $sex <- NULL
dim(salary_train)
sum(is.na(salary_train))
 
library(kernlab)
library(caret)

#kernel= "vanilladot"
model_van <- ksvm(Salary~.,data = salary_train,kernel = "vanilladot")
pred_van <- predict(model_van, newdata = salary_test)
mean(pred_van == salary_test$Salary) #82.03% accuracy

#kernel = "rbfdot"
model_rfdot <- ksvm(Salary~.,data = salary_train,kernel = "rbfdot")
pred_rfdot <- predict(model_rfdot,newdata = salary_test)                    
mean(pred_rfdot==salary_test$Salary) #82.7% 
#we can construct other svm models using the following methods. the best mdel will be the model with high accuracy.
#kernel = polydot
model_polydot <- ksvm(Salary~.,data = salary_train,kernel = "polydot")
pred_polydot <- predict(model_polydot,newdata = salary_test)                    
mean(pred_ploydot==salary_test$Salary)

#kernel = tanhdot
model_tan<-ksvm(Salary ~.,data = salary_train,kernel = "tanhdot")
pred_tan <- predict(model_tan, newdata = salary_test)
mean(pred_tan == salary_test$Salary) #70.5%

#kernel = laplacedot
model_ld<-ksvm(Salary ~.,data = salary_train,kernel = "laplacedot")
 pred_ld <- predict(model_ld, newdata = salary_test)
mean(pred_ld == salary_test$Salary) 


#kernel = anovadot
model_ad<-ksvm(Salary ~.,data = salary_train,kernel = "anovadot")
pred_ad <- predict(model_ad, newdata = salary_test)
mean(pred_ad == salary_test$Salary) 


#kernel = besseldot
model_bd<-ksvm(Salary ~.,data = salary_train,kernel = "besseldot")
pred_bd <- predict(model_bd, newdata = salary_test)
mean(pred_bd == salary_test$Salary) 


#kernel = splinedot
model_sd<-ksvm(Salary ~.,data = salary_train,kernel = "splinedot")
pred_sd <- predict(model_sd, newdata = salary_test)
mean(pred_sd == salary_test$Salary) 
