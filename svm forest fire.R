forest <- read.csv(file.choose())
View(forest)
forest <- forest[,-1:-2]
View(forest)

forest_train <- forest[1:414,]
forest_test <- forest[415:517,]
library(kernlab)
library(caret)

#kernel = rbfdot
model_rfdot <- ksvm(size_category~.,data = forest_train,kernel = "rbfdot")
pred_rfdot <- predict(model_rfdot,newdata = forest_test)                    
mean(pred_rfdot==forest_test$size_category) #73.78 % accuracy

# kernel = vanilladot
model_vanilla<-ksvm(size_category ~.,data = forest_train,kernel = "vanilladot")
pred_vanilla <- predict(model_vanilla, newdata = forest_test)
mean(pred_vanilla == forest_test$size_category) # 99.02 % accuracy

#kernel = polydot
model_polydot <- ksvm(size_category~.,data = forest_train,kernel ="polydot" )
pred_polydot <- predict(model_polydot,newdata = forest_test)                    
mean(pred_polydot==forest_test$size_category) #99.02%

#kernel = tanhdot
model_tan<-ksvm(size_category ~.,data = forest_train,kernel = "tanhdot")
pred_tan <- predict(model_tan, newdata = forest_test)
mean(pred_tan == forest_test$size_category)  #69.9%


#kernel = laplacedot
model_ld<-ksvm(size_category ~.,data = forest_train,kernel = "laplacedot")
pred_ld <- predict(model_ld, newdata = forest_test)
mean(pred_ld == forest_test$size_category) #69.9%


#kernel = anovadot
model_ad<-ksvm(size_category ~.,data = forest_train,kernel = "anovadot")
pred_ad <- predict(model_ad, newdata = forest_test)
mean(pred_ad == forest_test$size_category) #88.3%


#kernel = besseldot
model_bd<-ksvm(size_category ~.,data = forest_train,kernel = "besseldot")
pred_bd <- predict(model_bd, newdata = forest_test)
mean(pred_bd == forest_test$size_category) #69.90%


#kernel = splinedot
model_sd<-ksvm(size_category ~.,data = forest_train,kernel = "splinedot")
pred_sd <- predict(model_sd, newdata = forest_test)
mean(pred_sd == forest_test$size_category) #94.17%

# so the best model is vanilladot and polydot.


