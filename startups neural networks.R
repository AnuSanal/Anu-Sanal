Startups <- read.csv(file.choose())
View(Startups)
library(fastDummies)
library(knitr)
Startups <- fastDummies::dummy_cols(Startups,select_columns = "State")
knitr::kable(Startups)
Startups <- cbind(Startups)
View(Startups)
Startups$State <- NULL
str(Startups)
profit <- Startups$Profit
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
norm_startup<-as.data.frame(lapply(Startups[,-4],FUN=normalize))
norm_startup <- scale(Startups[,-4])
Startups <- cbind(norm_startup,profit)
dim(Startups)
colnames(Startups)[1] <- "rd"
colnames(Startups)[2] <- "ad"
colnames(Startups)[3] <- "ms"
colnames(Startups)[4] <- "sc"
colnames(Startups)[5] <- "sf"
colnames(Startups)[6] <- "sn"

Startups_train <- Startups[1:25,]
Startups_test <- Statups[25:50,]
library(neuralnet)
library(nnet)
Startups_model <- neuralnet(profit~rd+ad+ms, data=Startups_train)
str(Startups_model)
plot(Startups_model)
set.seed(12323)
model_results <- compute(Startups_model,Startups_test[1:6])
str(model_results)
predicted_profit <- model_results$net.result
cor(predicted_profit,Startups_test$Profit)
plot(predicted_profit,Startups_test$Profit)
