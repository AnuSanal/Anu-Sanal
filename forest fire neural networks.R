forestfires <- read.csv(file.choose())
View(forestfires)
forestfires <- forestfires[,-1:-2]
summary(forestfires)
dim(forestfires)

forestfiresnorm <- scale(forestfires[,1:7])
View(forestfiresnorm)
forestfires <- cbind(forestfiresnorm,forestfires[,8:29])


View(forestfires)
forest_train <- forestfires[1:400,]
forest_test <- forestfires[401:517,]
library(neuralnet)
library(nnet)
str(forestfires)
forest_model <- neuralnet( size_category~FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep,data = forest_train)
plot(forest_model)
pred_model <- predict(forest_model,forest_test)
plot(pred_model)
model_results <- compute(forest_model,forest_test)
str(model_results)
predicted_strength <- model_results$net.result

plot(predicted_strength)

forest_model2 <- neuralnet( size_category~FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep,data = forest_train,hidden=6)
plot(forest_model2)
pred_model2 <- predict(forest_model,forest_test)
plot(pred_model2)
model_results2 <- compute(forest_model,forest_test)
str(model_results2)
predicted_strength2 <- model_results2$net.result

plot(predicted_strength2)

