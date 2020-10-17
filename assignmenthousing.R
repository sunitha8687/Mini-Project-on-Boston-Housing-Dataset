install.packages("randomForest")
library(randomForest)
housing=read.csv('Boston_Housing.csv',header = T,na.strings="?")
View(housing)

set.seed(1)
id<-sample(2,nrow(housing),prob = c(0.85,0.15),replace = TRUE)
housingTrain<-housing[id==1,]
housingTest<-housing[id==2,]

#tuning
besttry <-tuneRF(housingTrain,housingTrain$MEDV,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)

#forest
housingForest<-randomForest(MEDV~.,data = housingTrain)
housingForest

#gini_index
housingForest$importance
varImpPlot(housingForest)

#linear regression
#mod <- lm(MEDV~ ., housingForest)

#predictions
pred1_housing<-predict(housingForest,newdata=housingTest)
pred1_housing
plot(pred1_housing)
summary(pred1_housing)
boxplot(pred1_housing)

plot(predict(mod), residuals(mod))

#validating model
library(caret)
confusionMatrix(pred1_housing,housingTest$MEDV)
confusionMatrix(pred1_housing,housingTest$MEDV)
