install.packages("randomForest")
library(randomForest)
devtools::install_github("MI2DataLab/randomForestExplainer")
library(randomForestExplainer)
install.packages("randomForestExplainer")


require(MASS)
View(Boston)

set.seed(1)

id<-sample(2,nrow(Boston),prob = c(0.85,0.15),replace = TRUE)
BostonTrain<-Boston[id==1,]
BostonTest<-Boston[id==2,]

#tuning
besttry <- tuneRF(BostonTrain, BostonTrain$medv,stepFactor = 1.2,improve = 0.01,trace=T,plot = T)

#forest
BostonForest<-randomForest(medv~.,data = BostonTrain,proximity=TRUE,localImp = TRUE)
BostonForest

plot(BostonForest)

min_depth_frame <- min_depth_distribution(BostonForest)
save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 13)


plot_min_depth_distribution(BostonForest) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame)


importance_frame <- measure_importance(BostonForest)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame


#gini_index
BostonForest$importance
varImpPlot(BostonForest)


#predictions
pred1_Boston<-predict(BostonForest,newdata=BostonTest)
plot(pred1_Boston)
summary(pred1_Boston)
boxplot(pred1_Boston)
xtab<-table(pred1_Boston,BostonTest$MEDV)

#validating model


library(caret)
confusionMatrix(table(pred1_Boston,BostonTest$MEDV))

