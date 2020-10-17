install.packages("ROCR")
library(randomForest)
data("iris")
View(iris)
summary(iris)
pairs(iris[1:5], main = "Edgar Anderson's Iris Data", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
boxplot(iris)
cor(iris[,1:4])
#bootstrap = random sampling with replacement
set.seed(1)
ind <- sample(2,nrow(iris),replace = TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
#to build rf with training data
iris_rf <- randomForest(Species~.,data=trainData,ntree=100,proximity=FALSE)
iris_rf
iris_rf$err.rate[100,1]
table(predict(iris_rf),trainData$Species) 
plot(iris_rf) #black curve = OOB error rate,red=error rate of setosa,green and blue for versicolor and verginica 
importance(iris_rf)
varImpPlot(iris_rf) #variable importance based on GINI
#to build random forest for test data
irisPred<-predict(iris_rf,newdata=testData)
table(irisPred, testData$Species)
#Try to see the margin, positive or negative, if positive it means correct classification
plot(margin(iris_rf,testData$Species)) 
#tuning the randomforest to see OOB errors and then pick the min OOB error and best mtry value
mtry <- tuneRF(iris[,-5],iris[,5], stepFactor=0.5)
mtry #from this find the best mtry value which has least OOB error
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
best.m
# in our case mtry = 2 is best since it has least OOB error
#Build model again using best try value
set.seed(1)
rf <- randomForest(Species~.,data = iris,mtry=2,importance=FALSE,ntree=100)
rf #rf model with min OOB error
importance(rf)
varImpPlot(rf)
#OOB predictions with min OOB error model "rf" in our case
pred1=predict(rf,type = "prob")
pred1
#--------------#

