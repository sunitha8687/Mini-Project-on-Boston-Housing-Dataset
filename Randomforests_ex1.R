library(randomForest)
library(ISLR)
install.packages("ISLR")
#Boston <- read.csv("Boston Housing data.csv")
Boston.df <- read.table("./Boston Housing data.csv", header = FALSE, sep = "")
View(Boston.df)
names1 <- read.fwf("./Boston Housing data names.names", skip = 30, n = 17,
                   widths = c(-7,8,-60))
names2 <- as.character(names1[-c(3,6,15),])
names2 <- gsub(" ", "", names2)
names(Boston.df) <- names2
summary(Boston.df)
class(Boston.df)
str(Boston.df)
names2(Boston.df)
plot(Boston[,c(3,5,6,11,13,14)],pch=3)
#split 15% of data as test set and 85% as training set

