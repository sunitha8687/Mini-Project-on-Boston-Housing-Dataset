load('RFimportance2.Rdata')
summary(df2)
View(df2)
boxplot(df2)
plot(df2)
testForest<-randomForest(out~.,data = df2)
plot(testForest$importance)
df2


reg1<-lm(out~.,data = df2)
confint(reg1)
attach(df2)
plot(xin2,out)
plot(xin3,out)
plot(xin4,out)
plot(xin5,out)

par(mfrow=c(1,1))
pairs(df2)

summary(reg1)
summary(lm(out~xin2*xin3,data=df2))
summary(lm(out~xin2*xin4,data=df2))
summary(lm(out~xin2*xin5,data=df2))
