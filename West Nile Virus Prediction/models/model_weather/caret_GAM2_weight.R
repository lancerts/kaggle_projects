library(Metrics)
library(data.table)   ## load data in quickly with fread
library(plyr) 
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train4B.csv")
test <- fread("data/test4B.csv")


x$WnvPresent <- revalue(as.factor(x$WnvPresent), c("1" = "Yes", "0" = "No"))
x<-data.frame(x)
test<-data.frame(test)
a=x$Sunset-x$Sunrise
b=x$Tmax-x$Tmin
x=cbind(x,a,b)
x=rename(x, c("a" = "DayTime","b"="Tdifference"))

a=test$Sunset-test$Sunrise
b=test$Tmax-test$Tmin
test=cbind(test,a,b)
test=rename(test, c("a" = "DayTime","b"="Tdifference"))

sort(x$WnvPresent,decreasing = T)[1:457]
weight=c(rep(20,457),rep(1,nrow(x)-457))

library(caret)
cv.ctrl <- trainControl(method = "repeatedcv",number=5, repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)
variables<-c("Year","Week","Latitude","Longitude","NumMosquitos","Sunrise","WetBulb")
preProcValues <- preProcess(x[,variables], method = c("pca"))

trainTransformed <- predict(preProcValues, x[,variables])
a=x$WnvPresent
trainTransformed<-rename(cbind(a,trainTransformed),c("a"="WnvPresent"))
testTransformed <- predict(preProcValues, test[,variables])

a=(x$NumMosquitos)^2
x=rename(cbind(x,a), c("a" = "NumSquare"))


set.seed(30)
gam.grid <- data.frame(.df = c(8: 12))
gam22<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb+NumSquare,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl, distribution="poisson")
summary(gam22)
gam22$results
gam22$bestTune
#df 13 Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2260.9299 on 8394.9996 degrees of freedom
#AIC: 2420.9306 

set.seed(30)
library(gam)
gam22w<- gam(WnvPresent ~ s(Year,df=13)+ s(Week,df=13) + lo(Latitude, Longitude)+s(Block,df=13)+s(NumMosquitos,df=13)+s(Sunrise,df=13)+s(WetBulb,df=13), data = x, family="binomial",weights=weight,control=gam.control(epsilon=1e-05, bf.epsilon = 1e-05, maxit=1000, bf.maxit = 1000, trace=TRUE))





##submission

gam22w.probs <- predict(gam22w, test, type = "response")

summary(gam22w.probs)

submissionFile<-cbind(test$Id,gam22w.probs)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B_gam22w.csv",row.names=FALSE,quote=FALSE)

