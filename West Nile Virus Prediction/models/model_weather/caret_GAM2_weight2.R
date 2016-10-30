library(Metrics)
library(data.table)
library(plyr)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train4B.csv")
test <- fread("data/test4B.csv")


x$WnvPresent <- revalue(as.factor(x$WnvPresent), c("1" = "Yes", "0" = "No"))
x<-data.frame(x)
test<-data.frame(test)
weights=c(rep(1,457),rep(2,nrow(x)-457))
x=cbind(x,weights)


library(caret)
cv.ctrl <- trainControl(method = "repeatedcv",number=5, repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)



set.seed(30)
gam.grid <- data.frame(.df = c(12: 14))
gam22<- train(WnvPresent ~ Year+ Week+NumMosquitos+Sunrise+WetBulb+Block ,data = x, method = "gamSpline", metric = "ROC", trControl = cv.ctrl, tuneGrid = gam.grid  )
summary(gam22)
gam22$results
gam22$bestTune
#df 13 Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2260.9299 on 8394.9996 degrees of freedom
#AIC: 2420.9306 

set.seed(30)
gam.grid <- data.frame(.df = c(12: 14))
gam23<- train(WnvPresent ~ Year+ Week+NumMosquitos+Sunrise+WetBulb+TrapNumber ,data = x, method = "gamSpline", metric = "ROC", trControl = cv.ctrl, tuneGrid = gam.grid  )
summary(gam23)
gam23$results
gam23$bestTune

set.seed(30)
#svm.grid <- data.frame(.df = c(12: 14))
svm1<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb ,data = x, method = "svmRadialWeights", metric = "ROC", trControl = cv.ctrl)
summary(svm1)
svm1$results
svm1$bestTune


upSampledTrain <- upSample(x = x, y = x$WnvPresent,yname = "WnvPresent")
nrow(upSampledTrain)

set.seed(30)
gam.grid <- data.frame(.df = c(22: 26))
gam22<- train(WnvPresent ~ Year+ Week+NumMosquitos+Sunrise+WetBulb+ Latitude+ Longitude+TrapNumber,data = upSampledTrain, method = "gamSpline", metric = "ROC", trControl = cv.ctrl, tuneGrid = gam.grid)
summary(gam22)
gam22$results
gam22$bestTune

##submission

gam22.probs <- predict(gam22, test, type = "prob")$Yes

summary(gam22.probs)

submissionFile<-cbind(test$Id,gam22.probs)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B_gam22.csv",row.names=FALSE,quote=FALSE)

