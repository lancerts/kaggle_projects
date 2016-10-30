library(Metrics)
library(data.table)   ## load data in quickly with fread
library(plyr) 
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train4.csv")
test <- fread("data/test4.csv")


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

library(caret)
cv.ctrl <- trainControl(method = "repeatedcv",number=5, repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)


set.seed(30)
gam.grid <- data.frame(.df = c(4: 10))
gam31<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosqSum+Sunrise+WetBulb+NumMosquitos,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam31)
gam31$results
gam31$bestTune
#Null Deviance: 4321.243 on 10505 degrees of freedom
#Residual Deviance: 3149.681 on 10469 degrees of freedom
#AIC: 3223.681 

set.seed(30)
gam.grid <- data.frame(.df = c(2: 5))
gam32<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosqSum+Sunrise+WetBulb+NumMosquitos+Block,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam32)
gam32$results
gam32$bestTune
#Null Deviance: 4321.243 on 10505 degrees of freedom
#Residual Deviance: 3157.049 on 10472 degrees of freedom
#AIC: 3225.049 


set.seed(30)
gam.grid <- data.frame(.df = c(2: 5))
gam33<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosqSum+Sunrise+WetBulb+NumMosquitos+TrapNumber,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam33)
gam33$results
gam33$bestTune
#   Null Deviance: 4321.243 on 10505 degrees of freedom
#Residual Deviance: 3155.898 on 10472 degrees of freedom
#AIC: 3223.899 


set.seed(30)
gam.grid <- data.frame(.df = c(4: 8))
gam34<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosqSum+WetBulb+NumMosquitos+Sunrise+Tdifference,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam34)
gam34$results
gam34$bestTune
#   Null Deviance: 4321.243 on 10505 degrees of freedom
#Residual Deviance: 3140.204 on 10464 degrees of freedom
#AIC: 3224.204 


set.seed(30)
gam.grid <- data.frame(.df = c(14: 20))
gam35<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosqSum+Sunrise+WetBulb,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam35)
gam35$results
gam35$bestTune

set.seed(30)
gam.grid <- data.frame(.df = c(14: 20))
gam36<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosqSum+Sunrise+WetBulb+NumMosquitos,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam36)
gam36$results
gam36$bestTune

##submission

gam35.probs <- predict(gam35, test, type = "prob")$Yes

summary(gam35.probs)

submissionFile<-cbind(test$Id,gam35.probs)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4_test4_gam35.csv",row.names=FALSE,quote=FALSE)

