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

library(caret)
cv.ctrl <- trainControl(method = "repeatedcv",number=5, repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)


set.seed(30)
gam.grid <- data.frame(.df = c(8: 14))
gam21<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos +Block+Sunrise+WetBulb,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam21)
gam21$results
gam21$bestTune

set.seed(30)
gam.grid <- data.frame(.df = c(8: 14))
gam22<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam22)
gam22$results
gam22$bestTune
#df 13 Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2260.9299 on 8394.9996 degrees of freedom
#AIC: 2420.9306 



set.seed(30)
gam.grid <- data.frame(.df = c(14: 18))
gam23<- train(WnvPresent ~ Year+ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam23)
gam23$results
gam23$bestTune
# Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2239.9893 on 8392.9996 degrees of freedom
#AIC: 2403.9901 

set.seed(30)
gam.grid <- data.frame(.df = c(16: 20))
gam24<- train(WnvPresent ~ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam24)
gam24$results
gam24$bestTune
# Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2231.5111 on 8383.9993 degrees of freedom
#AIC: 2413.5125 

set.seed(30)
gam.grid <- data.frame(.df = c(12: 16))
gam25<- train(WnvPresent ~Year+ Week+Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb+TrapNumber,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam25)
gam25$results
gam25$bestTune
#Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2236.0065 on 8382.0007 degrees of freedom
#AIC: 2422.005 

set.seed(30)
gam.grid <- data.frame(.df = c(10: 14))
gam26<- train(WnvPresent ~Year+ Week+Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb+TrapNumber+Tmin,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam26)
gam26$results
gam26$bestTune
#Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2210.8449 on 8368.9987 degrees of freedom
#AIC: 2422.8474 

set.seed(30)
gam.grid <- data.frame(.df = c(10: 15))
gam27<- train(WnvPresent ~Year+ Week+Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb+TrapNumber+Tavg,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam27)
gam27$results
gam27$bestTune
#Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2212.279 on 8368.9994 degrees of freedom
#AIC: 2424.2801 

set.seed(30)
gam.grid <- data.frame(.df = c(10: 15))
gam28<- train(WnvPresent ~Year+ Week+Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb+TrapNumber+Tmin+Depart,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam28)
gam28$results
gam28$bestTune
#Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2192.9969 on 8355.9987 degrees of freedom
#AIC: 2430.9995 

set.seed(30)
gam.grid <- data.frame(.df = c(10: 15))
gam29<- train(WnvPresent ~Year+ Week+Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb+TrapNumber+Tmin+SeaLevel,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam29)
gam29$results
gam29$bestTune
# Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2188.4154 on 8355.9993 degrees of freedom
#AIC: 2426.4167 

set.seed(30)
gam.grid <- data.frame(.df = c(10: 15))
gam210<- train(WnvPresent ~Year+ Week+Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb+TrapNumber+Cool,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam210)
gam210$results
gam210$bestTune
# Null Deviance: 3557.9578 on 8474 degrees of freedom
#Residual Deviance: 2188.4154 on 8355.9993 degrees of freedom
#AIC: 2426.4167 

##submission

gam29.probs <- predict(gam29, test, type = "prob")$Yes

summary(gam29.probs)

submissionFile<-cbind(test$Id,gam29.probs)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B_gam28.csv",row.names=FALSE,quote=FALSE)

