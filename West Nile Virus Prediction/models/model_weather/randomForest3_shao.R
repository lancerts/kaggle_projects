library(Metrics)
require(randomForest)
library(data.table) 
## load the data
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
train <- fread("data/train4.csv")
#x <- fread("data/train5.csv")
test <- fread("data/test4.csv")
train$Species<-as.factor(train$Species)
test$Species<-as.factor(test$Species)
train$WnvPresent<-as.factor(train$WnvPresent)

## CV loop
## years <- unique(train$Year)
year <- 2011
## loop
train.cv <- train[train$Year!=year,]
cv <- train[train$Year==year,]

set.seed(1)
model <- randomForest(WnvPresent ~ Week + Species + Block  + Latitude + Longitude + NumMosqSum +Tmax +Tavg +DewPoint+NumMosquitos,data=train.cv, importance=TRUE, ntree=1000, mtry=5)

#NumMosqSum + TrapNumber+Sunrise + Sunset +Tmax + Tavg +DewPoint + WetBulb + ResultDir + AvgSpeed
## Some time Species is not completed in train.cv
#model$xlevels[["Species"]] <- union(model$xlevels[["Species"]], levels(cv$Species))
## predict
pred.prob <- predict(model, newdata = cv, type = "prob")[,2]
## check for a reasonable AUC of the model against unseen data
cv.auc <- auc(cv$WnvPresent, pred.prob)
cv.auc
varImpPlot(model)


library(randomForest)
fit <-randomForest(WnvPresent ~ Week + Species + Block  + Latitude + Longitude + NumMosqSum +Tmax +Tavg +DewPoint+NumMosquitos,data=train, importance=TRUE, ntree=1000, mtry=5)
fit


## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
pSubmit <- predict(fit, test, type = "prob")[,2]
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
submissionFile <- cbind(test$Id,pSubmit)
colnames(submissionFile) <- c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/randomForest_train4_test4.csv",row.names=FALSE,quote=FALSE)



## model <- randomForest(WnvPresent ~ Week + Species + Block + TrapNumber + 
##                       Latitude + Longitude + AddressAccuracy + NumMosquitos +
##                       NumMosqSum + Tmax + Tmin + Tavg +
##                       Depart + DewPoint + 
##                       Heat + Cool + Sunrise + Sunset +
##                       SeaLevel + ResultSpeed + ResultDir + AvgSpeed +
##                       Code.BR + Code.RA + Code.HZ + Code.VCTS + Code.TSRA +
##                       Code.FU + Code.DZ + Code.TS + Code.FG. + Code.BCFG +
##                       Code.MIFG + Code.FG + Code.SQ + Code.SN + Code.VCFG +
##                       Code.GR,
##                       data=train.cv, importance=TRUE, ntree=500, mtry=2)
