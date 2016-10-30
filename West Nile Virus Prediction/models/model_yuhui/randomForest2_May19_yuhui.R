library(Metrics)
require(randomForest)
## load the data
train <- read.csv("../../data/train3.csv")
test <- read.csv("../../data/test3.csv")
## change the attributes of some features
train$Date <- as.Date(train$Date)
train$Month <- factor(train$Month,
                      levels=c("May", "June", "July", "August", "September", "October"))
train$Weekday <- factor(train$Weekday,
                        levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
test$Date <- as.Date(test$Date)
test$Month <- factor(test$Month,
                      levels=c("June", "July", "August", "September", "October"))
test$Weekday <- factor(test$Weekday,
                        levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
train$WnvPresent <- factor(train$WnvPresent)
## change the Species to four classes
## train$Species <- as.character(train$Species)
## test$Species <- as.character(test$Species)
## train[train$Species=="CULEX ERRATICUS", "Species"] <- "CULEX NO VIRUS"
## train[train$Species=="CULEX SALINARIUS", "Species"] <- "CULEX NO VIRUS"
## train[train$Species=="CULEX TARSALIS", "Species"] <- "CULEX NO VIRUS"
## train[train$Species=="CULEX TERRITANS", "Species"] <- "CULEX NO VIRUS"
## test[test$Species=="CULEX ERRATICUS", "Species"] <- "CULEX NO VIRUS"
## test[test$Species=="CULEX SALINARIUS", "Species"] <- "CULEX NO VIRUS"
## test[test$Species=="CULEX TARSALIS", "Species"] <- "CULEX NO VIRUS"
## test[test$Species=="CULEX TERRITANS", "Species"] <- "CULEX NO VIRUS"
## train$Species <- as.factor(train$Species)
## test$Species <- as.factor(test$Species)
## Suntime
train$Suntime <- train$Sunset - train$Sunrise
test$Suntime <- test$Sunset - test$Sunrise
## TDiff
train$TDiff <- train$Tmax - train$Tmin
test$TDiff <- test$Tmax - test$Tmin

## CV loop
## years <- unique(train$Year)
year <- 2011
## loop
train.cv <- train[train$Year!=year,]
cv <- train[train$Year==year,]
set.seed(1)
model <- randomForest(WnvPresent ~ Date + Weekday + Species + Block + TrapNumber + 
                      Latitude + Longitude + NumMosquitos + 
                      TDiff + Tavg + 
                      DewPoint + 
                      Sunrise + Sunset + 
                      ResultSpeed + ResultDir + AvgSpeed,
                      data=train.cv, importance=TRUE, ntree=1000, mtry=3)
## Some time Species is not completed in train.cv
model$xlevels[["Species"]] <- union(model$xlevels[["Species"]], levels(cv$Species))
## predict
pred.prob <- predict(model, newdata = cv, type = "prob")
## check for a reasonable AUC of the model against unseen data
cv.auc <- auc(cv$WnvPresent, pred.prob[,2])
cv.auc
varImpPlot(model)

set.seed(1)
library(randomForest)
fit <- randomForest(WnvPresent ~ Date + Weekday + Species + Block + TrapNumber + 
                      Latitude + Longitude + NumMosquitos +
                      Tmax + Tavg + 
                      DewPoint + 
                      Sunrise + Sunset +
                      ResultSpeed + ResultDir + AvgSpeed,
                      data=train.cv, importance=TRUE, ntree=1000, mtry=3)
fit


## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
pSubmit <- predict(fit, test, type = "prob")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
submissionFile <- cbind(test$Id,pSubmit[,2])
colnames(submissionFile) <- c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"../../submission/randomForest_May19_yuhui.csv",row.names=FALSE,quote=FALSE)



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
