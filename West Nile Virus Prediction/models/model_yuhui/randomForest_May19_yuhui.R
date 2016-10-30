library(Metrics)
require(randomForest)
require(caret)
## load the data
train <- read.csv("../../data/train4.csv")
test <- read.csv("../../data/test4.csv")
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
## Suntime
train$Suntime <- train$Sunset - train$Sunrise
test$Suntime <- test$Sunset - test$Sunrise

## Cross validation and model training using caret
fitControl <- trainControl(method = "repeatedcv",
                           number=4,
                           repeats=1,
                           ## classProbs=F
                           ## preProcOptions = list(thresh = 0.999, 
                           ##     ),
                           verbose=TRUE
                           ## summaryFunction = F1Summary,
                           ## returnResamp="all"
                           )

set.seed(1)
library(randomForest)
fit <- train(WnvPresent ~ Week + Species + Block + Trap +
             Latitude + Longitude + AddressAccuracy + NumMosquitos + NumMosqSum +
             TrapNumber + Tmax + Tmin + Tavg + Depart + DewPoint + WetBulb +
             Heat + Cool + Sunrise + Sunset + PrecipTotal + StnPressure +
             SeaLevel + ResultSpeed + ResultDir + AvgSpeed +
             Code.BR + Code.RA + Code.HZ + Code.VCTS + Code.TSRA +
             Code.FU + Code.DZ + Code.TS + Code.FG. + Code.BCFG +
             Code.MIFG + Code.FG + Code.SQ + Code.SN + Code.VCFG + Code.GR,
             data = train,
             method = "rf",
             ## preProcess = c("YeoJohnson"),
             trControl = fitControl
             )
fit
VarImp(fit, scale=FALSE).

## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
pSubmit <- predict(fit, newdata = test, type = "response")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)
submissionFile <- cbind(test$Id,pSubmit)
colnames(submissionFile) <- c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"../../submission/randomForest_May19_yuhui.csv",row.names=FALSE,quote=FALSE)
