library(Metrics)
require(gam)
## load the data
train <- read.csv("../data/train3.csv")
test <- read.csv("../data/test3.csv")
## change the attributes of some features
train$Date <- as.Date(train$Date)
train$Month <- factor(train$Month,
                      labels=c("May", "June", "July", "August", "September", "October"))
train$Weekday <- factor(train$Weekday,
                        labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
test$Date <- as.Date(test$Date)
test$Month <- factor(test$Month,
                      labels=c("June", "July", "August", "September", "October"))
test$Weekday <- factor(test$Weekday,
                        labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

## CV loop
years <- unique(train$Year)
years <- 2011
cv.auc <- vector()
## loop
for (year in years) {
train.cv <- train[train$Year!=year,]
cv <- train[train$Year==year,]
model <- gam(WnvPresent ~ s(Week) + Species + lo(Latitude, Longitude)+s(Block)+s(Week):Species+s(NumMosquitos), data = train.cv, family="binomial")
## Some time Species is not completed in train.cv
model$xlevels[["Species"]] <- union(model$xlevels[["Species"]], levels(cv$Species))
## predict
pred.prob <- predict(model, newdata = cv, type = "response")
## check for a reasonable AUC of the model against unseen data
cv.auc <- c(cv.auc, auc(cv$WnvPresent, pred.prob))
}
## show the results of cv
cbind(years, cv.auc)
mean(cv.auc)
sd(cv.auc)

## results using:
## model <- gam(WnvPresent ~ s(Week) + Species + lo(Latitude, Longitude)+s(Block)+s(Week):Species+s(NumMosquitos), data = train.cv, family="binomial")
## > >      years    cv.auc
## [1,]  2007 0.8945369
## [2,]  2009 0.8402732
## [3,]  2011 0.8626117
## [4,]  2013 0.8945369
## Warning message:
## In cbind(years, cv.auc) :
##   number of rows of result is not a multiple of vector length (arg 2)
## > [1] 0.8658073
## > [1] 0.02727266

## GAM modelling
#+s(NumMosquitos)
## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
fitSubmit <- gam(WnvPresent ~ s(dYear)+s(dWeek) + Species2 + lo(Latitude, Longitude)+s(Block)+s(dWeek):Species2+s(NumMosquitos), data = my.x, family="binomial")
pSubmit<-predict(fitSubmit, newdata = test, type = "response")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/GAMnoAddress_trap.csv",row.names=FALSE,quote=FALSE)


## CV: 0.8978
## fitSubmit <- gam(WnvPresent ~ s(dYear)+s(dWeek) + Species2 + lo(Latitude, Longitude)+s(Block)+s(dWeek):Species2+s(NumMosquitos), data = my.x, family="binomial")
