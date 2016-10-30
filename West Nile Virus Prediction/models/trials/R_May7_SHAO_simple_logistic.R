
#library(h2o)
#h2oServer <- h2o.init()   ##we'd like to try this, but does not initialize
library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train.csv")
test <- fread("data/test.csv")

## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species2:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

## also add some fields for components of the date using simple substrings
x[,dMonth:=substr(x$Date,6,7)]
x[,dYear:=substr(x$Date,1,4)]
test[,dMonth:=substr(test$Date,6,7)]

## glance at some conditional probabilities
x[,mean(WnvPresent),by="Species"][order(V1)]  ##glance at average by species
x[,mean(WnvPresent),by="Block"][order(V1)]  ##glance at average by block
x[,mean(WnvPresent),by="dMonth"][order(V1)]  ##glance at average by month of year

### Start modeling
## use 2011 as a cross validation year; x1 will include the other three years;x2 will include 2011
x1<-x[dYear!=2011,]
x2<-x[dYear==2011,]


## fit a logistic regression model using just three key fields
fitCv<-glm(WnvPresent ~ dMonth + Species2 + Block+Latitude+Longitude, data = x1, family = "binomial")
summary(fitCv)
p2<-predict(fitCv, newdata = x2, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(x2$WnvPresent,p2)

## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
fitSubmit<-glm(WnvPresent ~ dMonth + Species2 + Block+Latitude+Longitude, data = x, family = "binomial")
levels(test$Species2)
pSubmit<-predict(fitSubmit, newdata = test, type = "response")

## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)



submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"logistic_regression_four_factors.csv",row.names=FALSE,quote=FALSE)