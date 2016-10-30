
library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
#setwd("../")
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#x <- fread("data/train.csv")
x <- fread("data/train_weight.csv")
test <- fread("data/test.csv")

levels(factor(test$Species))

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



### Start modeling
## use 2011 as a cross validation year; x1 will include the other three years;x2 will include 2011
x1<-x[dYear!=2011,]
xcv<-x[dYear==2011,]


## fit a logistic regression model using just three key fields
fitCv1<-glm(WnvProb ~ dMonth + Species2 + Block+Latitude+Longitude+NumMosquitos, data = x1, family = "binomial",weight=x1$NumMosquitos)  
summary(fitCv1)
p1<-predict(fitCv1, newdata = xcv, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(xcv$WnvPresent,p1)

fitCv2<-glm(cbind(x1$WnvPresent,x1$WnvNotPresent) ~ dMonth + Species2 + Block+Latitude+Longitude+NumMosquitos, data = x1, family = "binomial")  
summary(fitCv2)
p2<-predict(fitCv2, newdata = xcv, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(xcv$WnvPresent,p2)