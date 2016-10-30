library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train_expand.csv")
test <- fread("data/test.csv")
#sort(x$WnvPresent)

vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies[vSpecies=="CULEX SALINARIUS"]<-"CULEX NONE"
vSpecies[vSpecies=="CULEX TERRITANS"]<-"CULEX NONE"
vSpecies[vSpecies=="CULEX TARSALIS"]<-"CULEX NONE"
vSpecies[vSpecies=="CULEX ERRATICUS"]<-"CULEX NONE"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))


## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species2:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

## also add some fields for components of the date using simple substrings
x[,dMonth:=substr(x$Date,6,7)]
x[,dYear:=substr(x$Date,1,4)]
test[,dMonth:=substr(test$Date,6,7)]
## also add some fields for components of the date using simple substrings
x[,dMonth:=as.factor(paste(substr(x$Date,6,7)))]
x[,dYear:=as.numeric(paste(substr(x$Date,1,4)))]
x$Date = as.Date(x$Date, format="%Y-%m-%d")
xsDate = as.Date(paste0(x$dYear, "0101"), format="%Y%m%d")
x$dWeek = as.numeric(paste(floor((x$Date - xsDate + 1)/7)))

test[,dMonth:=as.factor(paste(substr(test$Date,6,7)))]
test[,dYear:=as.factor(paste(substr(test$Date,1,4)))]
test$Date = as.Date(test$Date, format="%Y-%m-%d")
tsDate = as.Date(paste0(test$dYear, "0101"), format="%Y%m%d")
test$dWeek = as.numeric(paste(floor((test$Date - tsDate + 1)/7)))
## train set
train$TrapNumber <- as.integer(substr(as.character(train$Trap), 2, 4))

## test set
test$TrapNumber <- as.integer(substr(as.character(test$Trap), 2, 4))

### Start modeling
## use 2011 as a cross validation year; x1 will include the other three years;x2 will include 2011
x1<-x[dYear!=2011,]
x2<-x[dYear==2011,]
#levels(factor(x1$Trap))
#levels(factor(x2$Trap))
## fit a logistic regression model using just three key fields

fitCv<-glm(WnvPresent ~  dMonth+Species2 + Block+Latitude+Longitude+dMonth:Species2+NumMosquitos, data = x1, family = "binomial",weight=x1$AddressAccuracy)
summary(fitCv)


p2<-predict(fitCv, newdata = x2, type = "response")
auc(x2$WnvPresent,p2)




fitSubmit<-glm(WnvPresent ~ dMonth + Species2 + Block+Latitude+Longitude+dMonth:Species2, data = x, family = "binomial",weight=x$AddressAccuracy)


pSubmit<-predict(fitSubmit, newdata = test, type = "response")

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"exapnd_logistic_new_feature.csv",row.names=FALSE,quote=FALSE)
