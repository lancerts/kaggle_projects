library(Metrics)
library(data.table)   ## load data in quickly with fread
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train.csv")
test <- fread("data/test4.csv")

## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies[-which(vSpecies == "CULEX PIPIENS" |
                  vSpecies == "CULEX PIPIENS/RESTUANS" |
                  vSpecies == "CULEX RESTUANS")] = "CULEX OTHER"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species2:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

## also add some fields for components of the date using simple substrings
x[,dMonth:=as.numeric(paste(substr(x$Date,6,7)))]
x[,dYear:=as.numeric(paste(substr(x$Date,1,4)))]
x$Date = as.Date(x$Date, format="%Y-%m-%d")
xsDate = as.Date(paste0(x$dYear, "0101"), format="%Y%m%d")
x$dWeek = as.numeric(paste(floor((x$Date - xsDate + 1)/7)))

test[,dMonth:=as.numeric(paste(substr(test$Date,6,7)))]
test[,dYear:=as.numeric(paste(substr(test$Date,1,4)))]
test$Date = as.Date(test$Date, format="%Y-%m-%d")
tsDate = as.Date(paste0(test$dYear, "0101"), format="%Y%m%d")
test$dWeek = as.numeric(paste(floor((test$Date - tsDate + 1)/7)))
## train set
x$TrapNumber <- as.integer(substr(as.character(x$Trap), 2, 4))

## test set
test$TrapNumber <- as.integer(substr(as.character(test$Trap), 2, 4))


# we'll set aside 2011 data as test, and train on the remaining
my.x = data.frame(x[,list(WnvPresent,dYear,dWeek,dMonth, Species2, Latitude, Longitude,Block,NumMosquitos,AddressAccuracy,TrapNumber)])
x1<-my.x[x$dYear!=2011,]
x2<-my.x[x$dYear==2011,]


## GAMboost modelling
require(gbm)
set.seed(2)
fitCv = gbm(WnvPresent ~ dYear+dWeek +Species2+Latitude+Longitude+TrapNumber+NumMosquitos, data = x1, n.trees = 5000, interaction.depth = 2,cv.folds=5,shrinkage = 0.005,distribution = "bernoulli") #distribution = "adaboost"  distribution = "adaboost"
best.iter <- gbm.perf(fitCv,method="cv")
best.iter
summary(fitCv,n.trees=best.iter)

p2<-predict(fitCv, newdata = x2,n.trees = best.iter, type = "response")
auc(x2$WnvPresent,p2)





# dWeek +Species2+Latitude+Longitude+Block+NumMosquitos
## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
fitSubmit <- gbm(WnvPresent ~ dYear+dWeek +Species2+Latitude+Longitude+Block+NumMosquitos, data = my.x, n.trees = 2000, interaction.depth = 2,cv.folds=5,shrinkage = 0.01,distribution = "bernoulli")
best.iter <- gbm.perf(fitSubmit,method="cv")
best.iter
summary(fitSubmit,n.trees=best.iter)
pSubmit<-predict(fitSubmit, newdata = test, type = "response")

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/gbm_mean.csv",row.names=FALSE,quote=FALSE)
