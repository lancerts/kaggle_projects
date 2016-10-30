library(Metrics)
library(data.table)   ## load data in quickly with fread
#setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train5.csv")
test <- fread("data/test5.csv")

## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies[-which(vSpecies == "CULEX PIPIENS" |
                  vSpecies == "CULEX PIPIENS/RESTUANS" |
                  vSpecies == "CULEX RESTUANS")] = "CULEX OTHER"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

my.x = data.frame(x[,list(WnvPresent,Year,Week, Species, Latitude, Longitude,Block,NumMosquitos,Tmax, Tavg,  DewPoint,WetBulb,Heat,Cool,PrecipTotal)])
x1<-my.x[x$Year!=2011,]
x2<-my.x[x$Year==2011,]

## GAMboost modelling
require(gbm)
set.seed(2)

if(1==0){

fitCv = gbm(WnvPresent ~ Year+Week +Species+NumMosquitos+Latitude+Longitude+Block+Tmax +Tavg+ DewPoint, data = x1, n.trees = 50000, interaction.depth = 2,cv.folds=5,distribution = "bernoulli")

#distribution = "adaboost"
best.iter <- gbm.perf(fitCv,method="cv")
best.iter
summary(fitCv,n.trees=best.iter)

p2<-predict(fitCv, newdata = x2,n.trees = best.iter, type = "response")
auc(x2$WnvPresent,p2)



fitCv = gbm(WnvPresent ~ Year+Week +Species+Latitude+Longitude+Block+Tmax+  Tavg+ DewPoint, data = x1, n.trees = 50000, interaction.depth = 2,cv.folds=5,distribution = "bernoulli") #distribution = "adaboost"
best.iter <- gbm.perf(fitCv,method="cv")
best.iter
summary(fitCv,n.trees=best.iter)

p2<-predict(fitCv, newdata = x2,n.trees = best.iter, type = "response")
auc(x2$WnvPresent,p2)
}



fitSubmit <- gbm(WnvPresent ~ Year+Week +Species+NumMosquitos+Latitude+Longitude+Block+Tmax+  Tavg+ DewPoint, data = my.x, n.trees = 100000, interaction.depth = 2,cv.folds=5,distribution = "bernoulli")
best.iter <- gbm.perf(fitSubmit,method="cv")
best.iter
summary(fitSubmit,n.trees=best.iter)
pSubmit<-predict(fitSubmit, newdata = test, type = "response")

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/gbm_train5_test5_speciescombined.csv",row.names=FALSE,quote=FALSE)
