library(Metrics)
library(data.table)   ## load data in quickly with fread
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train4.csv")
#x <- fread("data/train5.csv")
test <- fread("data/test4.csv")

x$Species<-as.factor(x$Species)
test$Species<-as.factor(test$Species)

my.x = data.frame(x[,list(WnvPresent,Year,Week, Species, Latitude, Longitude,Block,NumMosqSum,Tmax, Tmin, Tavg, Depart, DewPoint,WetBulb,Heat,Cool,PrecipTotal)])
x1<-my.x[x$Year!=2011,]
x2<-my.x[x$Year==2011,]

## GAMboost modelling
require(gbm)
set.seed(2)
if(1==0){
fitCv = gbm(WnvPresent ~Year + Week +Species+NumMosqSum+Latitude+Longitude+Block+Tmax+ Tmin+ Tavg+ Depart+DewPoint+WetBulb+PrecipTotal, data = x1, n.trees = 3000, interaction.depth = 2,shrinkage = 0.01,distribution = "bernoulli",cv.folds=5)

#distribution = "adaboost"
best.iter <- gbm.perf(fitCv,method="cv")
best.iter
summary(fitCv,n.trees=best.iter)

p2<-predict(fitCv, newdata = x2,n.trees = best.iter, type = "response")
auc(x2$WnvPresent,p2)
}


fitSubmit <- gbm(WnvPresent ~ Year+Week +Species+NumMosqSum+Latitude+Longitude+Block+Tmax+ Tmin+ Tavg+ Depart+DewPoint+WetBulb+Heat+Cool+PrecipTotal, data = my.x, n.trees = 3000, interaction.depth = 2,shrinkage = 0.01,cv.folds=5,distribution = "bernoulli")
best.iter <- gbm.perf(fitSubmit,method="cv")
best.iter
summary(fitSubmit,n.trees=best.iter)
pSubmit<-predict(fitSubmit, newdata = test, type = "response")

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/gbm_train4_test4_1.csv",row.names=FALSE,quote=FALSE)
