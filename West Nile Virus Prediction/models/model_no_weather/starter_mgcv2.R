library(Metrics)
library(data.table)   ## load data in quickly with fread
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train3.csv")
#test <- fread("data/test5.csv")

x$Species<-factor(x$Species)



#my.x = data.frame(x[,list(WnvPresent, Year,Month,Week, Species, Latitude, Longitude,Block,TrapNumber,AddressAccuracy,NumMosquitos)])
x1<-x[x$Year!=2011,]
xcv<-x[x$Year==2011,]

## GAM modelling

require(mgcv)

fitCv2 = gam(WnvPresent ~  s(Block)+s(NumMosquitos)+s(Latitude, Longitude)+te(Week,Species,bs="fs")+s(Tavg)+s(Cool)+s(WetBulb), data = x1,  family = binomial)
p2<-predict(fitCv2, newdata = xcv, type = "response")
summary(fitCv2)
auc(xcv$WnvPresent,p2)


#submission file
require(mgcv)

fitSubmit2 <- gam(WnvPresent ~  s(Year,k=3)+Species+s(Block)+s(NumMosquitos)+s(Latitude, Longitude)+ti(Week)+Species+ti(Week,Species,bs="fs"), data = x,  family = binomial )
pSubmit2<-predict(fitSubmit2, newdata = test, type = "response")

submissionFile2<-cbind(test$Id,pSubmit2)
colnames(submissionFile2)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile2,"submission/mgcv.csv",row.names=FALSE,quote=FALSE)

