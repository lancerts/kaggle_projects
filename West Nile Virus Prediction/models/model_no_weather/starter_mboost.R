library(Metrics)
library(data.table)   ## load data in quickly with fread
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train3.csv")
test <- fread("data/test5.csv")
x$Species<-as.factor(x$Species)
test$Species<-as.factor(test$Species)
x$WnvPresent<-as.factor(x$WnvPresent)


# we'll set aside 2011 data as test, and train on the remaining
my.x = data.frame(x[,list(WnvPresent,Year,Week, Species, Latitude, Longitude,Block,NumMosquitos,Tavg,Tmax,DewPoint)])
x1<-my.x[my.x$Year!=2011,]
x2<-my.x[my.x$Year==2011,]
test1=data.frame(test[,list(WnvPresent,Year,Week, Species, Latitude, Longitude,Block,NumMosquitos,Tavg,Tmax,DewPoint)])

## GAMboost modelling
require(mboost)
fitCv = gamboost(WnvPresent ~ Week+Tmax+bols(Species)+bbs(Week,lambda=10^-4)%O%bols(Species)+Latitude+Longitude+Block+NumMosquitos, data = x1,  control = boost_control(mstop = 200),family = Binomial())
p2<-predict(fitCv, newdata = x2, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(x2$WnvPresent,p2)





#+bols(Species)+bbs(Week)%X%bols(Species)+Latitude+Longitude+Block+NumMosquitos
## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
fitSubmit <- gamboost(WnvPresent ~ Year+ Week +bols(Species)+bbs(Week)%X%bols(Species)+Latitude+Longitude+Block+NumMosquitos, data = my.x,  control = boost_control(mstop = 200),family = Binomial())
pSubmit<-predict(fitSubmit, newdata = test1, type = "response")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/starter_mboost_on_expand.csv",row.names=FALSE,quote=FALSE)
