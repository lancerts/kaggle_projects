library(Metrics)
library(data.table)   ## load data in quickly with fread
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train4B.csv")
test <- fread("data/test4B.csv")
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



# we'll set aside 2011 data as test, and train on the remaining
my.x = data.frame(x[,list(WnvPresent,Year,Week, Species, Latitude, Longitude,Block,NumMosquitos,Tmax, Tavg,  DewPoint,WetBulb,Heat,Cool,PrecipTotal,Sunrise,Sunset)])
x1<-my.x[x$Year!=2011,]
x2<-my.x[x$Year==2011,]

## GAM modelling
require(gam)
fitCv = gam(WnvPresent ~ s(Week) + Species + lo(Latitude, Longitude)+s(Block)+s(Week):Species+s(NumMosquitos)+s(Sunrise)+s(DewPoint), data = x1, family="binomial",control=gam.control(epsilon=1e-07, bf.epsilon = 1e-05, maxit=1000, bf.maxit = 1000, trace=TRUE))
p2<-predict(fitCv, newdata = x2, type = "response")
## GAM general.wam loop 15: deviance = 2169.517 
auc(x2$WnvPresent,p2)
#0.8455485

fitCv = gam(WnvPresent ~ s(Week) + Species + lo(Latitude, Longitude)+s(Block)+s(Week):Species+s(NumMosquitos)+s(DewPoint), data = x1, family="binomial",control=gam.control(epsilon=1e-07, bf.epsilon = 1e-05, maxit=1000, bf.maxit = 1000, trace=TRUE))
p2<-predict(fitCv, newdata = x2, type = "response")
## GAM general.wam loop 15: deviance = 2175.87 
auc(x2$WnvPresent,p2)
#0.8440279





fitCv = gam(WnvPresent ~ s(Week) + Species + lo(Latitude, Longitude)+s(Block)+s(Week):Species+s(NumMosquitos), data = x1, family="binomial",control=gam.control(epsilon=1e-07, bf.epsilon = 1e-06, maxit=1000, bf.maxit = 1000, trace=TRUE))
p2<-predict(fitCv, newdata = x2, type = "response")
## GAM general.wam loop 16: deviance = 2186.748
auc(x2$WnvPresent,p2)
#0.8626117

fitCv = gam(WnvPresent ~ s(Week) + Species + lo(Latitude, Longitude)+s(Week):Species+s(NumMosquitos), data = x1, family="binomial",control=gam.control(epsilon=1e-07, bf.epsilon = 1e-06, maxit=1000, bf.maxit = 1000, trace=TRUE))
p2<-predict(fitCv, newdata = x2, type = "response")
## GAM general.wam loop 17: deviance = 2197.654 
auc(x2$WnvPresent,p2)
#0.864527



fitCv = gam(WnvPresent ~ s(Sunrise) + Species + lo(Latitude, Longitude)+s(Sunrise):Species+s(NumMosquitos), data = x1, family="binomial",control=gam.control(epsilon=1e-07, bf.epsilon = 1e-06, maxit=1000, bf.maxit = 1000, trace=TRUE))
p2<-predict(fitCv, newdata = x2, type = "response")
## GAM general.wam loop 18: deviance = 2198.129
auc(x2$WnvPresent,p2)
#0.8737435

fitCv = gam(WnvPresent ~ s(Sunrise) + Species + lo(Latitude, Longitude)+s(Sunrise):Species+s(NumMosquitos)+s(Block)+I(Heat==0), data = x1, family="binomial",control=gam.control(epsilon=1e-07, bf.epsilon = 1e-06, maxit=1000, bf.maxit = 1000, trace=TRUE))
p2<-predict(fitCv, newdata = x2, type = "response")
## GAM general.wam loop 13: deviance = 2173.6056 
auc(x2$WnvPresent,p2)
#0.8737435


fitSubmit1 <- gam(WnvPresent ~ s(Sunrise) + Species + lo(Latitude, Longitude)+s(Sunrise):Species+s(NumMosquitos)+s(Block)+I(Heat==0), data = my.x, family="binomial",control=gam.control(epsilon=1e-07, bf.epsilon = 1e-06, maxit=1000, bf.maxit = 1000, trace=TRUE))
pSubmit<-predict(fitSubmit1, newdata = test, type = "response")
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B.csv",row.names=FALSE,quote=FALSE)



