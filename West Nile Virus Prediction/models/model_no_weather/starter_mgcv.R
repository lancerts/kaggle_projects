library(Metrics)
library(data.table)   ## load data in quickly with fread
#setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train_weight.csv")
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
x[,dMonth:=as.factor(paste(substr(x$Date,6,7)))]
x[,dYear:=as.numeric(paste(substr(x$Date,1,4)))]
x$Date = as.Date(x$Date, format="%Y-%m-%d")
xsDate = as.Date(paste0(x$dYear, "0101"), format="%Y%m%d")
x$dWeek = as.numeric(paste(floor((x$Date - xsDate + 1)/7)))

test[,dMonth:=as.factor(paste(substr(test$Date,6,7)))]
test[,dYear:=as.numeric(paste(substr(test$Date,1,4)))]
test$Date = as.Date(test$Date, format="%Y-%m-%d")
tsDate = as.Date(paste0(test$dYear, "0101"), format="%Y%m%d")
test$dWeek = as.numeric(paste(floor((test$Date - tsDate + 1)/7)))
## train set
x$TrapNumber <- as.integer(substr(as.character(x$Trap), 2, 4))

## test set
test$TrapNumber <- as.integer(substr(as.character(test$Trap), 2, 4))



# we'll set aside 2011 data as test, and train on the remaining
my.x = data.frame(x[,list(WnvPresent dYear,dMonth,dWeek, Species2, Latitude, Longitude,Block,TrapNumber,AddressAccuracy,NumMosquitos)])
x1<-my.x[x$dYear!=2011,]
xcv<-my.x[x$dYear==2011,]

## GAM modelling

require(mgcv)
fitCv1 = gam(WnvPresent ~  s(dWeek) +Species2+s(Block)+s(NumMosquitos)+s(Latitude, Longitude)+s(dWeek,Species2,bs="fs"), data = x1,  family = binomial,weights=x1$NumMosquitos)
##s(dWeek,Species2,bs="fs")+s(NumMosquitos,Species2,bs="fs")+ s(Latitude, Longitude)+s(TrapNumber) s(dWeek) +Species2
p1<-predict(fitCv1, newdata = xcv, type = "response")
summary(fitCv1)
## check for a reasonable AUC of the model against unseen data (2011)
auc(xcv$WnvPresent,p1)



fitCv2 = gam(WnvPresent ~  s(Block)+s(NumMosquitos)+s(Latitude, Longitude)+te(dWeek,Species2,bs="fs"), data = x1,  family = binomial)
p2<-predict(fitCv2, newdata = xcv, type = "response")
summary(fitCv2)
auc(xcv$WnvPresent,p2)


fitCv3 = gam(WnvPresent ~  s(Block)+s(NumMosquitos)+te(Latitude, Longitude)+te(dWeek,Species2,bs="fs"), data = x1,  family = binomial)
p3<-predict(fitCv3, newdata = xcv, type = "response")
summary(fitCv3)
auc(xcv$WnvPresent,p3)


fitCv4 = gam(WnvPresent ~  s(Block)+s(Latitude, Longitude)+te(dWeek,Species2,bs="fs")+te(NumMosquitos,Species2,bs="fs"), data = x1,  family = binomial)
p4<-predict(fitCv4, newdata = xcv, type = "response")
summary(fitCv4)
auc(xcv$WnvPresent,p4)

fitCv5 = gam(WnvPresent ~  Species2+s(Block,k=3,bs="re")+s(NumMosquitos,k=3)+s(Latitude, Longitude,k=4,bs="sos")+te(dWeek,Species2,bs="fs",k=3), data = x1,  family = binomial,gamma=2)
p5<-predict(fitCv5, newdata = xcv, type = "response")
summary(fitCv5)
auc(xcv$WnvPresent,p5)

anova(fitCv1,fitCv2,fitCv3,fitCv4,fitCv5,test="Chisq")



#submission file
require(mgcv)
fitSubmit1 <- gam(WnvPresent ~s(dYear,k=3)+s(Block)+s(NumMosquitos)+s(Latitude, Longitude)+te(dWeek,Species2,bs="fs"), data = my.x,  family = binomial)
pSubmit1<-predict(fitSubmit1, newdata = test, type = "response")

submissionFile1<-cbind(test$Id,pSubmit1)
colnames(submissionFile1)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile1,"submission/mgcv1_5_13.csv",row.names=FALSE,quote=FALSE)

fitSubmit2 <- gam(WnvPresent ~ s(dYear,k=3)+Species2+s(Block,k=3,bs="re")+s(NumMosquitos,k=3)+s(Latitude, Longitude,k=4,bs="sos")+te(dWeek,Species2,bs="fs",k=3), data = my.x,  family = binomial ,gamma=2)
pSubmit2<-predict(fitSubmit2, newdata = test, type = "response")

submissionFile2<-cbind(test$Id,pSubmit2)
colnames(submissionFile2)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile2,"submission/mgcv2_5_13.csv",row.names=FALSE,quote=FALSE)

fitSubmit3 <- gam(WnvPresent ~ te(dWeek,dYear,k=3)+s(Block)+s(NumMosquitos)+s(Latitude, Longitude)+te(dWeek,Species2,bs="fs"), data = my.x,  family = binomial)
pSubmit3<-predict(fitSubmit3, newdata = test, type = "response")

submissionFile3<-cbind(test$Id,pSubmit3)
colnames(submissionFile3)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile3,"submission/mgcv3_5_13.csv",row.names=FALSE,quote=FALSE)