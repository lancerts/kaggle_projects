library(Metrics)
library(data.table)   ## load data in quickly with fread
x <- fread("../data/train.csv")
test <- fread("../data/test_noAddress_trap.csv")

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
my.x = data.frame(x[,list(WnvPresent, dYear,dWeek,dMonth, Species2, Latitude, Longitude,Block,NumMosquitos,AddressAccuracy,TrapNumber)])
x1<-my.x[x$dYear!=2011,]
x2<-my.x[x$dYear==2011,]

## GAM modelling
require(gam)
fitCv = gam(WnvPresent ~ s(dWeek) + Species2 + lo(Latitude, Longitude)+s(Block)+s(dWeek):Species2+s(NumMosquitos), data = x1, family="binomial")
p2<-predict(fitCv, newdata = x2, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(x2$WnvPresent,p2)
#+s(NumMosquitos)
## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
fitSubmit <- gam(WnvPresent ~ s(dYear)+s(dWeek) + Species2 + lo(Latitude, Longitude)+s(Block)+s(dWeek):Species2+s(NumMosquitos), data = my.x, family="binomial")
pSubmit<-predict(fitSubmit, newdata = test, type = "response")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/GAMnoAddress_trap.csv",row.names=FALSE,quote=FALSE)


## CV: 0.8978
## fitSubmit <- gam(WnvPresent ~ s(dYear)+s(dWeek) + Species2 + lo(Latitude, Longitude)+s(Block)+s(dWeek):Species2+s(NumMosquitos), data = my.x, family="binomial")
