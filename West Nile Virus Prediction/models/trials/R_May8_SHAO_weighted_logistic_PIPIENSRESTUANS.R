
library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train.csv")
test <- fread("data/test.csv")

levels(factor(test$Species))

## prep the species column by moving the test-only UNSPECIFIED CULEX to  CULEX PIPIENS/RESTUANS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX PIPIENS/RESTUANS"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species2:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

## also add some fields for components of the date using simple substrings
x[,dMonth:=substr(x$Date,6,7)]
x[,dYear:=substr(x$Date,1,4)]
test[,dMonth:=substr(test$Date,6,7)]


fitSubmit<-glm(WnvPresent ~ dMonth + Species2 + Block+Latitude+Longitude, data = x, family = "binomial",weights=x$NumMosquitos)

pSubmit<-predict(fitSubmit, newdata = test, type = "response")





submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"weighted_four_factors_CULEX PIPIENSRESTUANS.csv",row.names=FALSE,quote=FALSE)