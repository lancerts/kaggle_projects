
library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train_expand.csv")
test <- fread("data/test.csv")

table(test$Species2,test$dYear)
table(x$Species2,x$dYear)

## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species2:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

## also add some fields for components of the date using simple substrings
x[,dMonth:=substr(x$Date,6,7)]
x[,dYear:=substr(x$Date,1,4)]
test[,dMonth:=substr(test$Date,6,7)]


nlevels(factor(x$Date))
levels(factor(x$AddressNumberAndStreet))
nlevels(factor(x$Trap))


## glance at some conditional probabilities
x[,mean(WnvPresent),by="Species"][order(V1)]  ##glance at average by species
x[,mean(WnvPresent),by="Block"][order(V1)]  ##glance at average by block
x[,mean(WnvPresent),by="dMonth"][order(V1)]
x[,mean(WnvPresent),by="Trap"][order(V1)]##glance at average by month of year

table(x$Species,x$WnvPresent)
#table(x$Block,test$Block)
table(x$Block,x$WnvPresent)
table(test$Species)

levels(factor(x$Block))
levels(factor(test$Block))