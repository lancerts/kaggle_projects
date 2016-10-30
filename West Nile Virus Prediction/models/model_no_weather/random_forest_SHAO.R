###################################################################################
## This code is part of the kaggle project: West Nile Virus Prediction
## By Yuhui Zhang, and Shao Tang
## May. 9, 2015

## MODEL DESCRIPTION
###################################################################################

## LOAD
library(randomForest)
library(data.table)   ## load data in quickly with fread
library(Metrics)
## load the data
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
x<- fread("data/train.csv")
test <- fread("data/test.csv")

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

### Start modeling
## use 2011 as a cross validation year; x1 will include the other three years;x2 will include 2011
x1<-data.frame(x[dYear!=2011,])[,-c(1, 2, 5, 6, 7,10,11,13,15)]
x2<-data.frame(x[dYear==2011,])[,-c(1, 2, 5, 6, 7,10,11,13,15)]
x1=transform(x1, Species = as.factor(Species),WnvPresent = as.factor(WnvPresent),dMonth = as.factor(dMonth))
x2=transform(x2, Species = as.factor(Species),WnvPresent = as.factor(WnvPresent),dMonth = as.factor(dMonth))
levels(x1$Species)
levels(x2$Species)

model <- randomForest(WnvPresent~., 
	data = x1, 
	importance=TRUE,
	keep.forest=TRUE
)
print(model)
#what are the important variables (via permutation)
varImpPlot(model, type=1)

p2<-predict(model, newdata = x2, type = "response")
auc(x2$WnvPresent,p2)