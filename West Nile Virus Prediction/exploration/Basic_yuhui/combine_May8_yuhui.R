###################################################################################
## This code is part of the kaggle project: West Nile Virus Prediction
## By Yuhui Zhang, and Shao Tang
## May. 8, 2015

###################################################################################

## load library
library(data.table)   ## load data in quickly with fread
library(plyr)
## load the data
train <- fread("../data/train.csv")
test <- fread("../data/test.csv")

## subset the data sets
## train <- train[, -c(2, 5, 6, 7, 10)]
## test <- test[, -(c(2, 5, 6, 7, 10)+1)]

## also add some fields for components of the date using simple substrings
train[,Month:=substr(train$Date,6,7)]
train[,Year:=substr(train$Date,1,4)]
test[,Month:=substr(test$Date,6,7)]
test[,Year:=substr(test$Date,1,4)]
## change them to factors
train[,Month:=factor(train$Month, levels=unique(Month))]
test[,Month:=factor(test$Month, levels=unique(Month))]
train[,Year:=factor(train$Year, levels=unique(Year))]
test[,Year:=factor(test$Year, levels=unique(Year))]

## change Date to date type
train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)

## combine the same data sets (sum the NumMosquitos)
train <- ddply(train,
               .(Date, Address, Species, Block, Street, Trap, AddressNumberAndStreet, Latitude, Longitude, AddressAccuracy,  Month, Year),
               summarize,
               NumMosquitos = sum(NumMosquitos),
               WnvPresent = as.logical(sum(WnvPresent)))

## Visualization
par(mfrow=c(1,3))
## histogram of NumMosquitos
hist(train$NumMosquitos, breaks = 1000, xlim = c(0,100))
## histogram of NumMosquitos by WnvPresent
hist(train[train$WnvPresent==T,]$NumMosquitos, breaks = 1000, xlim = c(0,100))
hist(train[train$WnvPresent==F,]$NumMosquitos, breaks = 1000, xlim = c(0,100))

## change WnvPresent back to integer
train$WnvPresent <- as.integer(train$WnvPresent)

## write it back
write.csv(train, "../data/train2.csv", row.names=F)
write.csv(test, "../data/test2.csv", row.names=F)
