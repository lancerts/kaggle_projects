library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
train <- fread("data/train.csv")


library(plyr)
train <- ddply(train,
               .(Date, Address, Species, Block, Street, Trap,
                 AddressNumberAndStreet, Latitude, Longitude,
                 AddressAccuracy),
               summarize,
               WnvPresent = sum(WnvPresent*NumMosquitos),
               NumMosquitos = sum(NumMosquitos),
               WnvNotPresent = NumMosquitos-WnvPresent,
               WnvProb=WnvPresent/NumMosquitos
)



sort(train$WnvPresent,decreasing=TRUE)[1:10]
sort(train$WnvProb,decreasing=TRUE)[1:1000]
sort(train$NumMosquitos,decreasing=TRUE)[1:100]
sort(train$WnvPresent+train$WnvNotPresent,decreasing=TRUE)[1:10]


write.csv(train, "data/train_weight.csv", row.names=F)
