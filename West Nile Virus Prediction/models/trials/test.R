library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
test <- fread("data/test.csv")
library(plyr)
test[,NumMosquitos:=1]
test <- ddply(test,
              .(  Species, Latitude, Longitude), 
              summarize,
              NumMosquitos = sum(NumMosquitos)
)
nrow(test)