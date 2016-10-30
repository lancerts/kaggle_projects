###################################################################################
## This code is part of the kaggle project: West Nile Virus Prediction
## By Yuhui Zhang, and Shao Tang
## May. 8, 2015

###################################################################################

## load library
library(data.table)   ## load data in quickly with fread
library(plyr)
## load the data
rm(list=ls())
train <- fread("../data/train.csv")
test <- fread("../data/test.csv")

train.expand <- train[rep(seq(nrow(train)), train$NumMosquitos),]
## write it back
write.csv(train.expand , "data/train_expand.csv", row.names=F)
