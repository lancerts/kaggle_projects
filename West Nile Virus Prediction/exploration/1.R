library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
a=fread("data/train2_expand.csv")
b=fread("data/train8.csv")
c=fread("data/train_expand.csv")
nrow(a)

a$NumMosquitos[1:1000]
b$NumMosquitos[1:100]
c$NumMosquitos[1:1000]

norm(as.matrix(a$Week-b$Week),"F")
norm(as.matrix(a$Year-b$Year),"F")
norm(as.matrix(a$Block-b$Block),"F")
norm(as.matrix(a$Latitude-b$Latitude),"F")
norm(as.matrix(a$Longitude-b$Longitude),"F")
norm(as.matrix(a$NumMosquitos-b$NumMosquitos),"F")
norm(as.matrix(a$WnvPresent-b$WnvPresent),"F")^2

n=3
a$WnvPresent[(1000*n-1000):(1000*n)]
b$WnvPresent[(1000*n-1000):(1000*n)]


norm(as.matrix(as.integer(b$WnvPresent)-as.integer(b$WnvPresent1)),"F")^2