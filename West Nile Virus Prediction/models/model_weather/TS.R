library(Metrics)
library(data.table) 
library(ggplot2)
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train11.csv")
#test <- fread("data/test5.csv")

train<-x[order(x$Block,x$Date), ]

if(1==0){
ts10=ts(train[train$Block==10,]$NumMosquitos, start = 2007, freq = 200)
plot(ts10)
train[train$Block==10]$Date
}

train$Date = as.Date(train$Date, "%Y-%m-%d")
ts=list()
for (i in unique(train$Block)){
  ts[[i]]=train[train$Block==i,]
}

unique(train$Block)
ggplot(ts[[10]], aes(Date, NumMosquitos)) + geom_line() + xlab("") +ylab("NumMosquitos")+ ggtitle("Block10")

ggplot(ts[[98]], aes(Date, NumMosquitos)) + geom_line() + xlab("") +ylab("NumMosquitos")+ ggtitle("Block98")

ggplot(ts[[50]], aes(Date, NumMosquitos)) + geom_line() + xlab("") +ylab("NumMosquitos")+ ggtitle("Block50")
