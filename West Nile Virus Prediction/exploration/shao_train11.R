library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
train <- fread("data/train.csv")
test <- fread("data/test.csv")


# Feature Engineering
## Date

train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)

library(lubridate)
## train set
train$Year <- as.numeric(year(train$Date))
train$Month <- factor(months(train$Date))
train$Week <- as.numeric(week(train$Date))
train$Weekday <- factor(weekdays(train$Date))
## test set
test$Year <- as.numeric(year(test$Date))
test$Month <- factor(months(test$Date))
test$Week <- as.numeric(week(test$Date))
test$Weekday <- factor(weekdays(test$Date))

## Species
train$Species<-factor(train$Species)
test$Species<-factor(test$Species)
test[test$Species=="UNSPECIFIED CULEX", "Species"] <- "CULEX ERRATICUS"



library(plyr)



train <- ddply(train,
               .( Date,Species, Block, Latitude, Longitude,
                  Year, Month, Week),
               summarize,
               NumMosquitos = sum(NumMosquitos),
               WnvPresent = as.integer(as.logical(sum(WnvPresent)))
)


if(0==1){
test[,"NumMosquitos"] <- 1
test <- ddply(test, 
              .( Species, Block,  Latitude, Longitude,
                  Year, Month, Week),
                    transform,
                    NumMosquitos = sum(NumMosquitos)
)
m=sort(test$NumMosquitos,decreasing=T)[1]
## change scale
est.num <- function(num){
  n1 <- (num-1)*50
  n2 <- num*50
  result <- mean(train[train$NumMosquitos>=n1 & train$NumMosquitos<=n2, "NumMosquitos"])
  if (result == "NaN") {
    result=(n1+n2)/2
  }
  return(result)
}
mean.num <- sapply(1:m, est.num)
test$NumMosquitos <- mean.num[test$NumMosquitos]
summary(test$NumMosquitos)
}






write.csv(train, "data/train11.csv", row.names=F)




#write.csv(test, "data/test9.csv", row.names=F)


