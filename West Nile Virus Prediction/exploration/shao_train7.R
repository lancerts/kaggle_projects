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

train <- train[rep(seq(nrow(train)), train$NumMosquitos),]
train[,"NumMosquitos"] <- 1
train <- ddply(train,
               .(Date, Address, Species, Block, Street, Trap,
                 AddressNumberAndStreet, Latitude, Longitude,
                 AddressAccuracy, Year, Month, Week, Weekday),
               summarize,
               WnvPresent = sum(WnvPresent*NumMosquitos),
               NumMosquitos = sum(NumMosquitos),
               WnvNotPresent = NumMosquitos-WnvPresent,
               WnvProb=WnvPresent/NumMosquitos
)


if(0==1){
test[,"NumMosquitos"] <- 1
test <- ddply(test, 
              .(Date, Address, Species, Block, Street, Trap,
                   AddressNumberAndStreet, Latitude, Longitude,
                  AddressAccuracy, Year, Month, Week, Weekday),
                    transform,
                    NumMosquitos = sum(NumMosquitos)
)
sort(test$NumMosquitos,decreasing=T)[1:100]
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
mean.num <- sapply(1:26, est.num)
test$NumMosquitos <- mean.num[test$NumMosquitos]
summary(test$NumMosquitos)
}



## Trap

## train set
train$TrapNumber <- as.integer(substr(as.character(train$Trap), 2, 4))
train$TrapMS <- as.factor(substr(as.character(train$Trap), 5, 5))
levels(train$TrapMS) <- c("M", "B", "C")
## test set
test$TrapNumber <- as.integer(substr(as.character(test$Trap), 2, 4))
test$TrapMS <- as.factor(substr(as.character(test$Trap), 5, 5))
levels(test$TrapMS) <- c("M", "A", "B", "C")


## Distances to the two weather stations
## Calculate distance in kilometers between two points
## (Unit is km, but it doesn't matter)
## From https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6371
  d <- R * c
  return(d)
}
long.stn1 <- -87.933
lat.stn1 <- 41.995
long.stn2 <- -87.752
lat.stn2 <- 41.786
train[, "DisStn1"] <- earth.dist(train$Longitude, train$Latitude,
                                 rep(long.stn1, nrow(train)),
                                 rep(lat.stn1, nrow(train)))
train[, "DisStn2"] <- earth.dist(train$Longitude, train$Latitude,
                                 rep(long.stn2, nrow(train)),
                                 rep(lat.stn2, nrow(train)))
test[, "DisStn1"] <- earth.dist(test$Longitude, test$Latitude,
                                rep(long.stn1, nrow(test)),
                                rep(lat.stn1, nrow(test)))
test[, "DisStn2"] <- earth.dist(test$Longitude, test$Latitude,
                                rep(long.stn2, nrow(test)),
                                rep(lat.stn2, nrow(test)))
train[, "ClosestStn"] <- ifelse(train$DisStn1 < train$DisStn2, 1, 2)
test[, "ClosestStn"] <- ifelse(test$DisStn1 < test$DisStn2, 1, 2)



# Weather data

weather.data <- read.csv("data/weather.csv", na.strings=c("M", "-", "", " "))


## Notice of the weather data

## Data engineering
### Separate the data set by station

weather.data.split <- split(weather.data, weather.data$Station)
weather.stn1 <- weather.data.split[[1]]
weather.stn2 <- weather.data.split[[2]]



### Sunrise and Sunset

weather.stn2$Sunrise <- weather.stn1$Sunrise
weather.stn2$Sunset <- weather.stn1$Sunset


### Depart


normal.tmp <- weather.stn1$Tavg - weather.stn1$Depart
weather.stn2$Depart <- weather.stn2$Tavg - normal.tmp


### CodeSum


code.A <- unique(unlist(strsplit(unique(as.character(weather.stn1$CodeSum)), " ")))
code.B <- unique(unlist(strsplit(unique(as.character(weather.stn2$CodeSum)), " ")))
code <- union(code.A, code.B)[-1]

code.name <- paste(rep("Code.", length(code)), code, sep="")





## rewrite the reguar expression of "FG", "TS" and "RA" to avoid wrong matching
code[2] <- "^RA | RA$| RA |^RA$"
code[8] <- "^TS | TS$| TS |^TS$"


for(i in 1:length(code)) {
new.code <- code[i]
new.code.name <- code.name[i]
weather.stn1[, new.code.name] <- grepl(new.code, weather.stn1$CodeSum)
weather.stn2[, new.code.name] <- grepl(new.code, weather.stn2$CodeSum)
}

#This Water1 is useless, remove it from data frames.
weather.stn1 <- weather.stn1[,-15]
weather.stn2 <- weather.stn2[,-15]


### SnowFall
#SnowFall is not a good predictor, since the time is from May to October.
weather.stn1 <- weather.stn1[,-15]
weather.stn2 <- weather.stn2[,-15]


### Date

weather.stn1$Date <- as.Date(weather.stn1$Date)
weather.stn2$Date <- as.Date(weather.stn2$Date)


### PrecipTotal

weather.stn1$PrecipTotal <- as.numeric(weather.stn1$PrecipTotal)
weather.stn2$PrecipTotal <- as.numeric(weather.stn2$PrecipTotal)



# Combined Main and Weather Data Set
## Merge train/test and weather.stn1/weather.stn2
## combine the weather.stn1 and weather.stn2 to one data frame first
weather.stn <- rbind(weather.stn1, weather.stn2)
train$Id  <- 1:nrow(train)
## merge
train <- merge(train, weather.stn,   by.x=c("Date", "ClosestStn"), by.y=c("Date", "Station"))
train<-train[order(train$Id), ]
write.csv(train, "data/train7.csv", row.names=F)



test <- merge(test, weather.stn, by.x=c("Date", "ClosestStn"), by.y=c("Date", "Station"))
test<-test[order(test$Id), ]


#write.csv(test, "data/test5.csv", row.names=F)


