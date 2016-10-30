library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
train <- fread("data/train4B.csv")
test <- fread("data/test4B.csv")

vSpecies<-c(as.character(train$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies[-which(vSpecies == "CULEX PIPIENS" |
                  vSpecies == "CULEX PIPIENS/RESTUANS" |
                  vSpecies == "CULEX RESTUANS")] = "CULEX OTHER"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))


train[,Species:=factor(vSpecies[1:nrow(train)],levels=unique(vSpecies))]
test[,Species:=factor(vSpecies[(nrow(train)+1):length(vSpecies)],levels=unique(vSpecies))]

mosaicplot(train$Species ~ train$WnvPresent, shade=FALSE, color=TRUE, 
           xlab="WnvPresent", ylab="Species")
mosaicplot(train$CodeSum ~ train$WnvPresent, shade=FALSE, color=TRUE, 
           xlab="WnvPresent", ylab="TrapMS")
mosaicplot(train$Cool==0 ~ train$WnvPresent, shade=FALSE, color=TRUE, 
           xlab="WnvPresent", ylab="Species")

boxplot(train$Tavg~ train$WnvPresent, 
        xlab="WnvPresent", ylab="Tavg")
boxplot(train$StnPressure  ~ train$WnvPresent, 
        xlab="WnvPresent", ylab="Tavg")

#Tmin has some significance, DewPoint,WetBulb,Sunrise,Sunset,ResultSpeed
train=data.frame(train)

require(corrgram)
corrgram.vars <- c("WnvPresent","Year","Week", "Latitude", "Longitude","Block","NumMosquitos","Tmin","Tmax", "Tavg",  "DewPoint","WetBulb","Heat","Cool","PrecipTotal","Sunrise","Sunset")
corrgram(train[,corrgram.vars], order=FALSE, 
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt, main="Wnv")