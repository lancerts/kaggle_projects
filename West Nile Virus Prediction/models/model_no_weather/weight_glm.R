
library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
#setwd("../")
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#x <- fread("data/train.csv")
x <- fread("data/train_weight.csv")
y <- fread("data/train5.csv")

vSpeciesx<-as.character(x$Species)
vSpeciesx[-which(vSpeciesx == "CULEX PIPIENS" |
                  vSpeciesx == "CULEX PIPIENS/RESTUANS" |
                  vSpeciesx == "CULEX RESTUANS")] = "CULEX OTHER"
vSpeciesx<-factor(vSpeciesx,levels=unique(vSpeciesx))
x[,Species:=factor(vSpeciesx[1:nrow(x)],levels=unique(vSpeciesx))]
vSpeciesy<-as.character(y$Species)
vSpeciesy[-which(vSpeciesy == "CULEX PIPIENS" |
                   vSpeciesy == "CULEX PIPIENS/RESTUANS" |
                   vSpeciesy == "CULEX RESTUANS")] = "CULEX OTHER"
vSpeciesy<-factor(vSpeciesy,levels=unique(vSpeciesy))
y[,Species:=factor(vSpeciesy[1:nrow(y)],levels=unique(vSpeciesy))]


## also add some fields for components of the date using simple substrings
x[,Month:=as.factor(paste(substr(x$Date,6,7)))]
x[,Year:=as.numeric(paste(substr(x$Date,1,4)))]
x$Date = as.Date(x$Date, format="%Y-%m-%d")
xsDate = as.Date(paste0(x$Year, "0101"), format="%Y%m%d")
x$Week = as.numeric(paste(floor((x$Date - xsDate + 1)/7)))


### Start modeling
## use 2011 as a cross validation year; x1 will include the other three years;x2 will include 2011
x1<-x[Year!=2011,]
xcv<-x[Year==2011,]
y1<-y[Year!=2011,]
ycv<-y[Year==2011,]


## fit a logistic regression model using just three key fields
fitCv1<-glm(cbind(x1$WnvPresent,x1$WnvNotPresent) ~   Week+Species + Block+Latitude+Longitude+NumMosquitos, data = x1, family = "binomial")  
coef(fitCv1) 
p1<-predict(fitCv1, newdata = ycv, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(ycv$WnvPresent,p1)

fitCv2<-glm(WnvProb ~  Week+Species + Block+Latitude+Longitude+NumMosquitos, data = x1, family = "binomial",weight=x1$NumMosquitos)  
coef(fitCv2) 
summary(fitCv2)
p2<-predict(fitCv2, newdata = ycv, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(ycv$WnvPresent,p2)

## fit a logistic regression model using just three key fields
fitCv3<-glm(WnvPresent ~  Week+Species + Block+Latitude+Longitude+NumMosquitos, data = y1, family = "binomial")  
coef(fitCv3) 
summary(fitCv3)
p3<-predict(fitCv3, newdata = ycv, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(ycv$WnvPresent,p3)

####The coef is basically same for all parameters excpet for little relative difference among species other which is not significant and intercept
(coef(fitCv1)-coef(fitCv2))/coef(fitCv1)
(coef(fitCv1)-coef(fitCv3))/coef(fitCv1)
(coef(fitCv2)-coef(fitCv3))/coef(fitCv2)