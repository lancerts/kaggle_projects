library(Metrics)
library(data.table)   ## load data in quickly with fread
#setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train8.csv")
test <- fread("data/test4B.csv")




## GAM modelling
require(gam)
fitSubmit<- gam(WnvPresent~ s(Year)+s(Week) + lo(Latitude, Longitude)+s(NumMosquitos)+s(Sunrise)+s(WetBulb), data = x, family="binomial", control=gam.control(epsilon=1e-06, bf.epsilon = 1e-06, maxit=1000, bf.maxit = 1000, trace=TRUE))
pSubmit<-predict(fitSubmit, newdata = test, type = "response")

summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/GAM4_train8_test4B.csv",row.names=FALSE,quote=FALSE)




