library(Metrics)
library(data.table)   ## load data in quickly with fread
#setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train8.csv")
test <- fread("data/test4B.csv")

vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies[-which(vSpecies == "CULEX PIPIENS" |
                  vSpecies == "CULEX PIPIENS/RESTUANS" |
                  vSpecies == "CULEX RESTUANS")] = "CULEX OTHER"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))


x[,Species:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]


## GAM modelling
require(gam)
fitSubmit<- gam(WnvPresent~ s(Year)+s(Week) + Species + lo(Latitude, Longitude)+s(Block)+s(Week):Species+s(NumMosquitos)+s(Sunrise)+s(WetBulb), data = x, family="binomial", control=gam.control(epsilon=1e-05, bf.epsilon = 1e-05, maxit=1000, bf.maxit = 1000, trace=TRUE))
pSubmit<-predict(fitSubmit, newdata = test, type = "response")

summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/GAM_train8_test4B.csv",row.names=FALSE,quote=FALSE)




