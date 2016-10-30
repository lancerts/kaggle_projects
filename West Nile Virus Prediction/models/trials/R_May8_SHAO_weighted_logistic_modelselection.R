library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train2.csv")
test <- fread("data/test2.csv")

## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species2:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

## also add some fields for components of the date using simple substrings
x[,dMonth:=substr(x$Date,6,7)]
x[,dYear:=substr(x$Date,1,4)]
test[,dMonth:=substr(test$Date,6,7)]
test[,dYear:=substr(test$Date,1,4)]
### Start modeling
## use 2011 as a cross validation year; x1 will include the other three years;x2 will include 2011
x1<-x[dYear!=2011,]
x2<-x[dYear==2011,]
#levels(factor(x1$Species2))
#levels(factor(x2$Species2))

## fit a logistic regression model using just three key fields
fitCv<-glm(WnvPresent ~  dMonth+Species + Block+Latitude+Longitude+dMonth:Species, data = x1, family = "quasibinomial",weight=x1$NumMosquitos)
summary(fitCv)
#Species2:dMonth

p2<-predict(fitCv, newdata = x2, type = "response")
auc(x2$WnvPresent,p2)


newx2=cbind(x2,p2)
newx2[,mean(p2),by="Species"][order(V1)]
table(newx2$Species2)

fitSubmit<-glm(WnvPresent ~ dMonth + Species2 + Block+Latitude+Longitude, data = x, family = "binomial",weight=x$AddressAccuracy)
#final=step(fitSubmit, scope=list(lower=formula(fitSubmit), upper= ~ .^2),  trace=TRUE, direction="both")

pSubmit<-predict(fitSubmit, newdata = test, type = "response")
newtest=cbind(test,pSubmit)
newtest[,mean(pSubmit),by="Species"][order(V1)]



submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"exapnd_logistic_selection2.csv",row.names=FALSE,quote=FALSE)




###removed
upper=formula(WnvPresent ~dMonth + Species2 + Block+Latitude+Longitude+Latitude^2+Longitude^2+Latitude:Longitude+Block:Longitude:Latitude+Species2:dMonth)
#+Species2:Latitude+Species2:Longitude
fitCvselection=step(fitCv, scope=list(lower=formula(WnvPresent ~1), upper=upper),  trace=TRUE, direction="both")
fitCvselection$anova

p2<-predict(fitCvselection, newdata = x2, type = "response")
