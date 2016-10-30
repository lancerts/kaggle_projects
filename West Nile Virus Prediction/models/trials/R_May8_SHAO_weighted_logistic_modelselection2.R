
#library(h2o)
#h2oServer <- h2o.init()   ##we'd like to try this, but does not initialize
library(data.table)   ## load data in quickly with fread
library(Metrics)
rm(list=ls())
x <- fread("../data/train.csv")
test <- fread("../data/test.csv")

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
x[,dYear:=as.numeric(substr(x$Date,1,4))]
test[,dMonth:=substr(test$Date,6,7)]

### Start modeling
## use 2011 as a cross validation year; x1 will include the other three years;x2 will include 2011
x1<-x[dYear!=2011,]
x2<-x[dYear==2011,]


## fit a logistic regression model using just three key fields
fitCv<-glm(WnvPresent ~dMonth + Species2 + Block+Latitude+Longitude+Species2:dMonth, data = x1, family = "binomial",weights=x1$NumMosquitos)
summary(fitCv)

upper=formula(WnvPresent ~dYear+ dMonth + Species2 + Block+Latitude+Longitude+Species2:dMonth+Species2:dYear)
#+Species2:Latitude+Species2:Longitude
fitCvselection=step(fitCv, scope=list(lower=formula(WnvPresent ~1), upper=upper),  trace=TRUE, direction="both")
fitCvselection$anova

p2<-predict(fitCvselection, newdata = x2, type = "response")
#p2<-predict(fitCv, newdata = x2, type = "response")
auc(x2$WnvPresent,p2)


fitSubmit<-glm(WnvPresent ~ dMonth + Species2 + Block+Latitude+Longitude+dMonth:Species2, data = x, family = "binomial",weights=x$NumMosquitos)
#final=step(fitSubmit, scope=list(lower=formula(fitSubmit), upper= ~ .^2),  trace=TRUE, direction="both")

pSubmit<-predict(fitSubmit, newdata = test, type = "response")





submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"weighted_logistic_selection1.csv",row.names=FALSE,quote=FALSE)
