library(glmnet)
library(data.table)   ## load data in quickly with fread
library(Metrics)

rm(list=ls())
setwd("../")
train <- fread("data/train.csv")
test <- fread("data/test.csv")


## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(train$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
train[,Species2:=factor(vSpecies[1:nrow(train)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(train)+1):length(vSpecies)],levels=unique(vSpecies))]

## also add some fields for components of the date using simple substrings
train[,dMonth:=substr(train$Date,6,7)]
train[,dYear:=substr(train$Date,1,4)]
test[,dMonth:=substr(test$Date,6,7)]
test[,dYear:=substr(test$Date,1,4)]
test[,dMonth05:=0]

#The year of train data set and test data set is completely different!
levels(factor(train$dYear))
levels(factor(test$dYear))


x=model.matrix(~Species2+dMonth+Block+WnvPresent-1, data=train)
xtest=model.matrix(~Species2+dMonth+dMonth05+Block-1, data=test)



## sample some data as to check the auc; 
set.seed(1)
subset=sample(nrow(x), 500, replace=FALSE)
xtrain<-x[-subset,-dim(x)[2]]
ytain=as.factor(x[-subset,dim(x)[2]])
xvalidation<-x[subset,-dim(x)[2]]
yvalidation=x[subset,dim(x)[2]]


## fit a logistic regression model using  cv to tune parameters
alpha=0.5
l=50
grid.x =exp(seq (2,-10, length =l))
fit<-cv.glmnet(x=xtrain, y=ytain, family = "binomial", alpha=alpha, type.measure="auc",lambda=grid.x)
plot(fit) #This plot is the auc as a function of lambda while auc is calculated by cv

p<-predict(fit, newx = xvalidation, s = 0, type = "coefficients")
auc(yvalidation,p)

fitSubmit<-glmnet(x=x[,-dim(x)[2]], y=x[,dim(x)[2]], family = "binomial", alpha=alpha)

# plot(fitSubmit)  solution path

pSubmit<-predict(fitSubmit, newx = as.matrix(xtest),s = fit$lambda.min, type = "response")

## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"shao1_glmnet.csv",row.names=FALSE,quote=FALSE)
