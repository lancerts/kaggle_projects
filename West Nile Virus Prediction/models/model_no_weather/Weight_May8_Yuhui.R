###################################################################################
## This code is part of the kaggle project: West Nile Virus Prediction
## By Yuhui Zhang, and Shao Tang
## May. 8, 2015

## MODEL DESCRIPTION
###################################################################################

## LOAD
library(randomForest)
## load the data
train <- read.csv("../data/train2.csv")
test <- read.csv("../data/test2.csv")
# train$WnvPresent <- as.factor(train$WnvPresent)

## WEIGHT
weight.function <- function(present, number){
if (present==TRUE) r = 52-sqrt(number)
else r = sqrt(number)
return(r)
}
weight <- mapply(weight.function, train$WnvPresent, train$NumMosquitos)


## BUILD MODEL 1
train <- data.frame(train)
train2 <- train[,-c(1, 2, 5, 6, 7, 12, 13)]
model <- glm(WnvPresent ~ Month + Species + Block, data = train, family = "binomial", weights=weight)
model$xlevels[["Species"]] <- union(model$xlevels[["Species"]], levels(test$Species))

## BUILD MODEL 2
## train <- data.frame(train)
## train2 <- train[,-c(1, 2, 5, 6, 7, 12, 13)]
## model <- randomForest(WnvPresent~., 
## 	data = train2, 
## 	importance=TRUE,
## 	keep.forest=TRUE
## )
## print(model)
## #what are the important variables (via permutation)
## varImpPlot(model, type=1)

## SUBMIT
pSubmit<-predict(model, newdata = test, type = "response")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)
submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"../submission/Weight2_May8.csv",row.names=FALSE,quote=FALSE)
