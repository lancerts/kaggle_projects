###################################################################################
## This code is part of the kaggle project: West Nile Virus Prediction
## By Yuhui Zhang, and Shao Tang
## May. 3, 2015

## MODEL DESCRIPTION
## Predictors: Month, Species, Block
## Algorithm: logistic regression
###################################################################################

## LOAD
## load the libraries
library(data.table)   ## load data in quickly with fread
library(Metrics)
library(pROC)
## load the data
train <- fread("../data/train.csv")
test <- fread("../data/test.csv")

## FEATURE ENGINEERING
## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
species <- c(as.character(train$Species),as.character(test$Species))
species[species=="UNSPECIFIED CULEX"] <- "CULEX ERRATICUS"
species <- factor(species,levels=unique(species))
## data.table syntax for adding a column; could overwrite the existing column as well
train[,Species2:=factor(species[1:nrow(train)], levels=unique(species))]
test[,Species2:=factor(species[(nrow(train)+1):length(species)], levels=unique(species))]
## also add some fields for components of the date using simple substrings
train[,dMonth:=substr(train$Date,6,7)]
train[,dYear:=substr(train$Date,1,4)]
test[,dMonth:=substr(test$Date,6,7)]
test[,dYear:=substr(test$Date,1,4)]
## month and year transformed to factors
train[,dMonth:=factor(train$dMonth, levels=unique(dMonth))]
test[,dMonth:=factor(test$dMonth, levels=unique(dMonth))]
train[,dYear:=factor(train$dYear, levels=unique(dYear))]
test[,dYear:=factor(test$dYear, levels=unique(dYear))]

## SPLIT TRAIN SET
## split the train set into "train" and "cross.validation" sets
## sample size
sample.size <- floor(0.75 * nrow(train))
## set the seed to make your partition reproductible
set.seed(123)
train.index <- sample(seq_len(nrow(train)), size = sample.size)
## split
train2 <- train[train.index, ]
cross.validation <- train[-train.index, ]

## BUILD MODEL
model <- glm(WnvPresent ~ dMonth + Species2 + Block, data = train2, family = "binomial")
model$xlevels[["Species2"]] <- union(model$xlevels[["Species2"]], levels(cross.validation$Species2))

## MODEL EVALUATION
cross.pred.prob <- predict(model, newdata = cross.validation, type = "response")
## check the ROC of the model
roc.plot <- roc(cross.validation$WnvPresent, cross.pred.prob)
plot(roc.plot)
## check for a reasonable AUC of the model (KAGGLE"S METRIC!)
cross.obs <- cross.validation$WnvPresent
auc(cross.obs, cross.pred.prob)
## show the confusion matrix (with threshold 0.1)
cross.pred <- ifelse(cross.pred.prob >= 0.1, 1, 0)
confusion.matrix <- table(cross.pred, cross.obs)
confusion.matrix
## calculate the error in testing set
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy

## SUBMIT
pSubmit<-predict(model, newdata = test, type = "response")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)
submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"../submission/R_First_May3_Yuhui.csv",row.names=FALSE,quote=FALSE)
