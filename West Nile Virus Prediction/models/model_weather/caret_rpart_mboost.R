library(Metrics)
library(data.table)   ## load data in quickly with fread
library(plyr) 
rm(list=ls())
#setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train4B.csv")
test <- fread("data/test4B.csv")


x$WnvPresent <- revalue(as.factor(x$WnvPresent), c("1" = "Yes", "0" = "No"))
x<-data.frame(x)
test<-data.frame(test)


library(caret)
cv.ctrl <- trainControl(method = "repeatedcv",number=5, repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)


set.seed(30)
ada.grid <- expand.grid(.iter = c(150,200,250), .maxdepth=c(2,3,4), .nu=c(0.05,0.1,0.15))
tree1<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb,data = x, method = "ada", metric = "ROC",tuneGrid = ada.grid,  trControl = cv.ctrl)

tree1$results
tree1$bestTune

set.seed(30)
gamboost.grid <- expand.grid(.mstop = c(75,100,125), .prune=c("yes","no"))
tree2<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb,data = x, method = "gamboost", metric = "ROC",tuneGrid = gamboost.grid, trControl = cv.ctrl)

tree2$results
tree2$bestTune

set.seed(30)
gam.grid <- data.frame(.df = c(8: 14))
gam32<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb,data = x, method = "gam", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
summary(gam22)
gam32$results
gam32$bestTune

##submission

tree1.probs <- predict(tree1, test, type = "prob")$Yes

summary(tree1.probs)

submissionFile<-cbind(test$Id,tree1.probs)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B_tree1.csv",row.names=FALSE,quote=FALSE)

