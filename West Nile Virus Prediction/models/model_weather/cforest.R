library(Metrics)
library(data.table)   ## load data in quickly with fread
library(plyr) 
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train4B.csv")
test <- fread("data/test4B.csv")

vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies[-which(vSpecies == "CULEX PIPIENS" |
                  vSpecies == "CULEX PIPIENS/RESTUANS" |
                  vSpecies == "CULEX RESTUANS")] = "CULEX OTHER"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))
x[,Species:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

x$WnvPresent <- revalue(as.factor(x$WnvPresent), c("1" = "Yes", "0" = "No"))
x<-data.frame(x)
test<-data.frame(test)

library(caret)
cv.ctrl <- trainControl(method = "repeatedcv",number=5, repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

#conditional inference random forest
set.seed(30)
cf.grid <- expand.grid( .mtry = c(2:3))
tree1<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos +Block+Sunrise+WetBulb, data=x, method = "cforest", metric = "ROC", trControl = cv.ctrl,tuneGrid=cf.grid,controls = cforest_unbiased(ntree = 100))
tree1$results
varImp(tree1, scale = FALSE)

nrow(test)
#116293
n=30000
tree1.probs1<-predict(tree1, test[1:n,], type = "prob")$Yes
tree1.probs2<-predict(tree1, test[(n+1):(2*n),], type = "prob")$Yes
tree1.probs3<-predict(tree1, test[(2*n+1):(3*n),], type = "prob")$Yes
tree1.probs4<-predict(tree1, test[(3*n+1):116293,], type = "prob")$Yes
tree1.probs<-c(tree1.probs1,tree1.probs2,tree1.probs3,tree1.probs4)
summary(tree1.probs)

submissionFile<-cbind(test$Id,tree1.probs)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B_cforest1.csv",row.names=FALSE,quote=FALSE)