library(Metrics)
library(data.table)
library(plyr)
rm(list=ls())
setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
#setwd("C:/Users/tshao/Dropbox/kaggle/West Nile Virus Prediction")
x <- fread("data/train4B.csv")
test <- fread("data/test4B.csv")


x$WnvPresent <- revalue(as.factor(x$WnvPresent), c("1" = "Yes", "0" = "No"))
x<-data.frame(x)
test<-data.frame(test)
weights=c(rep(1,457),rep(2,nrow(x)-457))
x=cbind(x,weights)





library(caret)
cv.ctrl <- trainControl(method = "repeatedcv",number=5, repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)



set.seed(30)
x1 <-  rbind(x[which(x$WnvPresent=="Yes"),],x[sample(which(x$WnvPresent=="No"),457,replace=F),])
nrow(x[which(x$WnvPresent=="Yes"),])

gam.grid <- data.frame(.df = c(4: 8))
gam22<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb +TrapNumber,data = x1, method = "gamSpline", metric = "ROC", trControl = cv.ctrl, tuneGrid = gam.grid  )
summary(gam22)
gam22$results
gam22$bestTune


#conditional inference random forest
set.seed(30)
x1 <-  rbind(x[which(x$WnvPresent=="Yes"),],x[sample(which(x$WnvPresent=="No"),457,replace=F),])
#nrow(x[which(x$WnvPresent=="Yes"),])
cf.grid <- expand.grid( .mtry = c(1:5))
tree1<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos+Sunrise+WetBulb+TrapNumber+Tavg, data=x1, method = "cforest", metric = "ROC", trControl = cv.ctrl,tuneGrid=cf.grid,controls = cforest_unbiased(ntree = 100))
tree1$results
varImp(tree1, scale = FALSE)
plot(tree1)

##submission

gam22.probs <- predict(gam22, test, type = "prob")$Yes

summary(gam22.probs)

submissionFile<-cbind(test$Id,gam22.probs)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B_gam22.csv",row.names=FALSE,quote=FALSE)

