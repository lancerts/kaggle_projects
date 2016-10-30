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
library(gam)
###model

#glm 

set.seed(30)
glm1<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+Species+NumMosquitos +Block+DewPoint+Week:Species+I(Heat=="0"),data = x, method = "glm", metric = "ROC", trControl = cv.ctrl)
summary(glm1)
glm1

#gam
set.seed(30)
gam.grid <- data.frame(.df = c(8: 14))
gam1<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos +Block+Sunrise,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
gam1$results



set.seed(30)
gam.grid <- data.frame(.df = c(8: 14))
gam2<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos +Block+Sunrise+WetBulb,data = x, method = "gamSpline", metric = "ROC", tuneGrid = gam.grid, trControl = cv.ctrl)
gam2$results
gam2$bestTune
#df 13

set.seed(30)
gam.grid <- data.frame(.df = c(8: 14))
gam3<- gam(WnvPresent ~ s(Year,df=13)+ s(Week,df=13) + Species + lo(Latitude, Longitude)+s(Block,df=13)+s(Week,df=13):Species+s(NumMosquitos,df=13)+s(Sunrise,df=13)+s(DewPoint,df=13), data = x, family="binomial",control=gam.control(epsilon=1e-05, bf.epsilon = 1e-04, maxit=1000, bf.maxit = 1000, trace=TRUE))






#conditional inference random forest
set.seed(30)
cf.grid <- expand.grid( .mtry = c(2:3))
tree1<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos +Block+Sunrise+WetBulb, data=x, method = "cforest", metric = "ROC", trControl = cv.ctrl,tuneGrid=cf.grid,controls = cforest_unbiased(ntree = 100))
tree1$results
varImp(tree1, scale = FALSE)


set.seed(30)
cf.grid <- expand.grid( .mtry = c(3:7))
tree2<- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos +Block+Species+Sunrise+WetBulb, data=x, method = "cforest", metric = "ROC", trControl = cv.ctrl,tuneGrid=cf.grid,controls = cforest_unbiased(ntree = 100))
tree2$results
trellis.par.set(caretTheme())
plot(tree2)


#boosting tree
set.seed(30)
gbmGrid <- expand.grid(.interaction.depth = (2:4) * 2, .n.trees = (10:15)*25, .shrinkage = c(0.025,0.03,0.02), .n.minobsinnode=c(3:5))
gbmFit1 <- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos +Block+Sunrise+WetBulb+Species, data=x, method = "gbm", metric = "ROC", trControl = cv.ctrl, verbose = FALSE, bag.fraction = 0.5, tuneGrid = gbmGrid)
gbmFit1$results
gbmFit1$bestTune
Imp1 <- varImp(gbmFit1, scale = FALSE)
Imp1

set.seed(30)
gbmGrid <- expand.grid(.interaction.depth = (2:4) * 2, .n.trees = (10:15)*25, .shrinkage = c(0.025,0.015,0.02), .n.minobsinnode=c(3:5))
gbmFit2 <- train(WnvPresent ~ Year+ Week+ Latitude+ Longitude+NumMosquitos +Block+Sunrise+WetBulb, data=x, method = "gbm", metric = "ROC", trControl = cv.ctrl, verbose = FALSE, bag.fraction = 0.5, tuneGrid = gbmGrid)
gbmFit2$results
gbmFit2$bestTune
Imp2<- varImp(gbmFit2, scale = FALSE)
Imp2

##submission

gam3.probs <- predict(gam3, test, type = "response")
gbm2.probs<-predict(gbmFit2, test, type = "prob")$Yes
tree1.probs<-predict(tree1, test, type = "prob")$Yes

summary(gam3.probs)
summary(gbm2.probs)

submissionFile<-cbind(test$Id,gbm2.probs)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B_gbm2.csv",row.names=FALSE,quote=FALSE)


submissionFile<-cbind(test$Id,tree1.probs)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/train4B_test4B_cforest1.csv",row.names=FALSE,quote=FALSE)