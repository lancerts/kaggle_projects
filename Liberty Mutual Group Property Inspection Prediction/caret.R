library(data.table)   ## load data in quickly with fread
rm(list=ls())
#setwd("E:/Dropbox/kaggle/West Nile Virus Prediction")
setwd("C:/Users/tshao/Dropbox/kaggle/Liberty Mutual Group Property Inspection Prediction")
train <- fread("data/train.csv")
test <- fread("data/test.csv")

#drop ID column in train set
train<-data.frame(train)[,-1]
test<-data.frame(test)

#No missing values
#length(train[is.na(train)])
#length(test[is.na(test)])

#Gini code from kaggle
SumModelGini <- function(solution, submission) {
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = (1:nrow(df))/nrow(df)
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  return(sum(df$Gini))
}

NormalizedGini <- function(data, lev = NULL, model = NULL) {
  solution=data$obs
  submission=data$pred
  result=SumModelGini(solution, submission) / SumModelGini(solution, solution)
  names(result) <- "Gini"
  result
}

#simple linear model LB score 0.326353

#lm<- lm(Hazard ~ .,data = train)


library(caret)
cv.ctrl <- trainControl(method = "repeatedcv",number=3, repeats = 2,summaryFunction = NormalizedGini,verboseIter = TRUE)


#random forest
set.seed(30)
rf.grid <- expand.grid( .mtry = c(2:3))
rf <- train(Hazard ~ ., data=train, method = "rf", metric = "Gini", trControl = cv.ctrl, verbose = TRUE,importance = TRUE,tuneGrid=rf.grid)
rf$results
rf$bestTune
plot(varImp(rf,scale=FALSE))


#boosting tree LB score 0.350240
set.seed(30)
gbm <- train(Hazard ~ ., data=train, method = "gbm", metric = "Gini", trControl = cv.ctrl, verbose = FALSE)
gbm$results
gbm$bestTune
plot(varImp(gbm, scale = FALSE))







#conditional inference random forest
set.seed(30)
cf.grid <- expand.grid( .mtry = c(4:6))
cf<- train(Hazard ~ ., data=train, method = "cforest", metric = "Gini", trControl = cv.ctrl,tuneGrid=cf.grid,controls = cforest_unbiased(ntree = 500))
cf$results
varImp(cf, scale = FALSE)





##submission
response=c()
for (i in 1:nrow(test)){
  response=c(response,predict(cf,test[i,],type="raw"))
  if (i%%5000==0) print(i)
}

  

submissionFile<-cbind(test$Id,response)
colnames(submissionFile)<-c("Id","Hazard")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submission/cf.csv",row.names=FALSE,quote=FALSE)

