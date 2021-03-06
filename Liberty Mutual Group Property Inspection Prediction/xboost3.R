library(readr)
library(xgboost)
library(data.table)
library(Matrix)
library(caret)
rm(list=ls())
setwd("C:/Users/tshao/Dropbox/kaggle/Liberty Mutual Group Property Inspection Prediction")
# The competition datafiles are in the directory ../input
# Read competition data files:
train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

# keep copy of ID variables for test and train data
train_Id <- train$Id
test_Id <- test$Id

# response variable from training data
train_y <- train$Hazard

# predictor variables from training
train_x <- subset(train, select = -c(Id, Hazard,T2_V10,T2_V7,T1_V13,T1_V10))
train_x <- sparse.model.matrix(~., data = train_x)

# predictor variables from test
test_x <- subset(test, select = -c(Id,T2_V10,T2_V7,T1_V13,T1_V10))
test_x <- sparse.model.matrix(~., data = test_x)


# build Gini functions for use in custom xgboost evaluation metric
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

NormalizedGini <- function(solution, submission) {
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}

# wrap up into a function to be called within xgboost.train
evalgini <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- NormalizedGini(as.numeric(labels),as.numeric(preds))
  return(list(metric = "Gini", value = err))
}


# Set xgboost parameters
param <- list("objective" = "reg:linear",
              "eta" = 0.002,
              "min_child_weight" = 6,
              "subsample" = .7,
              "colsample_bytree" = .7,
              "scale_pos_weight" = 1.0,
              "max_depth" = 8)

# Using 5000 rows for early stopping. 

set.seed(2015)
trainControlxgbTree <- trainControl(method = "repeatedcv",number = 3,repeats = 1,verboseIter = TRUE)
folds <- 5
size=round(nrow(train_x)/folds)
num_rounds <- 10000
vect=c(1:nrow(train_x))
fold=sample(vect,size)
xgtrain <- xgb.DMatrix(data = train_x[-fold,], label= train_y[-fold])
xgval <-  xgb.DMatrix(data = train_x[fold,], label= train_y[fold])
watchlist <- list(val=xgval, train=xgtrain)
 
bst2a <- train(x = train_x,
                 y = train_y,
                 method = "xgbTree",
                 tuneGrid = expand.grid(nrounds = 100*(10:15),
                                        eta = c(0.02),
                                        max_depth = c(5:9)),                            
                 trControl = trainControlxgbTree,
                 watchlist = list(val = xgval,train = xgtrain),
                eval_metric = "error",
                 min_child_weight = 10,
                 early.stop.round = 50,
                 printEveryN = 100,
                 subsample = 0.6,
                 verbose = 1,
                 colsample_bytree =0.8,
                 base_score = 0.5,
#                maximize=FALSE
 )
plot(bst2a)
xgtest <- xgb.DMatrix(data = test_x)
preds2a <- predict(bst2a,newdata=as.data.frame(xgtest), type = "prob")
preds_a=rbind(preds_a,preds2a)
  
  bst2b <- train(x=train_x,y=train_y,params = param, data = xgtrain, nround=num_rounds, print.every.n = 100, watchlist=watchlist, early.stop.round = 100, maximize = FALSE)
  preds2b <- predict(bst2b,xgtest) 
  preds_b=rbind(preds_b,preds2b)
  vect=setdiff(vect,fold)
}




# Output submission
pred2a_df = data.frame(Id = test_Id, Hazard= colSums(preds_a))
write.table(pred2a_df, file = 'submission/xgboost_2a.csv', row.names = F, col.names = T, sep = ",", quote = F)



# Output submission
pred2b_df = data.frame(Id = test_Id, Hazard=  colSums(preds_b))
write.table(pred2b_df, file = 'submission/xgboost_2b.csv', row.names = F, col.names = T, sep = ",", quote = F)
