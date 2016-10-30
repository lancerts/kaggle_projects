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
folds <- 5
size=round(nrow(train_x)/folds)
xgtest <- xgb.DMatrix(data = test_x)
num_rounds <- 10000
vect=c(1:nrow(train_x))
preds_a=c()
preds_b=c()
set.seed(2015)
for (i in c(1:folds)) {
  # Set xgboost test and training and validation datasets
  if (i != folds){
  fold=sample(vect,size)}
  else  {fold=vect}

  xgtrain <- xgb.DMatrix(data = train_x[-fold,], label= train_y[-fold])
  xgval <-  xgb.DMatrix(data = train_x[fold,], label= train_y[fold])
  
  # setup watchlist to enable train and validation, validation must be first for early stopping
  watchlist <- list(val=xgval, train=xgtrain)
  # to train with watchlist, use xgb.train, which contains more advanced features
  bst2a <- xgb.train(params = param, data = xgtrain, feval = evalgini, nround=num_rounds, print.every.n = 100, watchlist=watchlist, early.stop.round = 100, maximize = TRUE)
  preds2a <- predict(bst2a,xgtest)
  preds_a=rbind(preds_a,preds2a)
  
  bst2b <- xgb.train(params = param, data = xgtrain, nround=num_rounds, print.every.n = 100, watchlist=watchlist, early.stop.round = 100, maximize = FALSE)
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
