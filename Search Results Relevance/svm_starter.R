

library(readr)
library(Metrics)
library(tm)
library(SnowballC)
library(e1071)
library(Matrix)
library(SparseM)
library(caTools)

setwd("C:/Users/tshao/Dropbox/kaggle/Search Results Relevance")
train = read_csv("data/train.csv")
test  = read_csv("data/test.csv")


#--------- remove html syntax in product description 
rm_garbage = function(string){
  garbage = c("<.*?>", "http", "www","img","border","style","px","margin","left", "right","font","solid","This translation tool is for your convenience only.*?Note: The accuracy and accessibility of the resulting translation is not guaranteed")
  for (i in 1:length(garbage)){
    string = gsub(garbage[i], "", string)
  }
  return (string)
}
train$product_description = lapply(train$product_description,rm_garbage)
test$product_description = lapply(test$product_description,rm_garbage)


#-----------restore dependent variables
test_id = test$id
train_relevance = as.factor(train$median_relevance)
train_variance = train$relevance_variance
train$median_relevance = NULL
train$relevance_variance = NULL


#------------compute number words in independent variables 
n_words = function(string){return(sapply(gregexpr("[[:alpha:]]+", string), function(x) sum(x > 0)))}

train_pt_words = apply(train[3],1,n_words)
test_pt_words = apply(test[3],1,n_words) 
 train_q_words = apply(train["query"],1,n_words)
 test_q_words = apply(test["query"],1,n_words)
 train_pd_words = apply(train[4],1,n_words)
 test_pd_words = apply(test[4],1,n_words)


#------------ compute number of matching words between independent variables 
 matching_words = function(terms){
    term1 = terms[1]
   term2 = terms[2]
    corpus = Corpus(VectorSource(list(term1, term2))) 
    dtm = DocumentTermMatrix(corpus,control=list(tolower=TRUE,removePunctuation=TRUE,removeNumbers=TRUE,stopwords=TRUE,stemming=TRUE))
    tf_matrix = as.matrix(dtm)  
    matching = sum(tf_matrix[1,]>0 & tf_matrix[2,]>0) 
    return(matching)
 }

#--query & product_title
 train_q_pt_match = apply(train[,c(2,3)],1,matching_words) 
 test_q_pt_match = apply(test[,c(2,3)],1,matching_words) 

#--product_description & product_title
 train_pt_pd_match = apply(train[,c(3,4)],1,matching_words) 
 test_pt_pd_match = apply(test[,c(3,4)],1,matching_words) 
 train_pt_pd_pred = ifelse(train_pt_pd_match==0,1,ifelse(train_pt_pd_match>=train_pt_words,4,ifelse(train_pt_pd_match==1,2,3)))
 test_pt_pd_pred = ifelse(test_pt_pd_match==0,1,ifelse(test_pt_pd_match>=test_pt_words,4,ifelse(test_pt_pd_match==1,2,3)))

#-- combine the sets' queries, p_d(s), p_t(s)
pt_words = c(train_pt_words,test_pt_words)
 q_words = c(train_q_words,test_q_words)
 pd_words = c(train_pd_words,test_pd_words)
 q_pt_match = c(train_q_pt_match,test_q_pt_match)
 pt_pd_pred = c(train_pt_pd_pred,test_pt_pd_pred)


#------------ bag of words 
DTM = function(corpus){
  dtm = DocumentTermMatrix(corpus,control=list(tolower=TRUE,removePunctuation=TRUE,removeNumbers=TRUE,stopwords=TRUE,
                                               stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))
  return (dtm)
}

combi = rbind(train,test)
all_text = Corpus(VectorSource(combi$query))
dtm = DTM(all_text)
df_q = as.data.frame(as.matrix(dtm))
colnames(df_q) = paste("q_",colnames(df_q),sep="")

 all_text = Corpus(VectorSource(combi$product_title))
 dtm = DTM(all_text)
 dtm = removeSparseTerms(dtm,0.999)    
 df_pt = as.data.frame(as.matrix(dtm))
 colnames(df_pt) = paste("pt_",colnames(df_pt),sep="")

 all_text = Corpus(VectorSource(combi$product_description))
 dtm = DTM(all_text)
 dtm = removeSparseTerms(dtm,0.999)   
 df_pd = as.data.frame(as.matrix(dtm))
 colnames(df_pd) = paste("pd_",colnames(df_pd),sep="")

# Combine all columns into a single dataframe
 combi = cbind(df_q,df_pt,df_pd,q_pt_match,pt_words,pt_pd_pred,pd_words,q_words) 

#-------------------------------- svm to predict relevance 
# Use naiveBayes to test various of ideas.. svm takes too long to run  
new_train = combi[1:10158,]
new_test = combi[10159:32671,]

#---separated the train set into train and test sets to play around with the "cost" variable in the svm. 
#---did fixed set cross validation to tune the model according to the in sample ScoreQuadraticWeightedKappa. 

 set.seed(1)
 spl= sample.split(train_relevance, SplitRatio = 0.7)
 train_train = subset(new_train, spl==TRUE)
 train_test = subset(new_train, spl==FALSE)
 train_train_relevance = subset(train_relevance,spl==TRUE)
 train_test_relevance = subset(train_relevance,spl==FALSE)

 sparse_train_train = Matrix(as.matrix(train_train),sparse=T)
 sparse_train_test = Matrix(as.matrix(train_test),sparse=T)
sparse_train = Matrix(as.matrix(new_train),sparse=T)
sparse_test = Matrix(as.matrix(new_test),sparse=T)

 model = svm(sparse_train_train,train_train_relevance, kernel="sigmoid", cost=0.3)
 train_pred = predict(model, sparse_train_test)
 ScoreQuadraticWeightedKappa(train_pred, train_test_relevance,1,4)  

model = svm(sparse_train,train_relevance, kernel="linear", cost=0.3)
pred = predict(model,sparse_test)
Newsubmission = data.frame(id=test_id, prediction = pred)
write.csv(Newsubmission,"model.csv",row.names=F)      #[1] 0.60718    