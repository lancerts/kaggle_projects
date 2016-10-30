library(readr)

setwd("C:/Users/tshao/Dropbox/kaggle/Search Results Relevance")
train = read_csv("data/train.csv")
test  = read_csv("data/test.csv")


# First part: just to explore the data

# Get all the letters to lower case 
trainSet$query = sapply(trainSet$query,tolower)
trainSet$product_title = sapply(trainSet$product_title,tolower)
testSet$query = sapply(testSet$query,tolower)
testSet$product_title = sapply(testSet$product_title,tolower)

# Create a dataframe for each relevance level
notRelevant = trainSet[trainSet$median_relevance==1,]
someHowRelevant = trainSet[trainSet$median_relevance==2,]
prettyMuchRelevant = trainSet[trainSet$median_relevance==3,]
totallyRelevant = trainSet[trainSet$median_relevance==4,]

# Tokenize
unRelevantQueries = lapply(notRelevant$query, function(x){unlist(strsplit(x," "))})
unRelevantTitles  = lapply(notRelevant$product_title, function(x){unlist(strsplit(x," "))})

someHowRelevantQueries = lapply(someHowRelevant$query, function(x){unlist(strsplit(x," "))})
someHowRelevantTitles   = lapply(someHowRelevant$product_title, function(x){unlist(strsplit(x," "))})

prettyMuchRelevantQueries  = lapply(prettyMuchRelevant$query, function(x){unlist(strsplit(x," "))})
prettyMuchRelevantTitles   = lapply(prettyMuchRelevant$product_title, function(x){unlist(strsplit(x," "))})


totallyRelevantQueries = lapply(totallyRelevant$query, function(x){unlist(strsplit(x," "))})
totallyRelevantTitles  = lapply(totallyRelevant$product_title, function(x){unlist(strsplit(x," "))})


# Check the percentage of the words in query that appears in product_title
unRelevantSums = c()

for(i in 1:774){
  unRelevantSums = c(unRelevantSums,sum(unRelevantQueries[[i]] %in% unRelevantTitles[[i]])/length(unRelevantQueries[[i]]))
}

someHowRelevantSums = c()

for(i in 1:1476){
  someHowRelevantSums = c(someHowRelevantSums,sum(someHowRelevantQueries[[i]] %in% someHowRelevantTitles[[i]])/length(someHowRelevantQueries[[i]]))
}

prettyMuchRelevantSums = c()

for(i in 1:1737){
  prettyMuchRelevantSums = c(prettyMuchRelevantSums,sum(prettyMuchRelevantQueries[[i]] %in% prettyMuchRelevantTitles[[i]])/length(prettyMuchRelevantQueries[[i]]))
}


totallyRelevantSums = c()

for(i in 1:6171){
  totallyRelevantSums = c(totallyRelevantSums,sum(totallyRelevantQueries[[i]] %in% totallyRelevantTitles[[i]])/length(totallyRelevantQueries[[i]]))
}

# Analyzing the results will provide an intuition about the threshold for each category
un = as.data.frame(table(unRelevantSums))
un = rbind(un,c(1,0),c(1,0))
some = as.data.frame(table(someHowRelevantSums))
pretty = as.data.frame(table(prettyMuchRelevantSums))
totally = as.data.frame(table(totallyRelevantSums))
totally = rbind(totally,c(0,0))

# View(data.frame(un = un, some = some, pretty , totally))


# Second part: running the same algorithm with the test set

testSums = c()

testQueries = lapply(testSet$query, function(x){unlist(strsplit(x," "))})
testTitles  = lapply(testSet$product_title, function(x){unlist(strsplit(x," "))})

for(i in 1:22513){
  testSums = c(testSums,sum(testQueries[[i]] %in% testTitles[[i]])/length(testQueries[[i]]))
}

pred = vector(mode='integer',length=length(testSums))
pred[testSums < 0.1 ] = 1
pred[testSums < 0.4 & testSums >= 0.1 ] = 2
pred[testSums < 0.6 & testSums >= 0.4 ] = 3
pred[testSums >= 0.6 ] = 4

out = data.frame(id=testSet$id,prediction=pred)
write.csv(out,'output.csv',row.names=F)