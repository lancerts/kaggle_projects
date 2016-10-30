# -*- coding: utf-8 -*-

import sys
reload(sys)
sys.setdefaultencoding("utf-8")
import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import SVC
from sklearn.decomposition import TruncatedSVD
from sklearn.preprocessing import StandardScaler
from sklearn import decomposition, pipeline, metrics, grid_search
from nltk.stem.porter import *
import re
from bs4 import BeautifulSoup
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction import text
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.ensemble import AdaBoostClassifier
# array declarations
sw=[]
s_data = []
s_labels = []
t_data = []
t_labels = []
stemmer = PorterStemmer()
#stopwords tweak - more overhead
stop_words1 = ['http','www','img','border','color','style','padding','table','font','thi','inch','ha','width','height']
stop_words1 += list(text.ENGLISH_STOP_WORDS)
for stw in stop_words1:
    sw.append("q"+stw)
    sw.append("z"+stw)
stop_words1 += sw
for i in range(len(stop_words1)):
    stop_words1[i]=stemmer.stem(stop_words1[i])
    
# The following 3 functions have been taken from Ben Hamner's github repository
# https://github.com/benhamner/Metrics
def confusion_matrix(rater_a, rater_b, min_rating=None, max_rating=None):
    """
    Returns the confusion matrix between rater's ratings
    """
    assert(len(rater_a) == len(rater_b))
    if min_rating is None:
        min_rating = min(rater_a + rater_b)
    if max_rating is None:
        max_rating = max(rater_a + rater_b)
    num_ratings = int(max_rating - min_rating + 1)
    conf_mat = [[0 for i in range(num_ratings)]
                for j in range(num_ratings)]
    for a, b in zip(rater_a, rater_b):
        conf_mat[a - min_rating][b - min_rating] += 1
    return conf_mat


def histogram(ratings, min_rating=None, max_rating=None):
    """
    Returns the counts of each type of rating that a rater made
    """
    if min_rating is None:
        min_rating = min(ratings)
    if max_rating is None:
        max_rating = max(ratings)
    num_ratings = int(max_rating - min_rating + 1)
    hist_ratings = [0 for x in range(num_ratings)]
    for r in ratings:
        hist_ratings[r - min_rating] += 1
    return hist_ratings


def quadratic_weighted_kappa(y, y_pred):
    """
    Calculates the quadratic weighted kappa
    axquadratic_weighted_kappa calculates the quadratic weighted kappa
    value, which is a measure of inter-rater agreement between two raters
    that provide discrete numeric ratings.  Potential values range from -1
    (representing complete disagreement) to 1 (representing complete
    agreement).  A kappa value of 0 is expected if all agreement is due to
    chance.
    quadratic_weighted_kappa(rater_a, rater_b), where rater_a and rater_b
    each correspond to a list of integer ratings.  These lists must have the
    same length.
    The ratings should be integers, and it is assumed that they contain
    the complete range of possible ratings.
    quadratic_weighted_kappa(X, min_rating, max_rating), where min_rating
    is the minimum possible rating, and max_rating is the maximum possible
    rating
    """
    rater_a = y
    rater_b = y_pred
    min_rating=None
    max_rating=None
    rater_a = np.array(rater_a, dtype=int)
    rater_b = np.array(rater_b, dtype=int)
    assert(len(rater_a) == len(rater_b))
    if min_rating is None:
        min_rating = min(min(rater_a), min(rater_b))
    if max_rating is None:
        max_rating = max(max(rater_a), max(rater_b))
    conf_mat = confusion_matrix(rater_a, rater_b,
                                min_rating, max_rating)
    num_ratings = len(conf_mat)
    num_scored_items = float(len(rater_a))

    hist_rater_a = histogram(rater_a, min_rating, max_rating)
    hist_rater_b = histogram(rater_b, min_rating, max_rating)

    numerator = 0.0
    denominator = 0.0

    for i in range(num_ratings):
        for j in range(num_ratings):
            expected_count = (hist_rater_a[i] * hist_rater_b[j]
                              / num_scored_items)
            d = pow(i - j, 2.0) / pow(num_ratings - 1, 2.0)
            numerator += d * conf_mat[i][j] / num_scored_items
            denominator += d * expected_count / num_scored_items

    return (1.0 - numerator / denominator)


if __name__ == '__main__':
    import os
    os.chdir("C:/Users/tshao/Dropbox/kaggle/Search Results Relevance")
    #load data
    train = pd.read_csv("data/train.csv").fillna("")
    test  = pd.read_csv("data/test.csv").fillna("")
 
    train_org = train
    test_org = test
    
    # we dont need ID columns
    idx = test.id.values.astype(int)
    train = train.drop('id', axis=1)
    test = test.drop('id', axis=1)
    
    # create labels. drop useless columns
    y = train.median_relevance.values
    train = train.drop(['median_relevance', 'relevance_variance'], axis=1)
    
    # do some lambda magic on text columns
    traindata = list(train.apply(lambda x:'%s %s' % (x['query'],x['product_title']),axis=1))
    testdata = list(test.apply(lambda x:'%s %s' % (x['query'],x['product_title']),axis=1))
    
    # the infamous tfidf vectorizer (Do you remember this one?)
    tfv = TfidfVectorizer(min_df=3,  max_features=None, 
            strip_accents='unicode', analyzer='word',token_pattern=r'\w{1,}',
            ngram_range=(1, 2), use_idf=1,smooth_idf=1,sublinear_tf=1,
            stop_words = 'english')
    
    # Fit TFIDF
    tfv.fit(traindata)
    X =  tfv.transform(traindata) 
    X_test = tfv.transform(testdata)
    
    # Initialize SVD
    svd = TruncatedSVD()
    
    # Initialize the standard scaler 
    scl = StandardScaler()
    
    # We will use SVM here..
    svm_model = SVC()
    
    # Create the pipeline 
    clfsvm = pipeline.Pipeline([('svd', svd),
                             ('scl', scl),
                             ('svm', svm_model)])
    clfgb=pipeline.Pipeline([('svd', svd),
                             ('ada', AdaBoostClassifier())])
    # Create a parameter grid to search for best parameters for everything in the pipeline
    param_grid_svm = {'svd__n_components' : [300],
                  'svm__C': [7]}
    param_grid_gb = {'svd__n_components' : [300,350],
    'ada__n_estimators' : [400,450,500],
    'ada__learning_rate' : [0.6,0.7]}
    # Kappa Scorer 
    kappa_scorer = metrics.make_scorer(quadratic_weighted_kappa, greater_is_better = True)
    
    # Set random state
    np.random.seed(123)
    prng = np.random.RandomState()
    state = prng.get_state()
    
    # Initialize Grid Search Model
    modelsvm = grid_search.GridSearchCV(estimator = clfsvm, param_grid=param_grid_svm, scoring=kappa_scorer, verbose=10, n_jobs=4, iid=True, refit=True, cv=2)
    
    modelgb = grid_search.GridSearchCV(estimator = clfgb, param_grid=param_grid_gb, scoring=kappa_scorer, verbose=10, n_jobs=4, iid=True, refit=True, cv=2)
                                 
    # Fit Grid Search Model
                            
#    modelsvm.fit(X, y)
 #   print("Best score: %0.3f" % modelsvm.best_score_)
  #  print("Best parameters set:")
   # best_parameters = modelsvm.best_estimator_.get_params()
    #for param_name in sorted(param_grid_svm.keys()):
     #   print("\t%s: %r" % (param_name, best_parameters[param_name]))
        
    modelgb.fit(X, y)
    print("Best score: %0.3f" % modelgb.best_score_)
    print("Best parameters set:")
    best_parameters = modelgb.best_estimator_.get_params()
    for param_name in sorted(param_grid_gb.keys()):
        print("\t%s: %r" % (param_name, best_parameters[param_name]))
    
    # Get best model
    best_modelsvm = modelsvm.best_estimator_
    best_modelgb = modelgb.best_estimator_
    
    # Fit model with best parameters optimized for quadratic_weighted_kappa
    best_modelsvm.fit(X,y)
    preds_svm = best_modelsvm.predict(X_test)
    best_modelgb.fit(X,y)
    preds_gb = best_modelgb.predict(X_test)

    #load data
    train = train_org
    test = test_org
    
    #remove html, remove non text or numeric, stem, make query and title unique features for counts using prefix (accounted for in stopwords tweak)
    for i in range(len(train.id)):
        tx = BeautifulSoup(train.product_description[i])
        tx1 = [x.extract() for x in tx.findAll('script')]
        tx = tx.get_text(" ").strip()
        if "translation tool" in tx:
            tx = tx[-500:]
        s = (" ").join(["q"+ z for z in train["query"][i].split(" ")]) + " " + (" ").join(["z"+ z for z in train.product_title[i].split(" ")]) + " " + tx
        s = re.sub("[^a-zA-Z0-9]"," ", s)
        s = re.sub("[0-9]{1,3}px"," ", s)
        s = re.sub(" [0-9]{1,6} |000"," ", s)
        s = (" ").join([stemmer.stem(z) for z in s.split(" ") if len(z)>2])
        s = s.lower()
        w = list(s.split(" "))
        #ngrams
        ngr=[]
        for ng in range(len(w)-1):
            if not w[ng] in stop_words1 and not w[ng+1] in stop_words1:
                ngr.append(w[ng]+"_"+w[ng+1])
        w += ngr
        s = (" ").join(w)
        s_data.append(s)
        s_labels.append(str(train["median_relevance"][i]))
    for i in range(len(test.id)):
        t_labels.append(test["id"][i])
        tx = BeautifulSoup(test.product_description[i])
        tx1 = [x.extract() for x in tx.findAll('script')]
        tx = tx.get_text(" ").strip()
        tx = (" ").join([z for z in tx.split(" ")])
        if "translation tool" in tx:
            tx = tx[-500:]
        s = (" ").join(["q"+ z for z in test["query"][i].split(" ")]) + " " + (" ").join(["z"+ z for z in test.product_title[i].split(" ")]) + " " + tx
        s = re.sub("[^a-zA-Z0-9]"," ", s)
        s = re.sub("[0-9]{1,3}px"," ", s)
        s = re.sub(" [0-9]{1,6} |000"," ", s)
        s = (" ").join([stemmer.stem(z) for z in s.split(" ") if len(z)>2])
        s = s.lower()
        w = list(s.split(" "))
        #ngrams
        ngr=[]
        for ng in range(len(w)-1):
            if not w[ng] in stop_words1 and not w[ng+1] in stop_words1:
                ngr.append(w[ng]+"_"+w[ng+1])
        w += ngr
        s = (" ").join(w)
        t_data.append(s)
        

    #create sklearn pipeline, fit all, and predit test data
    clfsvm = Pipeline([('v',TfidfVectorizer(min_df=5, max_df=500, max_features=None, strip_accents='unicode', analyzer='word', token_pattern=r'\w{1,}', ngram_range=(1, 2), use_idf=True, smooth_idf=True, sublinear_tf=True, stop_words = 'english')), 
    ('svd', TruncatedSVD(n_components=300, algorithm='randomized', n_iter=5, random_state=None, tol=0.0)), 
    ('scl', StandardScaler(copy=True, with_mean=True, with_std=True)), 
    ('svm', SVC(C=7.0, kernel='rbf', degree=2, gamma=0.0, coef0=0.0, shrinking=True, probability=False, tol=0.001, cache_size=200, class_weight=None, verbose=False, max_iter=-1, random_state=None))])
    clfsvm.fit(s_data, s_labels)
    t_labels_svm = clfsvm.predict(t_data)
    
    import math
    p3 = []
    for i in range(len(preds)):
        x = (int(t_labels_svm[i]) + preds_svm[i])/2
        x = math.floor(x)
        p3.append(x)

    # Create your first submission file
    submission = pd.DataFrame({"id": idx, "prediction": p3})
    submission.to_csv("submission/beating_the_benchmark1.csv", index=False)
   