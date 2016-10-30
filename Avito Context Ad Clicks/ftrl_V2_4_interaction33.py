from sklearn.utils import murmurhash3_32 as mhash
#from numbapro import autojit
from datetime import datetime
from csv import DictReader
from math import exp, log, sqrt


# TL; DR, the main training process starts on line: 250,
# you may want to start reading the code from there


##############################################################################
# parameters #################################################################
##############################################################################

# A, paths
train = 'C:/Users/tshao/data_Avito Context Ad Clicks/train.csv'             # path to training file
test = 'C:/Users/tshao/data_Avito Context Ad Clicks/test.csv'                 # path to testing file
submission = 'submission%s.csv'  # path of to be outputted submission file

# B, model
alpha = .1  # learning rate
beta = 1.   # smoothing parameter for adaptive learning rate
L1 = 1.     # L1 regularization, larger value means more regularized
L2 = 1.     # L2 regularization, larger value means more regularized

# C, feature/hash trick
D = 2 ** 26             # number of weights to use
interaction = True     # whether to enable poly2 feature interactions
interaction3=True

# D, training/validation
epoch = 20       # learn training data for N passes
holdafter = 11   # data after date N (exclusive) are used as validation
holdout = None  # use every N training instance for holdout validation


##############################################################################
# class, function, generator definitions #####################################
##############################################################################
def indices(x,D,interaction,interaction3):
    # first yield index of the bias term
    
    yield 0

    # then yield the normal indices

    for index in x:
        yield index
    L = len(x)         
    x = sorted(x)
        # now yield interactions (if applicable)
    if interaction:
        for i in xrange(L):
            for j in xrange(i+1, L):                
                yield abs(mhash(str(x[i]) + '_' + str(x[j]))) % D
                    
    if interaction3:
        for i in xrange(L):
            for j in xrange(i+1, L):
                for k in xrange(j+1,L):                
                    yield abs(mhash(str(x[i]) + '_' + str(x[j])+'_'+str(x[k]))) % D                              
            

                
class ftrl_proximal(object):
    ''' Our main algorithm: Follow the regularized leader - proximal

        In short,
        this is an adaptive-learning-rate sparse logistic-regression with
        efficient L1-L2-regularization

        Reference:
        http://www.eecs.tufts.edu/~dsculley/papers/ad-click-prediction.pdf
    '''

    def __init__(self, alpha, beta, L1, L2, D, interaction,interaction3):
        # parameters
        self.alpha = alpha
        self.beta = beta
        self.L1 = L1
        self.L2 = L2

        # feature related parameters
        self.D = D
        self.interaction = interaction
        self.interaction3 = interaction3

        # model
        # n: squared sum of past gradients
        # z: weights
        # w: lazy weights
        self.n = [0.] * D
        self.z = [0.] * D
        self.w = {}

                
     
            
                   
    def predict(self, x):
        ''' Get probability estimation on x

            INPUT:
                x: features

            OUTPUT:
                probability of p(y = 1 | x; w)
        '''

        # parameters
        alpha = self.alpha
        beta = self.beta
        L1 = self.L1
        L2 = self.L2

        # model
        n = self.n
        z = self.z
        w = {}

        # wTx is the inner product of w and x
        wTx = 0.
        for i in indices(x,self.D,self.interaction,self.interaction3):
            sign = -1. if z[i] < 0 else 1.  # get sign of z[i]

            # build w on the fly using z and n, hence the name - lazy weights
            # we are doing this at prediction instead of update time is because
            # this allows us for not storing the complete w
            if sign * z[i] <= L1:
                # w[i] vanishes due to L1 regularization
                w[i] = 0.
            else:
                # apply prediction time L1, L2 regularization to z and get w
                w[i] = (sign * L1 - z[i]) / ((beta + sqrt(n[i])) / alpha + L2)

            wTx += w[i]

        # cache the current w for update stage
        self.w = w

        # bounded sigmoid function, this is the probability estimation
        return 1. / (1. + exp(-max(min(wTx, 35.), -35.)))
 
    def update(self, x, p, y):
        ''' Update model using x, p, y

            INPUT:
                x: feature, a list of indices
                p: click probability prediction of our model
                y: answer

            MODIFIES:
                self.n: increase by squared gradient
                self.z: weights
        '''

        # parameter
        alpha = self.alpha

        # model
        n = self.n
        z = self.z
        w = self.w

        # gradient under logloss
        g = p - y

        # update z and n
        for i in indices(x,self.D,self.interaction,self.interaction3):
            sigma = (sqrt(n[i] + g * g) - sqrt(n[i])) / alpha
            z[i] += g - sigma * w[i]
            n[i] += g * g


def logloss(p, y):
    ''' FUNCTION: Bounded logloss

        INPUT:
            p: our prediction
            y: real answer

        OUTPUT:
            logarithmic loss of p given y
    '''

    p = max(min(p, 1. - 10e-15), 10e-15)
    return -log(p) if y == 1. else -log(1. - p)


def data(path, D):
    ''' GENERATOR: Apply hash-trick to the original csv line
                   and for simplicity, we one-hot-encode everything

        INPUT:
            path: path to training or testing file
            D: the max index that we can hash to

        YIELDS:
            x: a list of hashed and one-hot-encoded 'indices'
               we only need the index since all values are either 0 or 1
            y: y = 1 if we have a click, else we have y = 0
    '''

    for t, line in enumerate(DictReader(open(path))):

        y = 0.
        if 'IsClick' in line:
            if line['IsClick'] == '1':
                y = 1.
            del line['IsClick']
        try:
            del line['SearchID']
        except:
            pass
            
        # extract date
#        Year = line['SearchDate'].split()[0].split('-')[0]
        month = line['SearchDate'].split()[0].split('-')[1]        
        date = line['SearchDate'].split()[0].split('-')[2]
#        hour = line['SearchDate'].split()[1].split(':')[0]
        # turn them into dict
#        line['Year'] = Year
#        line['month'] = month
#        line['date'] = date
#        line['hour'] = hour
        

        del line['HistCTR']        
        del line['ObjectType']        
        del line['Title']
        del line['Params']
        del line['SubcategoryID']
        del line['ParentCategoryID']
        del line['Price']        
        del line['IsContext']
        del line['RegionID']
        del line['CityID']       
        del line['CLevel']  
        del line['SearchDate']
        del line['UserAgentID']
        del line['UserAgentFamilyID']
        del line['UserAgentOSID']        
        del line['UserDeviceID']     
        del line['LLevel'] 
        
#possible useful features        
#        del line['SearchQuery']
#        del line['SearchParams']                             
#        del line['SearchCategoryID']
#        del line['IsUserLoggedOn']
#        del line['IPID']        
#        del line['UserID']
#        del line['SearchLocationID']     
        
        # build x
        x = []
        for key in line:
            value = line[key]

            # one-hot encode everything with hash trick
            index = abs(mhash(key + '_' + value)) % D
            x.append(index) 
#        x.append(abs(mhash(line['UserID']+'_'+line['IsUserLoggedOn'])) % D)
#        x.append(abs(mhash(line['AdID']+'_'+line['Position'])) % D)               
        date=int(date)
        month=int(month)
        try:
            ID = line['TestID']
            yield t,ID,date,month, x, y
        except:
            yield t,date,month,x, y



##############################################################################
# start training #############################################################
##############################################################################

start = datetime.now()

# initialize ourselves a learner
learner = ftrl_proximal(alpha, beta, L1, L2, D, interaction,interaction3)

# start training
for e in xrange(epoch):
    loss = 0.
    count = 0

    for t,date,month,x, y in data(train, D):  # data is a generator
        #    t: just a instance counter
        # date: you know what this is
        #    x: features
        #    y: label (Isclick)

        # step 1, get prediction from learner
        p = learner.predict(x)
        condition= (date>holdafter and month ==5) or month>5
        if (holdafter and condition) or (holdout and t % holdout == 0):
            # step 2-1, calculate validation loss
            #           we do not train with the validation data so that our
            #           validation loss is an accurate estimation
            #
            # holdafter: train instances from day 1 to day N
            #            validate with instances from day N + 1 and after
            #
            # holdout: validate with every N instance, train with others
            loss += logloss(p, y)
            count += 1
            learner.update(x, p, y)
        else:
            # step 2-2, update learner with label (click) information
            learner.update(x, p, y)


      
        if t%5000000==0 and t>0:
           print(' %d finished, validation logloss: %f, count: %d, elapsed time: %s' % (
        t, loss/(count-1), count-1, str(datetime.now() - start)))
        
    print('Epoch %d finished, validation logloss: %f, elapsed time: %s' % (
        e, loss/(count-1), str(datetime.now() - start)))

    with open(submission% (e+1), 'w') as outfile:
         outfile.write('ID,IsClick\n')
         for t, ID,date, month, x, y in data(test, D):
             p = learner.predict(x)
             outfile.write('%s,%s\n' % (ID, str(p)))





        
