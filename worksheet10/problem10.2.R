############################
#
# Solution to Problem 10.2
#
############################

# Singh et al. (2002) gene expression prostate cancer data
library("sda")
data(singh2002)
Xtrain = singh2002$x
Ytrain = singh2002$y

# load randomForest package
library("randomForest")

# load crossval package
library("crossval")

# predictor function for crossval using a random forest
predfun.rf = function(train.x, train.y, test.x, test.y)
{
  rf.fit = randomForest(train.x, train.y, ntree=100)
  ynew = predict(rf.fit, test.x)
  
  # count false and true positives/negatives
  negative = levels(train.y)[2] # "healthy"
  cm = confusionMatrix(test.y, ynew, negative=negative)
  return(cm)
}

# perform cross validation
cv.out = crossval(predfun.rf, Xtrain, Ytrain, K=5, B=20, verbose=FALSE)
print(diagnosticErrors(cv.out$stat))