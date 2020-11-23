###########################
#
# Solution to Problem 8.2
#
###########################

library("sda") # R package for shrinkage discriminant analysis

# Singh et al. (2002) gene expression prostate cancer data
data(singh2002)

Xtrain = singh2002$x
Ytrain = singh2002$y

print(dim(Xtrain))
print(levels(Ytrain))

# predictor function for SDA
predfun.sda = function(Xtrain, Ytrain, Xtest, Ytest, diagonal=FALSE)
{
  sda.out = sda(Xtrain, Ytrain, diagonal=diagonal, verbose=FALSE)
  ynew = predict(sda.out, Xtest, verbose=FALSE)$class
  
  cm = confusionMatrix(Ytest, ynew, negative="healthy")
  
  return (cm)
}

# apply CV to to both LDA and DDA to estimate the confusion matrix and
# associated quantities such as accuracy, specificity, sensitivity, PPV, NPV etc.

library("crossval")

# DDA
cv.out = crossval(predfun.sda, Xtrain, Ytrain, K=5, B=50, diagonal=TRUE, verbose=FALSE)
print(cv.out$stat)
print(diagnosticErrors(cv.out$stat))

# LDA
cv.out = crossval(predfun.sda, Xtrain, Ytrain, K=5, B=50, diagonal=FALSE, verbose=FALSE)
print(cv.out$stat)
print(diagnosticErrors(cv.out$stat))

# select 100 genes and only use those in the predictor and estimate by cross validation
# the performance of the shrinkage LDA and DDA methods with 100 genes

best100 = sda.ranking(Xtrain, Ytrain, verbose=FALSE, diagonal=TRUE, fdr=FALSE)[1:100,"idx"]
print(best100)

# DDA
cv.out = crossval(predfun.sda, Xtrain[, best100], Ytrain, K=5, B=50, diagonal=TRUE, verbose=FALSE)
print(cv.out$stat)
print(diagnosticErrors(cv.out$stat))

# LDA
cv.out = crossval(predfun.sda, Xtrain[, best100], Ytrain, K=5, B=50, diagonal=FALSE, verbose=FALSE)
print(cv.out$stat)
print(diagnosticErrors(cv.out$stat))