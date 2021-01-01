###########################
#
# Solution to Problem 9.5
#
###########################

# load the data set
library("whitening")
data(forina1986)
wine.attrib = forina1986$attrib
wine.type = forina1986$type
print(dim(wine.attrib))
print(levels(wine.type))
print(table(wine.type))

# function to compute the feature ranking
# diagonal = TRUE: use t-scores for ranking
# diagonal = FALSE: ZCA-cor whiten the data, then use t-scores
library("sda")
featureRanking = function(train.x, train.y, diagonal=TRUE)
{
  return( sda.ranking(train.x, train.y, diagonal=diagonal,
                      verbose=FALSE, fdr=FALSE, lambda=0, lambda.var=0) )
}

library("crossval")
# predictor function for LDA (using sda)
predfun.lda = function(train.x, train.y, test.x, test.y)
{
  # fit sda with zero shrinkage and full covariance (=classic LDA)
  sda.fit = sda(train.x, train.y, diagonal=FALSE, lambda=0, lambda.var=0, verbose=FALSE)
  ynew = predict(sda.fit, test.x, verbose=FALSE)$class
  # compute accuracy
  out = mean( ynew == test.y)
}

# compute the ranking of the predictors based on the t-scores
ranking = featureRanking(wine.attrib, wine.type, diagonal=TRUE)
ordering = ranking[, "idx"]

print(ranking)
plot(ranking)
print(ordering)

ranking.decor = featureRanking(wine.attrib, wine.type, diagonal=FALSE)
ordering.decor = ranking.decor[, "idx"]

print(ranking.decor)
plot(ranking.decor)
print(ordering.decor)

# compute the prediction accuracy using all subsets
cvmat = matrix(0, 26, 2)
cvmat.decor = matrix(0, 26, 2)

for (i in 2:27)
{
  cv.out = crossval(predfun.lda, wine.attrib[, ordering[1:i]], wine.type,
                    K=5, B=50, verbose=FALSE)
  cvmat[i-1,] = c(cv.out$stat, cv.out$stat.se)
  
  cv.out = crossval(predfun.lda, wine.attrib[, ordering.decor[1:i]], wine.type,
                    K=5, B=50, verbose=FALSE)
  cvmat.decor[i-1,] = c(cv.out$stat, cv.out$stat.se)
}

plot(2:27, cvmat[, 1], type="l", xlab="Number of predictors",
     ylab="Accuracy", ylim=c(0.85, 1), main="CV estimates")
lines(2:27, cvmat.decor[, 1], col=2)
legend("bottomright", c("t-scores", "decorrelated"), col=1:2, lty=1)

# classifier with 5 and 16 predictors using decorrelated t-score ranking
cv.out = crossval(predfun.lda, wine.attrib[, ordering.decor[1:5]], wine.type,
                  K=5, B=50, verbose=FALSE)
print(c(cv.out$stat, cv.out$stat.se))

cv.out = crossval(predfun.lda, wine.attrib[, ordering.decor[1:16]], wine.type,
                  K=5, B=50, verbose=FALSE)
print(c(cv.out$stat, cv.out$stat.se))