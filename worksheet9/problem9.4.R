###########################
#
# Solution to Problem 9.4
#
###########################

# load data set
library("MASS")
data(painters)

X.painters = painters[, 1:4]
print(dim(X.painters))

print(colnames(X.painters))

L.painters = as.factor(painters[, 5]) # school
print(levels(L.painters))

print(table(L.painters))

# find the conditional independence graph
Sigma = cor(X.painters)
print(Sigma)

Omega = solve(Sigma)
Rho = -cov2cor(Omega)
diag(Rho) = - diag(Rho)
print(Rho)

# cross-validation and LDA
predfun.lda = function(train.x, train.y, test.x, test.y)
{
  lda.fit = lda(train.x, train.y)
  ynew = predict(lda.fit, test.x)$class
  acc = mean(ynew == test.y)
  return(acc)
}

library("crossval")

cv.out = crossval(predfun.lda, X.painters, L.painters, K=5, B=50, verbose=FALSE)
print(c(cv.out$stat, cv.out$stat.se))

# accuracy on "Venetian school"
L.venetian = factor(ifelse(L.painters=="D", "Venetian", "other"))
cv.out = crossval(predfun.lda, X.painters, L.venetian, K=5, B=50, verbose=FALSE)
print(c(cv.out$stat, cv.out$stat.se))