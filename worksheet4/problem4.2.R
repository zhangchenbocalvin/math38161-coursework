###########################
#
# Solution to Problem 4.2
#
###########################

library("mnormt")
library("whitening")

# generate bivariate normal data set with sample n = 500
mu = c(0, 0)
Sigma = matrix(c(2, 2, 2, 4), nrow = 2, ncol = 2)
X = rmnorm(500, mu, Sigma)

# apply whitening transformations
Z.ZCA = whiten(X, method = "ZCA")
Z.ZCA.cor = whiten(X, method = "ZCA-cor")
Z.PCA = whiten(X, method = "PCA")
Z.PCA.cor = whiten(X, method = "PCA-cor")
Z.Chol = whiten(X, method = "Cholesky")

# verify quantitatively that the transformed variables are uncorrelated
print(cov(Z.ZCA))
print(cov(Z.ZCA.cor))
print(cov(Z.PCA))
print(cov(Z.PCA.cor))
print(cov(Z.Chol))

# verify graphically that the transformed variables are uncorrelated
par(mfrow = c(2, 3))
plot(X)
plot(Z.ZCA)
plot(Z.ZCA.cor)
plot(Z.PCA)
plot(Z.PCA.cor)
plot(Z.Chol)

# inspect pairwise scatterplots between corresponding components of original and transformed data
par(mfrow = c(2, 5))
plot(X[, 1], Z.ZCA[, 1])
plot(X[, 2], Z.ZCA[, 2])
plot(X[, 1], Z.ZCA.cor[, 1])
plot(X[, 2], Z.ZCA.cor[, 2])
plot(X[, 1], Z.PCA[, 1])
plot(X[, 2], Z.PCA[, 2])
plot(X[, 1], Z.PCA.cor[, 1])
plot(X[, 2], Z.PCA.cor[, 2])
plot(X[, 1], Z.Chol[, 1])
plot(X[, 2], Z.Chol[, 2])

# cross-correlation matrices
Psi.ZCA = cor(Z.ZCA, X)
Psi.ZCA.cor = cor(Z.ZCA.cor, X)
Psi.PCA = cor(Z.PCA, X)
Psi.PCA.cor = cor(Z.PCA.cor, X)
Psi.Chol = cor(Z.Chol, X)

# plot cross-correlation matrices
par(mfrow = c(2, 3))
image(t(Psi.ZCA), axes = FALSE, main = "ZCA")
image(t(Psi.ZCA.cor), axes = FALSE, main = "ZCA.cor")
image(t(Psi.PCA), axes = FALSE, main = "PCA")
image(t(Psi.PCA.cor), axes = FALSE, main = "PCA.cor")
image(t(Psi.Chol), axes = FALSE, main = "Chol")

# demonstrate that ZCA.cor whitening produces the optimal average component-wise cross-correlations
print(sum(diag(Psi.ZCA)))
print(sum(diag(Psi.ZCA.cor)))
print(sum(diag(Psi.PCA)))
print(sum(diag(Psi.PCA.cor)))
print(sum(diag(Psi.Chol)))

# demonstrate that PCA.cor whitening produces the optimal compression in terms of squared cross-correlations
print(rowSums(Psi.ZCA^2))
print(rowSums(Psi.ZCA.cor^2))
print(rowSums(Psi.PCA^2))
print(rowSums(Psi.PCA.cor^2))
print(rowSums(Psi.Chol^2))