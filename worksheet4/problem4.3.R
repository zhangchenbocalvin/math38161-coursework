###########################
#
# Solution to Problem 4.3
#
###########################

library("whitening")

# load iris data set
data(iris)
# centre and stadardise data by considering the first 4 columns as the 5th is categorical
X = scale(iris[, 1:4], center = TRUE, scale = TRUE)
# save the 5th column as a categorical variable for the species
species = iris[, 5]

# conduct PCA
pca = prcomp(X)
print(pca)

# verify that the PCs are uncorrelated but not standardised
print(zapsmall(var(pca$x)))

# get the (cumulative) proportions of variation for each PC
print(summary(pca))
# display a graphical visualization
plot(pca, type = "l")
# we can see that the first two PCs capture 95% of the variance

# plot PC1 vs PC2 using the species information as coloring information
plot(pca$x[, 1], pca$x[, 2], col = species, xlab="PCA1", ylab="PCA2")
legend("topright", levels(species), col=1:3, pch=1)

# compute the PCs without the prcomp() function
eigen = eigen(cov(X))
U = eigen$vectors
lambda = eigen$values
X.PCA = X %*% U

print(zapsmall(var(X.PCA))) # PCs
print(lambda) # variance

# compute the proportions of variation
print(lambda / sum(lambda))
# compute the cumulative proportions of variation
print(cumsum(lambda) / sum(lambda))

# compare with PCA whitening
Z = whiten(X, method="PCA")
print(zapsmall(var(Z)))

plot(Z[,1], Z[,2], xlab="Z1", ylab="Z2", main="PCA Whitened Components",
     col=as.integer(species), pch=as.integer(species)+14)
legend("topright", levels(species), col=1:3, pch=(1:3)+14 )

imp = rowSums(cor(Z, X)^2)
print(imp/ sum(imp))