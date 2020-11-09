###########################
#
# Solution to Problem 6.4
#
###########################

library("mclust")

data(iris)

# preprocess
X.iris = scale((iris[, 1:4]), scale=TRUE) # center and standardise
L.iris = iris[, 5]

print(table(L.iris))

pairs(X.iris, col=as.integer(L.iris), pch=as.integer(L.iris)+14)

# Apply GMM
gmm3 = Mclust(X.iris, G=3)

# plot the clusters from GMM and check the results against the true labels
plot(gmm3, what="classification")
print(table(L.iris, gmm3$classification))

# apply GMM with G from 1 to 10 and plot
gmm = Mclust(X.iris, G=1:10)
print(gmm$G) # the optimal number of G
plot(gmm, what="BIC")