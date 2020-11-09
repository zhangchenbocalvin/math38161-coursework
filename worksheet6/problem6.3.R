###########################
#
# Solution to Problem 6.3
#
###########################

data(iris)

# preprocess
X.iris = scale((iris[, 1:4]), scale=TRUE) # center and standardise
L.iris = iris[, 5]

print(table(L.iris))

pairs(X.iris, col=as.integer(L.iris), pch=as.integer(L.iris)+14)

# perform k-means with k = 3
kmeans.out3 = kmeans(X.iris, centers = 3)
print(kmeans.out3)

# plot the clusters from k-means
plot(X.iris, col = kmeans.out3$cluster, pch = as.integer(L.iris) + 14, main = "K-Means")

# compare with original cluster
print(table(L.iris, kmeans.out3$cluster))

# apply k-means with number of classes varying from 1 to 10
between_var = numeric(10)
within_var = numeric(10)

for (k in 1:10)
{
  kmeans.out = kmeans(X.iris, centers = k)
  between_var[k] = kmeans.out$betweenss
  within_var[k] = kmeans.out$tot.withinss
}

# plot between and within variation as a function of k
plot(1:10, between_var, ylim = c(0, max(c(max(between_var, within_var)))),
     type = "b", xlab = "K", ylab = "Variation", main = "K-means Iris Data")
points(1:10, within_var, type = "b", col = 2, pch = 2)
legend("right", c("Between SS (explained)", "Within SS (unexplained)"), col=c(1,2), pch=c(1,2))