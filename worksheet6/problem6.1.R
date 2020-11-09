###########################
#
# Solution to Problem 6.1
#
###########################

# generate the "mouse" data
library("mnormt")

# class one (head)
n1 = 300
mu1 = c(0.5,0.5)
Sigma1 = matrix(c(0.15^2, 0, 0, 0.15^2), 2)
x1 = rmnorm(n1,mean=mu1,var=Sigma1)

# class two (left ear)
n2=100
mu2 = c(0.25,0.75)
Sigma2 = matrix(c(0.05^2, 0, 0, 0.05^2), 2)
x2 = rmnorm(n2,mean=mu2,var=Sigma2)

# class three (right ear)
n3 = 100
mu3 = c(0.75,0.75)
Sigma3 = matrix(c(0.05^2, 0, 0, 0.05^2), 2)
x3 = rmnorm(n3,mean=mu3,var=Sigma3)

# put all data together in one matrix
X.mouse = rbind(x1,x2,x3)
L.mouse = factor(c( rep("Head", n1), rep("Left Ear", n2), rep("Right Ear", n3) ))

plot(X.mouse, col=as.integer(L.mouse), pch=as.integer(L.mouse)+14, ylim=c(0,1), xlim=c(0,1))

# perform k-means with k = 3
kmeans.out3 = kmeans(X.mouse, centers = 3)
print(kmeans.out3)

# plot the clusters from k-means
plot(X.mouse, col = kmeans.out3$cluster, pch = as.integer(L.mouse) + 14, main = "K-Means")

# compare with original cluster
print(table(L.mouse, kmeans.out3$cluster))

# apply k-means with number of classes varying from 1 to 10
between_var = numeric(10)
within_var = numeric(10)

for (k in 1:10)
{
  kmeans.out = kmeans(X.mouse, centers = k)
  between_var[k] = kmeans.out$betweenss
  within_var[k] = kmeans.out$tot.withinss
}

# plot between and within variation as a function of k
plot(1:10, between_var, ylim=c(0, 34), type = "b", xlab = "K", ylab = "Variation", main = "K-means Mouse Data")
points(1:10, within_var, type = "b", col = 2, pch = 2)
legend("right", c("Between SS (explained)", "Within SS (unexplained)"), col=c(1,2), pch=c(1,2))