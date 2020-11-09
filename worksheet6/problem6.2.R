###########################
#
# Solution to Problem 6.2
#
###########################

# generate the "mouse" data
library("mnormt")
library("mclust")

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

# Apply GMM
gmm3 = Mclust(X.mouse, G=3)

# plot the clusters from GMM and check the results against the true labels
plot(X.mouse, col = gmm3$classification, pch = as.integer(L.mouse) + 14, main = "GMM")
print(table(L.mouse, gmm3$classification))

# apply GMM with G from 1 to 10 and plot the BIC
gmm = Mclust(X.mouse, G=1:10)
print(gmm$G) # the optimal number of G
plot(gmm, what="BIC")