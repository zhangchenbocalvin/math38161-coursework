###########################
#
# Solution to Problem 3.2
#
###########################

# data matrix X
X = matrix(c(4, -1, 3, 1, 3, 5), ncol=2)

# column means
mu = colMeans(X)

# covariance matrix
Sigma = cov(X)

# we want E(Y) and Var(Y) where Y is the Mahalanobis transformation on X
# write Y = C_n * X * Sigma^(-1/2)

# compute Sigma^(-1/2) = U * lambda^(-1/2) * U_T
eigen = eigen(Sigma)
lambda = eigen$values
U = eigen$vectors

inv_sqrt_Sigma = U %*% diag(lambda^(-1/2)) %*% t(U)

# compute Y with the Mahalanobis transformation
# sweep() function used to centre the matrix X, 2 means that we are working column-wise
Y = sweep(X, 2, mu) %*% inv_sqrt_Sigma

# check that E(Y) = 0 matrix and Var(Y) = I
print(colMeans(Y))
print(cov(Y))