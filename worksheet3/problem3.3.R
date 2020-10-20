###########################
#
# Solution to Problem 3.3
#
###########################

# function to generate n samples from a multivariate normal distribution with
# mean mu and covariance matrix Sigma
myrmnorm = function(n, mu, Sigma) {
  
  d = length(mu)
  
  # generate data using a standard univariate normal random number generator
  # in total we will need n * d values from the univeriate normal in order to
  # get n d-dimensional vectors in the multivariate space
  univ_data = rnorm(n * d)
  # place into a matrix so that it is a a sample from a standard multivariate sample
  X = matrix(univ_data, nrow = n)
  
  # use Mahalanobis coloring transformation to set mean and covariance
  # Y = mu + Sigma^(1/2) * X
  # eigenvalues decomposition of Sigma
  eigen = eigen(Sigma)
  lambda = eigen$values
  U = eigen$vectors
  # sqrt of Simga
  sqrt_Sigma = U %*% diag(lambda^(1/2)) %*% t(U)
  
  # make matrix from mu
  MU = t(replicate(n, mu))
  # apply the coloring transformation
  Y = MU + X %*% sqrt_Sigma
  
  return(Y)
}

# test the fucntion
mu = c(1, 2, 3, 4, 5, 6, 7, 8)
Sigma = matrix(0.8, nrow = 8, ncol = 8)
diag(Sigma) = c(8, 7, 6, 5, 4, 3, 2, 1)

gen_data = myrmnorm(100000, mu, Sigma)

print(colMeans(gen_data))
print(cov(gen_data))