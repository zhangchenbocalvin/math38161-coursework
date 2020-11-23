###########################
#
# Solution to Problem 7.2
#
###########################

EM = function(x, K, num_iter = 100)
{
  # number of samples
  n = length(x)
  
  # random initialization of z
  z = matrix(runif(K*n), n, K)
  # make rows add up to 1
  # take each row (1) of z and divide each element ("/") by the rowSum
  rowStd = function(z) sweep(z, 1, rowSums(z), "/")
  z = rowStd(z)
  
  # mean vector
  mu = numeric(K)
  # standard deviation vector
  s2 = numeric(K)
  
  # loop num_iter times
  for (r in 1:num_iter)
  {
    
    # M step: maximize the expected complete data log-likelihood
    n.group = colSums(z)  # vector of n_k_hat
    pi = n.group / n  # vector of pi_k_hat
    
    mu = colSums(x*z) / n.group
    s2 = colSums((replicate(K, x) - t(replicate(n, mu)))^2*z) / n.group
    
    # E step: compute the probabilities of allocation for each sample X_i
    for (i in 1:n)
    {
      z[i, ] = pi * dnorm(x[i], mean = mu, sd = sqrt(s2))
    }
    z = rowStd(z)
  }
  
  return (list(pi=pi, mu=mu, s2=s2, z=zapsmall(z)))
}

x = c(4.54, 1.57, 1.41, 1.77, 1.43, 0.07, 0.05, 4.19, -0.02, 1.32)
plot(x, rep(1, length(x)), ylab="", yaxt="n", main="The data points")

em.out = EM(x, 3)
print(em.out)

x = c(rnorm(20, mean=-10), rnorm(20, mean=10))
plot(x, rep(1, length(x)), ylab="", yaxt="n", main="The data points")

plot(density(x))

em.out = EM(x, 2, 5000)
print(em.out)