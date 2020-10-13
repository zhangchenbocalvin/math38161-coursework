###########################
#
# Solution to Problem 2.2
#
###########################

library(mnormt) # R package for multivariate normal distribution

# computes the MVN density values
compute_density = function(x, mu, Sigma) {
  
  len = length(x)
  f = matrix(0, nrow = len, ncol = len)
  
  for (i in 1:len) {
    for (j in 1:len) {
      f[i, j] = dmnorm(c(x[i], x[j]), mean = mu, var = Sigma)
    }
  }
  
  return(f)
}

# specified mean vector and covariance matrix
mu = c(0,0)
Sigma = matrix(c(1, .5, .5, 1), 2)

# values for which to compute the density
x = seq(-5, 5, length.out = 100)

# density function
f = compute_density(x, mu, Sigma)

# plot the density
persp(f, col = "gray",
      theta = 30, phi = 30,
      xlab = "x1", ylab = "x2", zlab = "f",
      main = "Density of MVN (d = 2)")

# plot contour
contour(x, x, f,
        xlab = "x1", ylab = "x2",
        main = "Countour plot of MVN (d = 2)")

# compute the eigenvalues
ev = eigen(Sigma)
evalues = ev$values

# compare radii of the contours using the sqrt of the ratio of the eigenvalues
ratio = sqrt(evalues[1]/evalues[2])

# simulate sample from the MVN with n = 1000
simulated_data = rmnorm(1000, mu, Sigma)

# draw the scatter plot
plot(simulated_data, xlim=range(x), ylim=range(x),
     main="Simulated Data from MVN (d = 2)", xlab="x1", ylab="x2")

