###########################
#
# Solution to Problem 2.3
#
###########################

library("mnormt")
library("corpcor")

### True Model ###

# mean = 0
mu = numeric(100)
# covariance matrix is an identity matrix
Sigma = diag(100)

# compute eigenvalues
ev = eigen(Sigma)
evalues = ev$values

# simulate data sets of sizes n = 20, n = 50, n = 200
sim_data1 = rmnorm(n = 20, mu, Sigma)
sim_data2 = rmnorm(n = 50, mu, Sigma)
sim_data3 = rmnorm(n = 200, mu, Sigma)

### Model 1 (empirical covariance) ###

# compute the empirical covariance matrices
S1.emp = cov(sim_data1)
S2.emp = cov(sim_data2)
S3.emp = cov(sim_data3)

# compute the eigenvalues
evalues1 = eigen(S1.emp)$values
evalues2 = eigen(S2.emp)$values
evalues3 = eigen(S3.emp)$values

# check the ranks and conditions
print("***Data set 1***")
print(rank.condition(S1.emp))
print("***Data set 2***")
print(rank.condition(S2.emp))
print("***Data set 3***")
print(rank.condition(S3.emp))

# compute total squared errors
error1 = mean((evalues1 - evalues)^2)
error2 = mean((evalues2 - evalues)^2)
error3 = mean((evalues3 - evalues)^2)

### Model 2 (regularised estimator) ###

# compute the regularised estimates
S1.reg = cov.shrink(sim_data1, verbose = FALSE)
S2.reg = cov.shrink(sim_data2, verbose = FALSE)
S3.reg = cov.shrink(sim_data3, verbose = FALSE)

# compute the eigenvalues
reg_evalues1 = eigen(S1.reg)$values
reg_evalues2 = eigen(S2.reg)$values
reg_evalues3 = eigen(S3.reg)$values

# check the ranks and conditions
print("***Data set 1***")
print(rank.condition(S1.reg))
print("***Data set 2***")
print(rank.condition(S2.reg))
print("***Data set 3***")
print(rank.condition(S3.reg))

# compute total squared errors
reg_error1 = mean((reg_evalues1 - evalues)^2)
reg_error2 = mean((reg_evalues2 - evalues)^2)
reg_error3 = mean((reg_evalues3 - evalues)^2)