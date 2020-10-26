###########################
#
# Solution to Problem 4.1
#
###########################

library("whitening")

# load iris data set
data("iris")
# centre and stadardise data by considering the first 4 columns as the 5th is not numeric
X = scale(iris[, 1:4], center = TRUE, scale = TRUE)

# whiten data with PCA
Z_PCA = whiten(X, method = "PCA")
# compute the loadings Psi
Psi = cor(Z_PCA, X)

# prepare plot
plot(0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", asp = 1,
     xlab = "z1", ylab = "z2", main = "Correlation Loading Iris Plot")

# add unit circle
curve(sqrt(1 - x^2), from = -1, to = 1, add = TRUE)
curve(-sqrt(1 - x^2), from = -1, to = 1, add = TRUE)

# add horizontal and vertical lines
abline(h = 0)
abline(v = 0)

# plot the variables
points(Psi[1, ], Psi[2, ], pch = 20, col = "red")
text(Psi[1, ], Psi[2, ], labels = names(Psi[1, ]))