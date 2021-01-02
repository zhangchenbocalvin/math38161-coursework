############################
#
# Solution to Problem 10.1
#
############################

# load data set
library("datasauRus")
print(dim(datasaurus_dozen)) # 1846 3
dsname = factor(unlist(datasaurus_dozen[,1]))
print(levels(dsname))

# plot data sets
library("ggplot2")
print(ggplot(datasaurus_dozen,
       aes(x = x, y = y, colour = dataset)) +
  geom_point() +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap( ~ dataset, ncol = 3))

# find statistics of the data
statistics = matrix(0, ncol=5, nrow=13)

for (i in 1:13)
{
  idx = (dsname == levels(dsname)[i])
  
  x = unlist(datasaurus_dozen[idx, 2])
  y = unlist(datasaurus_dozen[idx, 3])
  
  statistics[i, 1] = mean(x)
  statistics[i, 2] = mean(y)
  statistics[i, 3] = cor(x, y)
  statistics[i, 4:5] = coefficients(lm(y~x))
}

print(statistics)