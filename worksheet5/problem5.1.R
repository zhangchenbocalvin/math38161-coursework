###########################
#
# Solution to Problem 5.1
#
###########################

library("mlbench")
data(Zoo)

X.zoo = Zoo[,1:16]
L.zoo = factor(Zoo[,17])

print(dim(X.zoo)) # 101 samples

# 16 features
print(colnames(X.zoo))

# seven zoological groups
print(levels(L.zoo))

print(table(L.zoo))

# distance matrix with Euclidead distance
dist_matrix = dist(X.zoo, method = "euclidean")

# hierarchical clustering using four methods: Ward.D2, average, complete, single
hclust.ward = hclust(dist_matrix, method = "ward.D2")
hclust.avg = hclust(dist_matrix, method = "average")
hclust.comp = hclust(dist_matrix, method = "complete")
hclust.sing = hclust(dist_matrix, method = "single")

# plot the resulting trees
plot(hclust.ward, cex=0.3) # cex=0.3 reduce font size of leaf labels
rect.hclust(hclust.ward, k = 7)

plot(hclust.avg, cex=0.3) # cex=0.3 reduce font size of leaf labels
rect.hclust(hclust.avg, k = 7)

plot(hclust.comp, cex=0.3) # cex=0.3 reduce font size of leaf labels
rect.hclust(hclust.comp, k = 7)

plot(hclust.sing, cex=0.3) # cex=0.3 reduce font size of leaf labels
rect.hclust(hclust.sing, k = 7)

# extract allocation of samples to the seven groups
groups.ward = cutree(hclust.ward, k = 7)
groups.avg = cutree(hclust.avg, k = 7)
groups.comp = cutree(hclust.comp, k = 7)
groups.sing = cutree(hclust.sing, k = 7)

print(table(L.zoo)) # the classes and the number of members in each class

print(table(L.zoo, groups.ward))
print(table(L.zoo, groups.avg))
print(table(L.zoo, groups.comp))
print(table(L.zoo, groups.sing))

# plot trees with coloured leaves

# define colors for the seven groups/levels in L.zoo
col.zoo = c("black", "red", "orange", "green", "cyan", "blue", "magenta")
pie(rep(1, 7), col = col.zoo, labels=levels(L.zoo))

# name -> color
colMap = col.zoo[as.integer(L.zoo)]
names(colMap) = rownames(X.zoo)

# this function converts hc objects into dendograms, colors the leafs and then
# plots the tree along with a legend
plotColoredTree = function(hc, colMap, cex)
{
  colorLeafs = function(x)
  {
    if (is.leaf(x))
    {
      lbl = attr(x, "label") # label at leaf
      attr(x, "nodePar") = c(list(lab.col=colMap[lbl], lab.cex=cex)) # set color and font size
    }
    return(x)
  }
  hcd = dendrapply(as.dendrogram(hc), colorLeafs)
  plot(hcd, main=paste(hc$method, "+", hc$dist.method))
  legend("topright", legend=levels(L.zoo), text.col=col.zoo, cex=cex)
}

# plot the trees above with coloured leaves
plotColoredTree(hclust.ward, colMap, cex=0.3)
rect.hclust(hclust.ward, k=7)

plotColoredTree(hclust.avg, colMap, cex=0.3)
rect.hclust(hclust.avg, k=7)

plotColoredTree(hclust.comp, colMap, cex=0.3)
rect.hclust(hclust.comp, k=7)

plotColoredTree(hclust.sing, colMap, cex=0.3)
rect.hclust(hclust.sing, k=7)

# plot use the "ape" library
library("ape")

plot(as.phylo(hclust.ward), tip.color = colMap, cex = 0.3,
     direction = "downward", main=paste(hclust.ward$method, "+", hclust.ward$dist.method))
rect.hclust(hclust.ward, k=7)
legend("topright", legend=levels(L.zoo), text.col=col.zoo, cex=0.3)

plot(as.phylo(hclust.avg), tip.color = colMap, cex = 0.3,
     direction = "downward", main=paste(hclust.avg$method, "+", hclust.avg$dist.method))
rect.hclust(hclust.avg, k=7)

plot(as.phylo(hclust.comp), tip.color = colMap, cex = 0.3,
     direction = "downward", main=paste(hclust.comp$method, "+", hclust.comp$dist.method))
rect.hclust(hclust.comp, k=7)

plot(as.phylo(hclust.sing), tip.color = colMap, cex = 0.3,
     direction = "downward", main=paste(hclust.sing$method, "+", hclust.sing$dist.method))
rect.hclust(hclust.sing, k=7)