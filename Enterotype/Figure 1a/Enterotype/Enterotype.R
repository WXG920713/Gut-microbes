library(ade4)
library(cluster)
library(clusterSim)
library(readxl)
## 1. Bacteria-sample relative abundance table
data <- read.delim('genus.txt', row.names=1,header=T, sep = '\t',stringsAsFactors = F, check.names = F)

## write function
## JSD calculates sample distance, PAM clusters samples, CH index estimates cluster number, compares Silhouette coefficient to evaluate cluster quality.
dist.JSD <- function(inMatrix, pseudocount=0.000001, ...)
{
  ## Function: JSD calculates sample distance
  KLD <- function(x,y) sum(x *log(x/y))
  JSD<- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
  matrixColSize <- length(colnames(inMatrix))
  matrixRowSize <- length(rownames(inMatrix))
  colnames <- colnames(inMatrix)
  resultsMatrix <- matrix(0, matrixColSize, matrixColSize)
  
  inMatrix = apply(inMatrix,1:2,function(x) ifelse (x==0,pseudocount,x))
  
  for(i in 1:matrixColSize)
  {
    for(j in 1:matrixColSize)
    {
      resultsMatrix[i,j]=JSD(as.vector(inMatrix[,i]), as.vector(inMatrix[,j]))
    }
  }
  colnames -> colnames(resultsMatrix) -> rownames(resultsMatrix)
  as.dist(resultsMatrix)->resultsMatrix
  attr(resultsMatrix, "method") <- "dist"
  return(resultsMatrix)
}

pam.clustering = function(x, k)
{
  
  ## Function: PAM clustering samples
  # x is a distance matrix and k the number of clusters
  require(cluster)
  cluster = as.vector(pam(as.dist(x), k, diss=TRUE)$clustering)
  return(cluster)
}

## 2. Select the K value with the largest CH index as the optimal number of clusters
data.dist = dist.JSD(data)

data.cluster=pam.clustering(data.dist, k=3) # k=3 as an example
nclusters = index.G1(t(data), data.cluster, d = data.dist, centrotypes = "medoids") # View CH index
nclusters = NULL

for(k in 1:20)
{
  if(k==1)
  {
    nclusters[k] = NA
  }
  else
  {
    data.cluster_temp = pam.clustering(data.dist, k)
    nclusters[k] = index.G1(t(data), data.cluster_temp, d = data.dist, centrotypes = "medoids")
  }
}

plot(nclusters, type="h", xlab="k clusters", ylab="CH index", main="Optimal number of clusters") # Check the value relationship between K and CH


#It is best when the setting is divided into several categories
nclusters[1] = 0
k_best = which(nclusters == max(nclusters), arr.ind = TRUE)

## 3. PAM clusters samples according to JSD distance (divided into K groups)
data.cluster = pam.clustering(data.dist, k = k_best)

## 4. Silhouette evaluates the clustering quality, -1=<S(i)=<1, the closer to 1, the better
mean(silhouette(data.cluster, data.dist)[, k_best])

## plot 1
obs.pca=dudi.pca(data.frame(t(data)), scannf=F, nf=10)
obs.bet=bca(obs.pca, fac=as.factor(data.cluster), scannf=F, nf=4)


s.class(obs.bet$ls, fac=as.factor(data.cluster), grid=F, col=c('4','2','#bdb704'),xlim = c(-6, 6), ylim = c(-8,6))

pca <- as.data.frame(obs.bet$ls)

pca$group <- data.cluster

write.table(pca, 'pca.txt', row.names = TRUE, sep = '\t', quote = FALSE, na = '')