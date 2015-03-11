##Generates toy dataset to play around with clustering 

##Function to create toy 3 dimensional dataset
make.toy.data <- function(means, stdevs, N) {
  ##Returns a dataframe containing a 3D toy dataset 
  ##
  ##Arguements: 
  ##
  ##  means  <- a dataframe of 3 columns giving the means for each dimenstion of each cluster. Each row is a cluster.
  ##  stdevs <- a dataframe of 3 columns giving the standard deviations for each dimenstion of each cluster. Each row is a cluster.
  ##  N <- a value specifying the number of datapoints for each cluster
  ##  dist  <- a vector specifying the type of distribution you would like each cluster data to be
  
  ##get how many clusters are wanted
  cluster.n <- dim(means)[1]
  
  ##make an empty dataframe to hold the toy data
  dim1 <- rep(0,N*cluster.n)
  dim2 <- rep(0,N*cluster.n)
  dim3 <- rep(0,N*cluster.n)
  
  ##for each cluster, generate toy data and save them in dim1
  previous.N  <- 0
  for (i in 1:cluster.n){
    temp <- rnorm(N, means[i,1], stdevs[i,1])
    dim1[(previous.N+1):((previous.N)+N)]  <- temp
    previous.N  <- N + previous.N
  }
  
  ##for each cluster, generate toy data and save them in dim2
  previous.N  <- 0
  for (i in 1:cluster.n){
    temp <- rnorm(N, means[i,2], stdevs[i,2])
    dim2[(previous.N+1):((previous.N)+N)]  <- temp
    previous.N  <- N + previous.N
  }
  
  ##for each cluster, generate toy data and save them in dim3
  previous.N  <- 0
  for (i in 1:cluster.n){
    temp <- rnorm(N, means[i,3], stdevs[i,3])
    dim3[(previous.N+1):((previous.N)+N)]  <- temp
    previous.N  <- N + previous.N
  }
  
  toy.data <- data.frame(dim1, dim2, dim3)
  return(toy.data)
}


##define means for toy dataset clusters
dim1.mean <- c(2, 50, 10)
dim2.mean <- c(8, 6, 13)
dim3.mean <- c(2, 2.5, 40)
cluster.means <- data.frame(dim1.mean, dim2.mean, dim3.mean)

##define standard deviations for toy dataset clusters
dim1.stdev <- c(5, 1, 1)
dim2.stdev <- c(5, 1, 1)
dim3.stdev <- c(5, 1, 1)
cluster.stdevs <- data.frame(dim1.stdev, dim2.stdev, dim3.stdev)

##define number of samples wanted for each cluster
cluster.N <- 200

##make toy dataset using parameters specified above
my.toy.df <- make.toy.data(cluster.means, cluster.stdevs, cluster.N)

##save toy dataset to a .csv using comma as a delimitor
write.table(my.toy.df, "toy_data.csv", sep=",", row.names=FALSE, col.names=FALSE, quote=FALSE, append=FALSE)


# Change the data frame with training data to a matrix
#not scaled and centred but as a matrix
my.toy.df.matrix <- as.matrix(my.toy.df)

##plot unclustered data
library(scatterplot3d)
scatterplot3d(my.toy.df.matrix)

par(mfrow=c(1,3))
plot(my.toy.df.matrix[,1], my.toy.df.matrix[,2])
plot(my.toy.df.matrix[,2], my.toy.df.matrix[,3])
plot(my.toy.df.matrix[,3], my.toy.df.matrix[,1])