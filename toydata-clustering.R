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
par(mfrow=c(1,1))
scatterplot3d(my.toy.df.matrix)

par(mfrow=c(1,3))
plot(my.toy.df.matrix[,1], my.toy.df.matrix[,2])
plot(my.toy.df.matrix[,2], my.toy.df.matrix[,3])
plot(my.toy.df.matrix[,3], my.toy.df.matrix[,1])

##do K-means clustering on toy dataset
km.out = kmeans(my.toy.df.matrix, 3, nstart = 100)
km.out

##replot and colour code clusters
scatterplot3d(my.toy.df.matrix, color = km.out$cluster, pch = 19)


##SOM on same data 
library(kohonen)

# Also center and scale all variables
my.toy.df.matrix <- as.matrix(scale(my.toy.df))

##Create the SOM Grid - you generally have to specify the size of the 
##training grid prior to training the SOM. Hexagonal and Circular 
##topologies are possible
som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

##Train the SOM, options for the number of iterations,
##the learning rates, and the neighbourhood are available
som_model <- som(my.toy.df.matrix, grid=som_grid, rlen=100, alpha=c(0.05,0.01), keep.data = TRUE, n.hood='circular')

##visualise the quality of your generated SOM
plot(som_model, type="changes")

##visualise the count of how many samples are mapped to each node on the map
plot(som_model, type="count")

##“U-Matrix”, this visualisation is of the distance between each node and its neighbours
plot(som_model, type="dist.neighbours")

##visualise the node weight vectors, or “codes”, which are made up of normalised  
##values of the original variables used to generate the SOM
plot(som_model, type="codes")

##visualize heatmap (note-these are scaled and centred)
coolBlueHotRed <- function(n, alpha = 1) { rainbow(n, end=4/6, alpha=alpha)[n:1] }
par(mfrow=c(1,3))
plot(som_model, type = "property", property = som_model$codes[,1], main=names(som_model$data)[4], palette.name=coolBlueHotRed)
plot(som_model, type = "property", property = som_model$codes[,2], main=names(som_model$data)[4], palette.name=coolBlueHotRed)
plot(som_model, type = "property", property = som_model$codes[,3], main=names(som_model$data)[4], palette.name=coolBlueHotRed)

##Cluster SOM data
mydata <- som_model$codes 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
par(mfrow=c(1,1))
plot(wss)
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes)), 3)
##make a pretty colour pallete
pretty.colours  <- c("thistle1", "tomato1", "turquoise", "slategray1", "sienna1", "violet", "skyblue")
##plot these results:
plot(som_model, type="mapping", bgcol = pretty.colours[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)

