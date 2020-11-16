## Example II - Faithful Data

data(faithful) # loads faithful data
plot(faithful,xlab="Eruptions",ylab="Waiting Time",pch=20) # plots faithful data
?kmeans
kmeans.out1 <- kmeans(x=faithful,centers=2) # performs kmeans clustering for g=2 groups on faithful data
obs.group <- kmeans.out1$cluster # gives the vector of group memberships resulting from kmeans clustering
grp.means <- kmeans.out1$center # gives the estimates of the cluster centers for each group resulting from kmeans clustering
plot(faithful,xlab="Eruptions",ylab="Waiting Time",pch=20,col=obs.group)
points(grp.means[1,1],grp.means[1,2],col="black",pch="X")
points(grp.means[2,1],grp.means[2,2],col="red",pch="X")

###Starting points
cent=matrix(c(2,40,2.1,42),2,2)
kmeans.out2 <- kmeans(x=faithful,centers=cent)

cent=matrix((runif(4)*80+1.5),2,2)
kmeans.out3 <- kmeans(x=faithful,centers=cent)

cent=matrix(c(2,51,2,70),2,2)
kmeans.out3 <- kmeans(x=faithful,centers=cent)


### First measure of cluster strength.

ratio.strength <- function(X,G,output){ # A function to compute Calinski and  Harabasz  ratio 
  n <- nrow(X) # computes the number of observations
  sst <- cov.wt(X)$cov*(n-1) # computes the SST matrix
  # print(sst)
  ssw <- 0 # sets the initial SSW to be 0
  for(g in 1:G){ # for each group g
    mu.g <- output$center[g,] # isolate the mean vector of interest
    partition <- which(g == output$cluster) # gives the relevant observations determined from allocate
    ng <- length(partition) # counts the number of observations in each group
    ssw <- ssw + cov.wt(X[partition,],center=mu.g)$cov*(ng-1) # computes the ssw for group g and sums over all g
  }
  ssb <- sst - ssw  # computes SSB matrix
  val <- (sum(diag(ssb))*(n-G))/(sum(diag(ssw))*(G-1)) # Computes the ratio given on Slide 11
  return(val)
}


### Faithful Data

data(faithful)
kmeans.out <- list() # set kmeans.out to be a list
ratio <- NULL
for(g in 2:4){
  kmeans.out[[g]] <- kmeans(x=faithful,centers=g) # stores the output from kmeans in a list designated by the number of groups
  ratio[g] <- ratio.strength(X=faithful,G=g,output=kmeans.out[[g]])
}

ratio
which(ratio==max(ratio,na.rm=TRUE))

# Do twenty more runs for the k-means results.

chose.G <- matrix(NA,20,5)
data(faithful)
kmeans.out <- list() # set kmeans.out to be a list
ratio <- NULL
for(i in 1:20){
  for(g in 2:4){
    kmeans.out[[g]] <- kmeans(x=faithful,centers=g) # stores the output from kmeans in a list designated by the number of groups
    ratio[g] <- ratio.strength(X=faithful,G=g,output=kmeans.out[[g]])
  }
  chose.G[i,2:5] <- ratio
  chose.G[i,1] <- which(ratio==max(ratio,na.rm=TRUE))
}


#######multiple starting values
chose.G2 <- matrix(NA,20,5)
data(faithful)
kmeans.out <- list() # set kmeans.out to be a list
ratio <- NULL
for(i in 1:20){
for(g in 2:4){
  kmeans.out[[g]] <- kmeans(x=faithful,centers=g,nstart=10) # stores the output from kmeans in a list designated by the number of groups
  ratio[g] <- ratio.strength(X=faithful,G=g,output=kmeans.out[[g]])
}
  chose.G2[i,2:5] <- ratio
  chose.G2[i,1] <- which(ratio==max(ratio,na.rm=TRUE))
}

#############################################
#############################################
################Silhouette###################
#############################################
#############################################

data(faithful)
kmeans.out <- kmeans(x=faithful,centers=2,nstart=10) # stores the output from kmeans in a list designated by the number of groups

# Demonstration of silhouette function for G=2 group kmeans solution.
library(cluster)
s.vals <- silhouette(x=kmeans.out$cluster,dist=dist(faithful)) # computes the silhouette values
plot(s.vals,main="Silhouette Plot of Two Groups Solution",col="blue") # gives a plot of the silhouette values
# Which observations are ``reasonably-classified?"

# Considering all observations at once.
which(s.vals[,3] > 0.5)

# Considering only the observations in group 1
grp.1 <- which(s.vals[,1]==1)
which(s.vals[grp.1,3] > 0.5)

# Considering only the observations in group 2
grp.2 <- which(s.vals[,1]==2)
which(s.vals[grp.2,3] > 0.5)



# Compare average Silhouette Width to make a decision about G

data(faithful)
kmeans.out <- list() # set kmeans.out to be a list
s.width <- NULL
dist.mat <- dist(faithful)
for(g in 2:4){
  kmeans.out[[g]] <- kmeans(x=faithful,centers=g,nstart=10) # stores the output from kmeans in a list designated by the number of groups
  clust.soln <- kmeans.out[[g]]$cluster
  s.vals <- silhouette(x=clust.soln,dist=dist.mat)
  s.width[g] <- mean(s.vals[,3])
}

s.width




