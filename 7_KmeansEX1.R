#################################
### Distance-Based Clustering ###
#################################

### k-means clustering

## The following code performs Example I given in Lecture 7

# Data Creation and Visualization

# var1 <- round(runif(10,1,5),digits=2) # Randomly sample values for the first variable from a uniform distribution.
# var2 <- round(runif(10,1,5),digits=2) # Randomly sample values for the second variable from a uniform distribution.
# X <- data.frame(cbind(var1,var2)) # Combine the randomly sampled values into a data frame to create a bivariate data set.
# save(X,file="SimpleKMeans_Ex.RData") # Save the data set
load("SimpleKMeans_Ex.RData") # Loads the data set created above.
X # Displays the numerical values of the data set
plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5)) # Plots the data

# Step 1: Initialization

# set.initc <- sample(1:10,3) # Randomly sample three numbers from the values 1 to 10, these are the initial centers.
# save(set.initc,file="InitialCenters.RData") # saves the initial centers
load("InitialCenters.RData") # loads the initial centers
init.c <- X[set.initc,] # Isolates and saves the initial centers
init.c

plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5))
points(init.c[1,],col=2,pch="X")
points(init.c[2,],col=3,pch="X")
points(init.c[3,],col=4,pch="X")


# Step 2: Allocate the observations to each group mean (this is unlabelled to provide YOU with the challenge of interpreting the code)

G <- 3
n <- 10
d <- matrix(NA,n,G)
for(i in 1:n){
	for(g in 1:G){
		combination <- rbind(X[i,],init.c[g,])
		d[i,g] <- dist(combination)^2
		}	
	}
d
	
allocate <- NULL	
for(i in 1:n){
	allocate[i] <- which(d[i,] == min(d[i,]))
	}
allocate

plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5),col=allocate+1)
points(init.c[1,],col="red",pch="X")
points(init.c[2,],col="green",pch="X")
points(init.c[3,],col="blue",pch="X")

# Step 3: Recompute the means

means <- matrix(NA,G,2)
for(g in 1:G){ 
	cat("Group",g,"\n")
	partition <- which(allocate == g)
	cat("Parition is equal to",partition,"\n")
	means[g,] <- apply(X[partition,],2,mean)
	}

plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5),col=allocate+1)
points(means[1,1],means[1,2],col="red",pch="X")
points(means[2,1],means[2,2],col="green",pch="X")
points(means[3,1],means[3,2],col="blue",pch="X")

#Step 4: Reallocation

G <- 3
n <- 10
d <- matrix(NA,n,G)
for(i in 1:n){
	for(g in 1:G){
		combination <- rbind(X[i,],means[g,])
		d[i,g] <- dist(combination)^2
		}	
	}
d
	
allocate <- NULL	
for(i in 1:n){
	allocate[i] <- which(d[i,] == min(d[i,]))
	}
allocate

plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5),col=allocate+1)
points(means[1,1],means[1,2],col="red",pch="X")
points(means[2,1],means[2,2],col="green",pch="X")
points(means[3,1],means[3,2],col="blue",pch="X")

## Example II - Faithful Data

data(faithful) # loads faithful data
plot(faithful,xlab="Eruptions",ylab="Waiting Time",pch=20) # plots faithful data
?kmeans
kmeans.out1 <- kmeans(x=faithful,centers=2) # performs kmeans clustering for g=2 groups on faithful data
obs.group <- kmeans.out1$cluster # gives the vector of group memberships resulting from kmeans clustering
grp.means <- kmeans.out1$center # gives the estimates of the cluster centers for each group resulting from kmeans clustering
plot(faithful,xlab="Eruptions",ylab="Waiting Time",pch=obs.group,col=obs.group)
points(grp.means[1,1],grp.means[1,2],col="black",pch="X")
points(grp.means[2,1],grp.means[2,2],col="red",pch="X")

###Starting points
cent=matrix(c(2,40,2.1,42),2,2)
 kmeans.out2 <- kmeans(x=faithful,centers=cent)
 cent=matrix((runif(4)*80+1.5),2,2)
 kmeans.out3 <- kmeans(x=faithful,centers=cent)
 
 cent=matrix(c(2,51,2,70),2,2)
 kmeans.out3 <- kmeans(x=faithful,centers=cent)
 
 
