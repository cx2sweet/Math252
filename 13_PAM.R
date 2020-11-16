###################################
##Distance-Based Clustering, PAM###
###################################


### PAM

## The following code performs the PAM Example given in Lecture 9

# Data Creation and Visualization

load("SimpleKMeans_Ex.RData") # Loads the data set created above.
X # Displays the numerical values of the data set
plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5)) # Plots the data

# Step 1: Initialization

load("InitialCenters.RData") # loads the initial centers
init.c <- X[set.initc,] # Isolates and saves the initial centers
init.c

plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5))
points(init.c[1,],col=2,pch="X")
points(init.c[2,],col=3,pch="X")
points(init.c[3,],col=4,pch="X")


# Step 2: Cost for each medoid

G <- 3
n <- 10
d <- matrix(NA,n,G)
for(i in 1:n){
	for(g in 1:G){
		d[i,g] <- sum(abs(X[i,] - init.c[g,]))
		}	
	}
d
	
plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5))
points(init.c[1,],col="red",pch="X")
v.interest <- c(1:10)[-set.initc[1]]
for(i in v.interest){
	segments(init.c[1,1],init.c[1,2],X[i,1],X[i,2])
	}

plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5))
points(init.c[2,],col="green",pch="X")
for(i in c(1:10)[-set.initc[2]]){
	segments(init.c[2,1],init.c[2,2],X[i,1],X[i,2])
	}

plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5))
points(init.c[3,],col="blue",pch="X")
for(i in c(1:10)[-set.initc[3]]){
	segments(init.c[3,1],init.c[3,2],X[i,1],X[i,2])
	}


# Step 3: Calculate lowest cost

allocate <- NULL	
for(i in 1:n){
	allocate[i] <- which(d[i,] == min(d[i,]))
	}
allocate

plot(X,xlab="Variable 1",ylab="Variable 2",pch=20,xlim=c(0.5,4),ylim=c(1,5),col=allocate+1)
points(init.c[1,1],init.c[1,2],col="red",pch="X")
points(init.c[2,1],init.c[2,2],col="green",pch="X")
points(init.c[3,1],init.c[3,2],col="blue",pch="X")


total.cost <- 0
for(i in 1:n){
	print(d[i,allocate[i]])
	total.cost <- total.cost + d[i,allocate[i]]
	}

total.cost

# PAM in R
library(cluster)
data(faithful)
pam.faithful <- pam(x=wine,k=2)
kmeans.faithful <- kmeans(x=wine,centers=2,nstart=10)
s.vals <- silhouette(x=kmeans.faithful$cluster,dist=dist(wine,method="manhattan")) # computes the silhouette values
plot(s.vals,main="Silhouette Plot of k-means Solution",col="blue") # gives a plot of the silhouette values

s.vals2 <- silhouette(x=pam.faithful$cluster,dist=dist(wine,method="manhattan")) # computes the silhouette values
plot(s.vals2,main="Silhouette Plot of k-medoids Solution",col="blue") # gives a plot of the silhouette values

data(ais,package="alr3")
sub.ais <- ais[,c(10,12)]
pam.ais <- pam(x=sub.ais,k=2)
kmeans.ais <- kmeans(x=sub.ais,centers=2,nstart=10)

s.vals <- silhouette(x=kmeans.ais$cluster,dist=dist(sub.ais)) # computes the silhouette values
plot(s.vals,main="Silhouette Plot of k-means Solution",col="blue") # gives a plot of the silhouette values

s.vals2 <- silhouette(x=pam.ais$cluster,dist=dist(sub.ais)) # computes the silhouette values
plot(s.vals2,main="Silhouette Plot of k-medoids Solution",col="blue") # gives a plot of the silhouette values



# Classification Table
(t.ais <- table(pam.ais$cluster,kmeans.ais$cluster))

(t.faithful <- table(pam.faithful$cluster,kmeans.faithful$cluster))
# ARI

#faithful
library(e1071)
classAgreement(t.faithful)
(ari1.faithful <- classAgreement(t.faithful)$crand)

library(MixGHD)
(ari2.faithful <- ARI(kmeans.faithful$cluster,pam.faithful$cluster))


# ais
library(e1071)
classAgreement(t.faithful)
(ari1.ais <- classAgreement(t.ais)$crand)

library(MixGHD)
(ari2.ais <- ARI(kmeans.ais$cluster,pam.ais$cluster))
