###################################
##########Fuzzy clustering#########
###################################


### fuzzy k-means


library(cluster)
data(faithful)
library(fclust)
FKM.faithful <- FKM(X=faithful,k=2,m=5)
s.vals <- silhouette(x=FKM.faithful$clus[,1],dist=dist(faithful)) # computes the silhouette values
plot(s.vals,main="Silhouette Plot of fuzzy k-means Solution",col="blue") # gives a plot of the silhouette values
plot(FKM.faithful)

#####interesting plot
plot(FKM.faithful,umin=0.5,ucex=TRUE)

head(FKM.faithful$clus)

###################################
########PD clustering##############
###################################

library(cluster)
data(faithful)
library(FPDclustering)

pdc.faithful=PDclust(faithful,k=2)
Silh(pdc.faithful$probability)

s.vals <- silhouette(x=pdc.faithful$label,dist=dist(faithful)) # computes the silhouette values
plot(s.vals,main="Silhouette Plot of PD-clustering Solution",col="blue") # gives a plot of the silhouette values





