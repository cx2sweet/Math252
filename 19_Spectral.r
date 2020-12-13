
#####Graph example####

library(mvtnorm)
library(igraph)

c1=rmvnorm(10, mean=c(0,0))
c2=rmvnorm(10, mean=c(4,4))#,sigma=diag(2)*0.5)
c3=rmvnorm(10, mean=c(4,0))

data=rbind(c1,c2,c3)
plot(data)
adj=dist(data)
adj2=adj
adj2[which(adj>1)]=0 #I choose the treshold
adj2[which(adj<=1)]=1


g1 <- graph_from_adjacency_matrix( adj2 )
plot(g1)


#########################################
########## Eigen decomposition ##########
#########################################

c1=rmvnorm(10, mean=c(0,0),sigma=diag(2)*0.2)
c2=rmvnorm(10, mean=c(4,4),sigma=diag(2)*0.2)
data=rbind(c1,c2)
plot(data)
adj=dist(data)
adj2=adj
adj2[which(adj>1)]=0 #I choose the treshold
adj2[which(adj<=1)]=1
adj2=as.matrix(adj2)
View(adj2)
eig=eigen(adj2)
View(eig$vectors)
#########################################
########## Spectral clustering ##########
#########################################
library(kernlab)
data(spirals)
plot(spirals)
sc <- specc(spirals, centers=2)### The output is not a list

centers(sc)# centers
size(sc) # n of elements per cluster
withinss(sc) #The within-cluster sum of squares for each cluster

plot(spirals, col=sc)

res=specc(as.matrix(iris[,1:4]),3)

plot(iris[,1:4],col=res)



c1=rmvnorm(200, mean=c(0,0))
c2=rmvnorm(200, mean=c(4,4),sigma=diag(2)*0.5)
c3=rmvnorm(200, mean=c(4,0))
l=c(rep(1,200),rep(2,200),rep(3,200))
data=rbind(c1,c2,c3)
res_sim=specc(data,3)

plot(data,col=res_sim)
library(MixGHD)
ARI(l,res_sim)



