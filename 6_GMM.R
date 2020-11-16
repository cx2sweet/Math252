#####################################################
###Mixtures of Multivariate Gaussian Distributions###
#####################################################

library(mvtnorm)


######function rmvnorm
##data generation
sig=diag(2)
mu=c(4,4)
X= rmvnorm(400, mean = mu, sigma = sig)
        
plot(X)

#####contour Generation
x = seq(min(X[,1]),max(X[,1]),length.out=50)
y = seq(min(X[,2]),max(X[,2]),length.out=50)

xyS1 = matrix(0,nrow=length(x),ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    xy <- matrix(cbind(x[i],y[j]),1,2)	
    xyS1[i,j] = dmvnorm(x=xy,mean=as.vector(mu),sigma=sig)
  }
}

contour(x=x,y=y,z=xyS1, levels=seq(0.001,0.7,by=0.01),ylim=range(X[,2]),xlim=range(X[,1]),col="green")
points(x=mu[1],y=mu[2],pch="X",col=2)
points(X)



###############GMM
data(faithful)
head(faithful)
plot(faithful,xlab="eruptions",ylab="waiting",pch=20)


### The MLEs: For a GMM 

library(mixture)
z <- as.matrix(faithful)
gmm.fit <- gpcm(data=z,G=2,mnames="VVV") # fits a two-component GMM
comp1.par <- gmm.fit$gpar[[1]] # gives the MLEs for the first component of the GMM
comp2.par <- gmm.fit$gpar[[2]] # gives the MLEs for the second component of the GMM

### Contour generation to visualize the density created by the GMMs
x = seq(min(z[,1]),max(z[,1]),length.out=50)
y = seq(min(z[,2]),max(z[,2]),length.out=50)

mu1 <- comp1.par$mu
sigma1 <- comp1.par$sigma
mu2 <- comp2.par$mu
sigma2 <- comp2.par$sigma

xyS1 <- xyS2 <- matrix(0,nrow=length(x),ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    xy <- matrix(cbind(x[i],y[j]),1,2)	
    xyS1[i,j] = dmvnorm(x=xy,mean=as.vector(mu1),sigma=sigma1)
    xyS2[i,j] = dmvnorm(x=xy,mean=as.vector(mu2),sigma=sigma2)
  }
}

pi.g <- gmm.fit$gpar$pi

contour(x=x,y=y,z=pi.g[1]*xyS1+pi.g[2]*xyS2, levels=c(0.00005,0.0001,0.0005,0.001,.005,.01,.025),ylim=range(z[,2]),xlim=range(z[,1]),xlab="eruptions",ylab="waiting")
points(z,pch=20,col=gmm.fit$map)
class.predict.gpcm <- gmm.fit$map # gets the vector of classifications for each observation


####package mclust

library(mclust) # loads required package for Mclust

mclust.out <- Mclust(z,G=2,modelNames="VVV") ####We will go back on the option VVV

mclust.out$parameters$mean  ##the output is different
class.predict.mclust <- mclust.out$classification # gets the vector of classifications for each observation
 table(class.predict.mclust,class.predict.gpcm)

