
########################################
######## Student-t distributions#######
########################################


library(mvtnorm)

sigma=diag(2)
df=3
delta=c(5,5)
X=rmvt(500,sigma,df,delta)
plot(X)

#####contour Generation
x = seq(min(X[,1]),max(X[,1]),length.out=50)
y = seq(min(X[,2]),max(X[,2]),length.out=50)

xyS1 = matrix(0,nrow=length(x),ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    xy <- matrix(cbind(x[i],y[j]),1,2)	
    xyS1[i,j] = dmvt(x=xy,delta=delta,sigma=sigma,df=df,log = FALSE)
  }
}

contour(x=x,y=y,z=xyS1,nlevels=100,ylim=range(X[,2]),xlim=range(X[,1]), col="orange", main="Stundet-t distribution v=10")
points(X)

###########Correlation##########

sigma=matrix(c(2,0.9,0.9,2),2,2)
df=5
delta=c(0,0)
X=rmvt(500,sigma,df,delta)
plot(X)

#####contour Generation
x = seq(min(X[,1]),max(X[,1]),length.out=50)
y = seq(min(X[,2]),max(X[,2]),length.out=50)

xyS1 = matrix(0,nrow=length(x),ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    xy <- matrix(cbind(x[i],y[j]),1,2)	
    xyS1[i,j] = dmvt(x=xy,delta=delta,sigma=sigma,df=df,log = FALSE)
  }
}

contour(x=x,y=y,z=xyS1,nlevels=100,ylim=range(X[,2]),xlim=range(X[,1]), col="orange", main="Stundet-t distribution v=5 correlated data")
points(X)

#########################################################
########Mixture of multivariate-t distribution###########
#########################################################

library(teigen)
data(diabetes,package="mclust")
data=scale(diabetes[,3:4])
t.out <- teigen(x=data,Gs=1:5,models="UUUU") # fits mixtures of multivariate-t distributions to the data
class.predict.t <- t.out$classification # gets the vector of classifications for each observation
library(MixGHD)
table(diabetes[,1],class.predict.t)
ARI(diabetes[,1],class.predict.t)


#####contours
xmin=min(data[,1])-1
xmax=max(data[,1])+1
ymin=min(data[,2])-1
ymax=max(data[,2])+1
em=t.out$parameters
G=length(em$pi)
x = seq(xmin,xmax,length.out=50)
y = seq(ymin,ymax,length.out=50)


xyS1 = array(0,c(length(x),length(y),G))
for(g in 1:G){
for(i in 1:length(x)){
  for(j in 1:length(y)){
    xy <- matrix(cbind(x[i],y[j]),1,2)	
    sig=as.matrix(t.out$parameters$sigma[,,g])
    xyS1[i,j,g] = dmvt(x=xy,delta=as.vector((t.out$parameters$mean[g,])),sigma=sig,df=t.out$parameters$df[g],log = FALSE)
  }
}
}
zz=xyS1[,,1]*t.out$parameters$pig[1]
for(k in 2:G){
  zz=xyS1[,,k]*t.out$parameters$pig[k]+zz}
contour(x=x,y=y,z=zz,nlevels=100,ylim=range(data[,2]),xlim=range(data[,1]), col="orange", main="Stundet-t distribution v=5 correlated data")
points(data)

##################### 3 variables
library(teigen)
data(diabetes,package="mclust")
data=diabetes[,2:4]
t.out <- teigen(x=data,Gs=1:5,models="UUUU") # fits mixtures of multivariate-t distributions to the data
class.predict.t <- t.out$classification # gets the vector of classifications for each observation
library(MixGHD)
table(diabetes[,1],class.predict.t)
ARI(diabetes[,1],class.predict.t)

plot(data,col=t.out$classification)
table(t.out$classification)
t.out$parameters$mean
t.out$parameters$df
t.out$parameters$sigma

############Skew normal and Skew t
library(mixsmsn)
#smsn.mix
sigma=diag(2)
del=c(0,-20)
df=7
mean=c(0,0)
arg1=list(mu=mean,Sigma=sigma,shape=del)
arg1=list(arg1,arg1)
arg2=list(mu=mean,Sigma=sigma,shape=del,nu=df)
arg2=list(arg2,arg2)
data=rmmix(n=500,p=c(.5,.5),family = 'Skew.normal',arg=arg1)#
data2=rmmix(n=500,p=c(.5,.5),family = 'Skew.t',arg=arg2)#
plot(data)
plot(data2)





data(diabetes,package="mclust")
### Fitting 5 mixtures of multivariate skew-normal and multivariate skew-t distributions using EmSkew(...) to the diabetes data
z=diabetes[,2:4]
res=smsn.mmix(y=z,g=3,family ="Skew.t",get.init = TRUE,group = TRUE,kmeans.param=list(iter.max = 10, n.start = 20, algorithm = "Hartigan-Wong")) 
msn.out <- list()
for(g in 1:5) msn.out[[g]] <- smsn.mmix(y=z,g=g,family ="Skew.normal",get.init = TRUE,group = TRUE)  # function to fit the skew-normal distributions
bic.msn <- numeric(5)
for(g in 1:5) bic.msn[g] <- msn.out[[g]]$bic # saves the bic values from each skew-normal mixture
which(bic.msn==min(bic.msn)) # gives the maximum component
plot(z,col=msn.out[[3]]$group)

mst.out <- list()
for(g in 1:3) mst.out[[g]] <- msn.out[[g]] <- smsn.mmix(y=z,g=g,family ="Skew.t",get.init = TRUE,group = TRUE)  # function to fit the skew-normal distributions
bic.mst <- numeric(3)
for(g in 1:3) bic.mst[g] <- mst.out[[g]]$bic # saves the bic values from each skew-t mixture
bic.mst
which(bic.mst==min(bic.mst)) # gives the maximum component.
plot(z,col=mst.out[[3]]$group)

###bivariate data
mst.out2 <-smsn.mmix(y=scale(z[,1:2]),g=2,family ="Skew.t",get.init = TRUE,group = TRUE)  # function to fit the skew-normal distributions


mix.contour(scale(z[,1:2]),mst.out2,ncontour=40)
