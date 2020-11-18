#####################################################
######## Mixtures of Factor Analyzers ###############
#####################################################
library(pgmm)
library(MixGHD)
data(wine,package="pgmm")
data=wine[,-1]

MFA.out=pgmmEM(data, rG=3, rq=2:5, modelSubset=c("UUU"),
               seed=runif(1,min=0,max=1000000))
summary(MFA.out)
ARI(MFA.out$map,wine[,1])
MFA.out$load
#The factor loadings matrix (Lambda) for the best model.
head(MFA.out$noisev	)
#The Psi matrix for the best model.

MFA.out=pgmmEM(data, rG=2:4, rq=2:4, modelSubset=c("UUU"),seed=4)
summary(MFA.out)
ARI(MFA.out$map,wine[,1])
#####################################################
####### Mixtures of t Factor Analyzers##############
#####################################################
data(wine,package="pgmm")
data=wine[,-1]

library(teigen)
MTFA.out=teigen(data, models="UUUU", Gs=3,init="hard")
ARI(MTFA.out$classification,wine[,1])
MTFA.out$parameters$df
MTFA.out$parameters$mean
head(MTFA.out$parameters$sigma)

MTFA.out=teigen(data, models="UUUU", Gs=2:3, init="hard")
ARI(MTFA.out$classification,wine[,1])

#####################################################
####### Mixtures of GH Factor Analyzers##############
#####################################################

###IF we know G and q
library(MixGHD)
data(wine,package="pgmm")
data=wine[,-1]
MGHFA.out=MGHFA(data,G=3,q=3)
ARI(MGHFA.out@map,wine[,1])
summary(MGHFA.out)
plot(MGHFA.out)
MGHFA.out@gpar[[1]]$alpha
MGHFA.out@gpar[[1]]$mu
MGHFA.out@gpar[[1]]$cpl
head(MGHFA.out@gpar[[1]]$sigma)
head(MGHFA.out@gpar[[1]]$err)
###IF we don't know G and q
MGHFA.out=MGHFA(data,G=2:4,q=2:4)
ARI(MGHFA.out@map,wine[,1])
summary(MGHFA.out)
