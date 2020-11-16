#########################################################
###############Factor Discriminant k-means###############
#########################################################


library(clustrd)
data(macro)

outRKM = cluspca(macro, 3, 2, method = "RKM", rotation = "varimax")
outRKM$centroid
plot(macro,col=outRKM$cluster)
plot(outRKM, cludesc = TRUE)
 
outFKM = cluspca(macro, 3, 3, method = "FKM", rotation = "varimax")
outFKM$centroid
plot(macro,col=outFKM$cluster)
plot(outFKM, cludesc = TRUE)

###############Choice of q

outpca=princomp(macro)
plot(outpca)

###or silhouette


outRKM = cluspca(macro, 3, 3, method = "RKM", rotation = "varimax")
outRKM$centroid
plot(macro,col=outRKM$cluster)
plot(outRKM, cludesc = TRUE)


##################Exercise

data(wine,package="pgmm")
data=wine[,2:28]



#########################################################
#################Factor PD-clustering####################
#########################################################

library(FPDclustering)
TuckerFactors(macro,3)
outFPDC=FPDC(macro,3,5,5)
Silh(outFPDC$probability)


data('outliers')
x<-outliers[,-1]
plot(x[,1:2])
TuckerFactors(x,4)
fpdout=FPDC(x,4,5,4)
table(outliers[,1],fpdout$label)
Silh(fpdout$probability)

##################Exercise

data(wine,package="pgmm")
data=wine[,2:28]


