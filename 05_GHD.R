
########################################
########Mixture of GH distributions#######
########################################


library(MixGHD)

data(ais,package="FPDclustering")

data=ais[,3:4]
plot(data)

GHD.out=MGHD(data,G=2:4,modelSel="BIC")
summary(GHD.out)
plot(GHD.out)

###Note the output is not a list but a S3 object

GHD.out@gpar[[1]]
GHD.out@gpar[[2]]

ARI(GHD.out@map,ais[,1])

####More than 2 variables

data=ais[,3:5]
plot(data)

GHD.out=MGHD(data,G=2:4)
summary(GHD.out)
plot(GHD.out)
ARI(GHD.out@map,ais[,1])
