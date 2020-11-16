###############################################################
############   Model based Classification    ##################
###############################################################


library(mixture)
library(teigen)
library(MixGHD)

data(olive,package="pgmm")
data=as.matrix(olive[,3:10])
lab=olive[,2]
nm=572*0.2
mis=runif(nm,1,572)
lab[mis]=0
out.Gaus=gpcm(data,G=9,label=lab)
ARI(olive[,2],out.Gaus$map)

lab[mis]=NA
out.t=teigen(data,Gs=9,known=lab)
ARI(olive[,2],out.t$classification)
ARI(olive[mis,2],out.t$classification[mis])
ARI(olive[-mis,2],out.t$classification[-mis])



lab[mis]=0
out.MGHD=MGHD(data,G=9,label=lab)
ARI(olive[,2],out.MGHD@map)


###############################################################
##########   Model based Discriminant analysis  ###############
###############################################################

data(banknote)
banknote[,1]=as.numeric(factor(banknote[,1]))


res.t=teigen(banknote[,2:7],Gs=2, training=c(1:74,126:200),known=banknote[,1])
ARI(res.t$classification,banknote[,1])

##divide the data in training set and test set
train=banknote[c(1:74,126:200),]
test=banknote[75:125,]

##model estimation
res.GH=DA(train[,2:7],train[,1],test[,2:7],test[,1],method="MSGHD",max.iter=20)

res.GH$ARItest

####Try to do discriminant analysis on the olive oil data set

