
############################################################
############################################################
#########################    DLM    ########################
############################################################
############################################################

library(FisherEM)
data(iris)
DLM.out = fem(iris[,-5],K=2:5,model='AkB',nstart=50)
summary(DLM.out)
plot(DLM.out)
fem.ari(DLM.out,as.numeric(iris[,5]))


DLM.out2 = fem(iris[,-5],K=3)
DLM.out2$model
DLM.out2$mean

plot(DLM.out2)
fem.ari(DLM.out2,as.numeric(iris[,5]))
