######Example I PCA#######

data(wine,package="pgmm")#load the wine data
wine.sub=scale(wine[1:59,2:28])#standardize the data
pca.out=princomp(wine.sub) ##perform pca
plot(pca.out) ##screeplot
biplot(pca.out,choices=c(1,2)) ##biplot
biplot(pca.out,choices=c(3,4))

biplot(pca.out, xlabs = rep(".", nrow(wine.sub)))##easier to interpret
biplot(pca.out, xlabs = rep(".", nrow(wine.sub)),choices=c(3,4))##easier to interpret

######Example I MCA#######

library(FactoMineR)
data(tea)

mca.out = MCA(tea,graph=T,quanti.sup=19) #perform MCA
plot(mca.out$eig[,2])##to choose the number of components

mca.out = MCA(tea,ncp=4,graph=T,quanti.sup=19) #perform MCA
summary(mca.out)

plot(mca.out)
plot(mca.out,invisible=c("ind","quanti.sup"),cex=0.8)#easier to interpret
plot(mca.out,invisible=c("var","quanti.sup"),cex=0.8)#easier to interpret

plot(mca.out,invisible=c("ind","quanti.sup"),cex=0.8,axes=3:4)#easier to interpret
plot(mca.out,invisible=c("var","quanti.sup"),cex=0.8,axes=3:4)#easier to interpret
