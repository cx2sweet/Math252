library("usmap")
library(ggplot2)
library(mclust)
#GMM
sol=kmeans(UScost[,3:7],4)
#Plot
dataplot1=data.frame(state=UScost$X1,cluster=factor(sol$cluster))
plot_usmap(regions = "states",data=dataplot1,values = 'cluster')+theme(legend.position='right',legend.title = element_blank())

