library(FactoMineR)
library(Rmixmod)
data("hobbies")
head(hobbies)
X = hobbies[,1:18]
# Clustering with LCM through Rmixmod
res = mixmodCluster(X,nbCluster =1:5, dataType="qualitative"
  , model = mixmodMultinomialModel( listModels=c("Binary_pk_Ekjh") ),criterion = ("BIC"))
# Visualization of the best result according to BIC
par(mfrow=c(1,2))
lbl = res@results [[1]] @partition
acm = MCA(X)##Just for visualization, more next class
plot(acm$ind$coord[,1:2],col=lbl ,pch=c(17 ,15 ,18 ,19)[lbl],
     xlab='',ylab='',main="Factor map of observations")

###less variables for visualization
res = mixmodCluster(X[,1:4],nbCluster =1:5, dataType="qualitative"
                    , model = mixmodMultinomialModel( listModels=c("Binary_pk_Ekjh") ), criterion = ("BIC"))

# Barplot of cluster-conditional level frequency
par(mfrow=c(1,1))
barplot(res)

#################################################
#################################################
############Parsimonious Models #################
#################################################
#################################################
?mixmodMultinomialModel

###less variables for visualization
res = mixmodCluster(X[,1:4],nbCluster =1:5, dataType="qualitative"
                    , model = mixmodMultinomialModel( listModels=c("Binary_pk_Ekjh","Binary_pk_E") ), criterion = ("BIC"))

res@bestResult@model
res@bestResult@parameters
summary(res)
# Barplot of cluster-conditional level frequency
par(mfrow=c(1,1))
barplot(res)
##All the models
res = mixmodCluster(X[,1:4],nbCluster =1:5, dataType="qualitative"
                    , model = mixmodMultinomialModel( ), criterion = ("BIC"))
res@bestResult@model
res = mixmodCluster(X[,1:4],nbCluster =1:5, dataType="qualitative"
                    , model = mixmodMultinomialModel( ), criterion = ("ICL"))
res@bestResult@model

#################################################
#################################################
############## Mixed-type data ##################
#################################################
#################################################


data(heterodata)
View(heterodata)
resMix=mixmodCluster(heterodata,2)
resMix

plot(heterodata[,4:5],col=resMix@bestResult@partition)
plot(resMix,showOnly={'quantitative'})
plot(resMix,showOnly={'qualitative'})


library(clustMD)
data=matrix(as.numeric(unlist(heterodata[,c(4,5,1,2,3)])),nrow=nrow(heterodata),ncol=ncol(heterodata))
resMD <- clustMD(X=data, G=2, CnsIndx =2, OrdIndx =5, Nnorms =20000 ,
               MaxIter =500, model="VVI", store.params=FALSE , scale=TRUE ,
               startCL="kmeans")

table(resMix@bestResult@partition,resMD$cl)
