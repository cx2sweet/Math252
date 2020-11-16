########################################
##Distance-Based Clustering, k-modes####
########################################


###  k-modes


library(klaR)


####German Credit data set
data(GermanCredit)
km=kmodes(GermanCredit[,3:4],2)


###Find the groups
c1=GermanCredit[which(km$cluster==1),3:4]
 c2=GermanCredit[which(km$cluster==2),3:4]
 
 ### results
 table(c1[,1])
table(c2[,1])
table(c2[,2])
 table(c1[,2])
 
 ########################################
 ##Distance-Based Clustering, k-prototypes####
 ########################################
 
 
 ###  k-modes
 
 
 library(clustMixType)
 
 
 ####German Credit data set
 data(GermanCredit)
 class(GermanCredit[,3])
 class(GermanCredit[,4])
 km=kproto(GermanCredit[,2:4],3)
 
 table(km$cluster,GermanCredit[,3])
 table(km$cluster,GermanCredit[,4])
 clprofiles(km, GermanCredit[,2:4])
 summary(km)
 
 