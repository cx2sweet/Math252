#########################################################
###############Clustering correspondence analysis###############
#########################################################


library(clustrd)
data(cmc)#data on contraceptive choice in indonesia
#1,473 non-pregnant married women, 
#9 socio-economic characteristics and
#choice of contraceptive method.


###transforming some variables
#Wife's age and number of children are grouped based on quartiles
 cmc$W_AGE = ordered(cut(cmc$W_AGE, c(16, 26, 39, 49),
                          include.lowest = TRUE))
 levels(cmc$W_AGE) = c("16-26","27-39","40-49")
 cmc$NCHILD = ordered(cut(cmc$NCHILD, c(0, 1, 4, 17), right =
                             FALSE))
 levels(cmc$NCHILD) = c("0", "1-4", "5 and above")
#Tuning of Cluster CA based on average Silhouette width
 bestclusCA = tuneclus(cmc, nclusrange = 3:5, ndimrange =2:4, method = "clusCA" , criterion = "asw")

 bestclusCA$nclusbest#selected number of clusters
 bestclusCA$ndimbest #select dimension
 bestclusCA$critgrid
 sel=bestclusCA$clusobjbest
 #Plot the best solution (corresp. to lowest value of ASW)
 plot(sel, cludesc = TRUE)
 
 ###Other options
 resiFCB=clusmca(cmc,3,2,method = ("iFCB"))
 resMCAk=clusmca(cmc,3,2,method = ("MCAk"),alphak=.7)
 
 