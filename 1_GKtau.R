GKtau=function(H,D){
    C=0
    Di=0
    n=length(D)
    for(i in 1:(n-1)){
        for(j in (i+1):n){
            if((H[i]>H[j]&&D[i]>D[j])||(H[i]<H[j]&&D[i]<D[j]))C=C+1
            else if ((H[i]>H[j]&&D[i]<D[j])||(H[i]<H[j]&&D[i]>D[j]))Di=Di+1
        }}
    tau=(C-Di)/(C+Di)
    tau}