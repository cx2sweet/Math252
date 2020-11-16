binarydis<-function(v1,v2,method="Matching"){
  t=table(v1,v2)
  ##test the input
  if(nrow(t)>2 | ncol(t)>2 ){stop("v1 and v2 have to be binary vectors")}
  a=t[2,2]
  b=t[1,2]
  c=t[2,1]
  d=t[1,1]
  if(method=="RT"){
    s=(a+d)/(a+2*(b+c)+d)
  }
  else if(method=="GL1"){
    s=(a+d)/(a+0.5*(b+c)+d)
  }
  else if(method=="Jaccard"){
    s=(a)/(a+b+c)
  }
  else if(method=="SS"){
    s=(a)/(a+2*(b+c))
  }
  else if(method=="GL2"){
    s=(a)/(a+0.5*(b+c))
  }
  else{
    s=(a+d)/(a+b+c+d)
  }
  s
  }
