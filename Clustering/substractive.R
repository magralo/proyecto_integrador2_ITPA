

dist=function(x,y){
  
  dif=(x-y)
  dif2=sqrt((x-y)^2)
  return(mean(as.numeric(dif2)))
  
}

densidad=function(X,v,a){
  datos=nrow(X)
  suma=0
  for(i in 1 :datos){
    suma=suma+exp(-a*dist(X[i,],v))
  }
  return(suma)
}

cerca=function(X,v,a){
  n=nrow(X)
  d=0
  for(i in 1:n){
    
    d=c(d,dist(X[i,],v))
  }
  d=d[-1]
  salen=which(d<=a)
  return(salen)
}

subtra=function(X,a){
  ### Elijo el de mayor densidad y le llevo lo que esté a un radio a
  medias=matrix(rep(colMeans(X),nrow(X)),ncol = ncol(X),byrow = T)
  sds=matrix(rep(apply(X,2,sd),nrow(X)),ncol = ncol(X),byrow = T)
  X=(X-medias)/sds
  
  grupos=list()
  Pcentros=X
  faltan=nrow(X)
  k=1
  cluster=rep(0,faltan)
  clus=1
  while(faltan>0){
   
    n=faltan
    if(faltan>1){
    d=0
  
    for(i in 1:n){
      d=c(d,densidad(Pcentros,Pcentros[i,],a))
    }
    d=d[-1]
    seleccionado=Pcentros[which.max(d),]
    salen=cerca(Pcentros,seleccionado,a)
    if(k==1){
      centros=seleccionado
    }else{
      centros=rbind(centros,seleccionado)
    }
    cluster[salen]=clus
    faltan=faltan-length(salen)
    grupos[[k]]=Pcentros[salen,]

    Pcentros=Pcentros[-salen,]
    k=k+1
    }else{
      grupos[[k]]=Pcentros
      cluster[which(cluster==0)]=clus
      centros=rbind(centros,Pcentros)
      faltan=0
    }
    clus=clus+1
  }
  
  return(list(centros=centros,cluster=cluster))
  
}
  






