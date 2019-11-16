
norma=function(x,y){return(sum((x-y)*(x-y)))}

mv=function(X,v,sigma){
  valor=0
  for (i in 1:nrow(X)){
    extra=exp(-norma(X[i,],v)/(2*sigma^2))
    valor= valor+extra
  }
  return(valor)
}

actualizar=function(M,c,v,mejor,beta){
  nuevo=M
  for (i in 1:length(M)){
    nuevo[i]=M[i]-mejor*exp(-norma(v[i,],c)/(2*beta^2))
  }
  return(nuevo)
}

centros=function(c,x){
  chat=0
  for (i in 1:nrow(X)){
    d=0
    for(j in 1 : nrow(c)){
      d=c(d,norma(X[i,],c[j,]))
    }
    d=d[-1]
    chat=c(chat,which.min(d))
    
  }
  chat=chat[-1]
  return(chat)
}
    






MountainCluster=function(X,h=1,K=3,lp=16,s=1000){
  
  mins=apply(X,2,min)
  maxs=apply(X,2,max)
  for(i in 1:length(mins)){
    if (i==1){
     cord=seq(mins[i],maxs[i],by=(maxs[i]-mins[i])/lp) 
    }else{
      aux=seq(mins[i],maxs[i],by=(maxs[i]-mins[i])/lp) 
      cord=cbind(cord,aux)
    }
  }
  
  
  V=cross_df(data.frame(cord))
  s=min(nrow(V),s)
  V=V[sample(nrow(V),s),]
  
  Mountain=rep(0,nrow(V))
  for (i in 1: nrow(V)){
    Mountain[i]=mv(X,V[i,],h)
  }
  #K=3
  cen=V[1:K,]
  for (i in 1: K){
    mejor=V[which.max(Mountain),]
    cen[i,]=mejor
    vieja=Mountain
    Mountain=actualizar(vieja,mejor,V,max(vieja),h)
  }
  
  cluster=rep(0,nrow(X))
  
  for (c in 1:length(cluster)){
    D=cen-matrix(as.numeric(rep(X[c,],K)),ncol=ncol(X),byrow=T)
    cluster[c]=which.min(apply(D*D,1,sum))
  }
  return(list(cluster=cluster,centros=cen))

}



initial_mount=function(X,h=1,lp=16,s=1000){
  
  mins=apply(X,2,min)
  maxs=apply(X,2,max)
  for(i in 1:length(mins)){
    if (i==1){
      cord=seq(mins[i],maxs[i],by=(maxs[i]-mins[i])/lp) 
    }else{
      aux=seq(mins[i],maxs[i],by=(maxs[i]-mins[i])/lp) 
      cord=cbind(cord,aux)
    }
  }
  
  
  V=cross_df(data.frame(cord))
  s=min(nrow(V),s)
  V=V[sample(nrow(V),s),]
  
  Mountain=rep(0,nrow(V))
  for (i in 1: nrow(V)){
    Mountain[i]=mv(X,V[i,],h)
  }
  
  return(list(Mountain=Mountain,
              V=V))
  
  
  
}


mount_cluster=function(Mountain,V,h=0.1,K=3){
  
  cen=V[1:K,]
  for (i in 1: K){
    mejor=V[which.max(Mountain),]
    cen[i,]=as.numeric(mejor)
    vieja=Mountain
    Mountain=actualizar(vieja,mejor,V,max(vieja),h)
  }
  
  cluster=rep(0,nrow(X))
  
  for (c in 1:length(cluster)){
    D=cen-matrix(as.numeric(rep(X[c,],K)),ncol=ncol(X),byrow=T)
    cluster[c]=which.min(apply(D*D,1,sum))
  }
  
  return(list(cluster=cluster,centros=cen))
}



