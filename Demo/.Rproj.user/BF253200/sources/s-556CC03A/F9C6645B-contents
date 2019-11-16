#### Functions

my_prcomp=function(data,method='pearson'){
  center=apply(data,2,median)
  scale=apply(data,2,sd)
  
  m=matrix(rep(pcao$center,nrow(data)),ncol=ncol(data),byrow = TRUE)
  s=matrix(rep(pcao$scale,nrow(data)),ncol=ncol(data),byrow = TRUE)
  
  data_c=(data-m)/s
  
  covm=cov(data_c,method = method)
  
  eigo=eigen(covm)
  
  sdev=sqrt(eigo$values)
  
  rotation=eigo$vectors
  
  colnames(rotation)=paste0('PC',1:length(sdev))
  
  lista=list(center=center,
             scale=scale,
             rotation=rotation,
             sdev=sdev)
  
}



put_label=function(index,k=7){
  ## Function to put the label at each date by looking k days into the future
  roc=ROC(index,n=k,type = 'discrete')
  roc=roc[!is.na(roc)]
  roc=c(roc,rep(0,k))
  return(roc)
}


get_tec=function(raw,etfs,k=7){
  ## Function to get addition ino from te price using technicals
  etf1=raw%>%
    filter(etf==etfs[1])%>%
    dplyr::select(date,tri)%>%
    rename(etf1=tri)
  etf2=raw%>%
    filter(etf==etfs[2])%>%
    dplyr::select(date,tri)%>%
    rename(etf2=tri)
  
  pair=etf1%>%
    inner_join(etf2,by='date')%>%
    mutate(pair_index=etf1/etf2)%>%
    dplyr::select(date,pair_index)%>%
    mutate(pair=paste0(etfs[1],'_',etfs[2]))
  
  
  
  
  
  pair_tecnical=pair%>%
    mutate(sma14     = SMA(pair_index,n=14)/pair_index,
           sma50     = SMA(pair_index,n=50)/pair_index,
           sma200    = SMA(pair_index,n=200)/pair_index,
           c14vs50   = sma14/sma50,
           c14vs200  = sma14/sma200,
           c50vs200  = sma50/sma200,
           ema       = EMA(pair_index,n=14)/pair_index,
           momentum  = momentum(pair_index,n=2),
           macd      = MACD(pair_index, nFast=12, nSlow=26,
                            nSig=9, maType=SMA)[,2],
           rsi       = RSI(pair_index, n=14),
           cci       = CCI(pair_index, n = 20),
           vhf       = VHF(pair_index, n = 28),
           label     = put_label(pair_index,k=k))%>%
    filter(!is.na(c50vs200),!is.na(label))%>%
    dplyr::select(pair,date,label,pair_index,
                  sma14,sma50,sma200,
                  c14vs50,c50vs200,c14vs200,
                  ema,momentum,
                  macd,cci,vhf,rsi)%>%
    mutate(label=factor(ifelse(label>0,'act_1','act_2')))
  return(pair_tecnical)
}


get_model=function(training,met='knn'){
  ## Funtion to train a model based on a traing set and a type of model i.e knn,glm,rf
  
  
  
  if (met=='rf'){
    if(ncol(training)>10){
      grid <-  expand.grid(mtry=seq(2,10,2))
    }else{
      grid <-  expand.grid(mtry=seq(1,ncol(training)-1,1))
    }
    
    
  }else if(met=='mlpKerasDropout'){
    
    
    
    
  }else if(met=='svmRadial'){
    
    grid <-  expand.grid(C=c(0.25,0.5,1),
                         sigma=c(0.1))
    
  }else if(met=='rpart2'){
    
    
    
    if(ncol(training)>10){
      grid <-  expand.grid(maxdepth=seq(1,10,2))
    }else{
      grid <-  expand.grid(maxdepth=seq(1,5,1))
    }
    
  }else if (met=='regLogistic'){
    
    grid <-  expand.grid( cost= c(0.5,0.75,1,2),
                          loss='L1'  ,
                          epsilon=c(0.1,0.01))
    
  }else(
    print('ojo, no tengo parametros')
  )
  
  fitControl <- trainControl(## 5-fold CV repeted 5 times
    method = "repeatedcv",
    number = 5,
    repeats = 5,
    classProbs = TRUE)
  
  if(met=='mlpKerasDropout'){
    model <- train(label ~ ., data = training, 
                   method = met, 
                   trControl = fitControl,
                   preProcess=c("center", "scale"),
                   tuneGrid = grid,
                   verbose=FALSE)
  }else{
    model <- train(label ~ ., data = training, 
                   method = met, 
                   trControl = fitControl,
                   tuneGrid = grid,
                   preProcess=c("center", "scale"))
  }
  
  return(model)
}





get_strategy=function(pair_tecnical,y=2019,yot=3,met='knn'){
  ## Function to return the active model for each year and the predicted labels (to be used as test)
  
  ini=as.Date('2010-01-01')
  year(ini)=y-yot
  cut=ini
  year(cut)=year(ini)+yot
  sco=ini
  year(sco)=year(ini)+yot+1
  
  training=pair_tecnical%>%
    filter(date<cut,date>=ini)%>%
    dplyr::select(-date,-pair_index,-pair)
  
  X=training%>%
    select(-label)
  
  
  
  pcao=prcomp(X, center = TRUE,scale. = TRUE)
  
  cumvar=cumsum(pcao$sdev)/sum(pcao$sdev)
  no_comp=which(cumvar>0.9)[1]
  
  trans=pcao$rotation[,1:no_comp]
  
  m=matrix(rep(pcao$center,nrow(X)),ncol=ncol(X),byrow = TRUE)
  s=matrix(rep(pcao$scale,nrow(X)),ncol=ncol(X),byrow = TRUE)
  
  data_trans=as.matrix(((X-m)/s))%*%as.matrix(trans)
  
  
  training=training%>%
    select(label)%>%
    cbind(data_trans)
  
  test=pair_tecnical%>%
    filter(date>=cut,date<sco)
  
  
  X_test=test%>%
    select(colnames(X))
  
  m=matrix(rep(pcao$center,nrow(X_test)),ncol=ncol(X_test),byrow = TRUE)
  s=matrix(rep(pcao$scale,nrow(X_test)),ncol=ncol(X_test),byrow = TRUE)
  
  data_trans=as.matrix(((X_test-m)/s))%*%as.matrix(trans)
  
  test=test%>%
    cbind(data_trans)
  
  name = paste0(y,"final_model.rds")
  if(file.exists(name)){
    model=readRDS(name)
  }else{
    model=get_model(training,met=met)
    saveRDS(model, name)
  }
  
  prediction=test%>%
    mutate(expected=predict(model,.,type = "prob")[,1])%>%
    dplyr::select(date,pair,label,expected)
  
  return(list(model=model,prediction=prediction))
}


multiple_vote=function(prob,etf1,etf2){
  ## Function to vote double if sure but make no real decision if prob between 40% and 60%
  vote=etf1
  for (p in 1:length(prob)){
    pp=prob[p]
    if(pp>0.5){
      vote[p]=etf1[p] #normal
    }else{
      vote[p]=etf2[p]
    }
    if(abs(pp-0.5)<0.1){
      if(pp>0.5){
        vote[p]=etf2[p] #contrario
      }else{
        vote[p]=etf1[p] #contrario
      }
    }
  }
  return(vote)
}




get_portfolio=function(strategy){

  strategy=strategy%>%
    mutate(prob=expected,expected=factor(ifelse(expected>0.5,'act_1','act_2')))
  
  portfolio=strategy%>%
    separate(pair,c('etf1','etf2'),'_')%>%
    mutate(vote=ifelse(prob>0.5,etf1,etf2),
           vote2=multiple_vote(prob,etf1,etf2))
  
  portfolio_base=portfolio%>%
    dplyr::select(vote,date,etf1,etf2)
  portfolio_conf=portfolio%>%
    dplyr::select(vote2,date,etf1,etf2)%>%
    rename(vote=vote2)
  
  
  portfolio=portfolio_base%>%
    rbind(portfolio_conf)%>%
    count(date,vote)%>%
    spread(key=vote,value=n,fill=0)
  
  portfolio_tidy=portfolio%>%
    gather(etf,w,-date)%>%
    group_by(date)%>%
    mutate(w=w/sum(w))%>%
    ungroup()
  
  
  return(portfolio_tidy)
  
}
