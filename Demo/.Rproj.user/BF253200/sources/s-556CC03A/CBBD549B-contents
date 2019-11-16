put_label=function(index,k=7){
  ## Function to put the label at each date by looking k days into the future
  roc=ROC(index,n=k,type = 'discrete')
  roc=roc[!is.na(roc)]
  roc=c(roc,rep(NA,k))
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





get_year_model_and_pred=function(pair_tecnical,y=2019,yot=3,met='knn'){
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
  
  test=pair_tecnical%>%
    filter(date>=cut,date<sco)
  
  model=get_model(training,met=met)
  
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




show_results=function(strategy_and_model){
  models=list()
  strategy=list()
  for (y in 1:length(years)){
    models[[y]]=strategy_and_model[[y]]$model
    strategy[[y]]=strategy_and_model[[y]]$prediction
  }
  
  
  strategy=do.call(rbind,strategy)%>%
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
    mutate(w=w/sum(w))
  
  
  
  
  retornos_tidy_1a=raw%>%#el retorno de la semana siguiente
    dplyr::select(date,etf,tri)%>%
    filter(wday(date)==5)%>%
    group_by(etf)%>%
    mutate(ret=c(ROC(tri,n=1,type = 'discrete')[-1],NA))%>%
    ungroup()%>%
    dplyr::select(date,etf,ret)
  
  retorno_portafolio=portfolio_tidy%>%
    inner_join(retornos_tidy_1a,by=c('etf','date'))%>%
    mutate(ret=ifelse(is.na(ret),0,ret))%>%
    group_by(date)%>%
    summarise(ret_stat=sum(w*ret),ret_ew=mean(ret))%>%
    ungroup()%>%
    mutate(stat=cumprod(1+ret_stat),ew=cumprod(1+ret_ew))
  
  
  
  gbar=portfolio_tidy%>%
    left_join(retornos_tidy_1a,by=c('etf','date'))%>%
    mutate(ret=ifelse(is.na(ret),0,ret))%>%
    group_by(date)%>%
    summarise(ret_stat=sum(w*ret),ret_ew=mean(ret))%>%
    ungroup()%>%
    mutate(y=year(date))%>%
    group_by(y)%>%
    summarise(stat=prod(1+ret_stat)-1,ew=prod(1+ret_ew)-1)%>%
    ungroup()%>%
    gather(stat,ret,-y)%>%
    plot_ly(x=~y,y=~ret,color=~stat,type='bar')%>%
    layout(title="Retornos Anuales")
  
  gtime=retorno_portafolio%>%
    dplyr::select(date,stat,ew)%>%
    gather(strategy,index,-date)%>%
    plot_ly(x=~date,y=~index,color=~strategy,mode='lines')%>%
    layout(title="Backtest de la estrategia")
  
  
  
  con1=strategy%>%
    count(expected,label)%>%
    group_by(expected)%>%
    mutate(p=n/sum(n))%>%
    ungroup()%>%
    mutate(label=fct_rev(label))%>%
    ggplot(aes(label,expected,fill=p,label=round(p*100,3)))+
    geom_tile()+geom_text()+
    ggtitle('Tasas de acierto')
  
  
  con2=strategy%>%
    mutate(expected=ifelse(prob<0.4,'act_2',ifelse(prob>0.6,'act_1','no des')))%>%
    count(expected,label)%>%
    group_by(expected)%>%
    mutate(p=n/sum(n))%>%
    ungroup()%>%
    mutate(label=fct_rev(label))%>%
    ggplot(aes(label,expected,fill=p,label=round(p*100,3)))+
    geom_tile()+geom_text()+
    ggtitle('Tasa de acierto con indiferencia entre el% 40 y 60%')
  
  #groc=roc(strategy$label=='act_1',strategy$prob,
  #smoothed = TRUE,
  ## arguments for ci
  #ci=TRUE, ci.alpha=0.9, stratified=FALSE,
  ## arguments for plot
  #plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
  #print.auc=TRUE, show.thres=TRUE)
  
  
  
  cm = as.matrix(table(strategy$label, strategy$expected))
  
  
  accuracy=sum(diag(cm))/sum(colSums(cm))
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  
  ktable=data.frame(positive_class=names(precision),
                    Precision=precision,
                    Recall=recall,
                    Accuracy=accuracy)%>%
    knitr::kable(row.names = FALSE)
  
  return(list(gtime=gtime,gbar=gbar,
              groc=NULL,
              ktable=ktable,
              con1=con1,con2=con2,
              data=retorno_portafolio))
  
}


show_resultsP=function(file){
  
  

  strategy = read_csv(file)%>%
    rename(prob=Pred)%>%
    mutate(prob=1-prob)%>%
    mutate(expected=factor(ifelse(prob>0.5,'act_1','act_2')),
           label=factor(ifelse(Real>0.5,'act_1','act_2')))%>%
    select(-Real)
  
  
  
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
    mutate(w=w/sum(w))
  
  
  
  
  retornos_tidy_1a=raw%>%#el retorno de la semana siguiente
    dplyr::select(date,etf,tri)%>%
    filter(wday(date)==5)%>%
    group_by(etf)%>%
    mutate(ret=c(ROC(tri,n=1,type = 'discrete')[-1],NA))%>%
    ungroup()%>%
    dplyr::select(date,etf,ret)
  
  retorno_portafolio=portfolio_tidy%>%
    inner_join(retornos_tidy_1a,by=c('etf','date'))%>%
    mutate(ret=ifelse(is.na(ret),0,ret))%>%
    group_by(date)%>%
    summarise(ret_stat=sum(w*ret),ret_ew=mean(ret))%>%
    ungroup()%>%
    mutate(stat=cumprod(1+ret_stat),ew=cumprod(1+ret_ew))
  
  
  
  gbar=portfolio_tidy%>%
    left_join(retornos_tidy_1a,by=c('etf','date'))%>%
    mutate(ret=ifelse(is.na(ret),0,ret))%>%
    group_by(date)%>%
    summarise(ret_stat=sum(w*ret),ret_ew=mean(ret))%>%
    ungroup()%>%
    mutate(y=year(date))%>%
    group_by(y)%>%
    summarise(stat=prod(1+ret_stat)-1,ew=prod(1+ret_ew)-1)%>%
    ungroup()%>%
    gather(stat,ret,-y)%>%
    plot_ly(x=~y,y=~ret,color=~stat,type='bar')%>%
    layout(title="Retornos Anuales")
  
  gtime=retorno_portafolio%>%
    dplyr::select(date,stat,ew)%>%
    gather(strategy,index,-date)%>%
    plot_ly(x=~date,y=~index,color=~strategy,mode='lines')%>%
    layout(title="Backtest de la estrategia")
  
  
  
  con1=strategy%>%
    count(expected,label)%>%
    group_by(expected)%>%
    mutate(p=n/sum(n))%>%
    ungroup()%>%
    mutate(label=fct_rev(label))%>%
    ggplot(aes(label,expected,fill=p,label=round(p*100,3)))+
    geom_tile()+geom_text()+
    ggtitle('Tasas de acierto')
  
  
  con2=strategy%>%
    mutate(expected=ifelse(prob<0.4,'act_2',ifelse(prob>0.6,'act_1','no des')))%>%
    count(expected,label)%>%
    group_by(expected)%>%
    mutate(p=n/sum(n))%>%
    ungroup()%>%
    mutate(label=fct_rev(label))%>%
    ggplot(aes(label,expected,fill=p,label=round(p*100,3)))+
    geom_tile()+geom_text()+
    ggtitle('Tasa de acierto con indiferencia entre el% 40 y 60%')
  
  #groc=roc(strategy$label=='act_1',strategy$prob,
  #smoothed = TRUE,
  ## arguments for ci
  #ci=TRUE, ci.alpha=0.9, stratified=FALSE,
  ## arguments for plot
  #plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
  #print.auc=TRUE, show.thres=TRUE)
  
  
  
  cm = as.matrix(table(strategy$label, strategy$expected))
  
  
  accuracy=sum(diag(cm))/sum(colSums(cm))
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  
  ktable=data.frame(positive_class=names(precision),
                    Precision=precision,
                    Recall=recall,
                    Accuracy=accuracy)%>%
    knitr::kable(row.names = FALSE)
  
  return(list(gtime=gtime,gbar=gbar,
              groc=NULL,
              ktable=ktable,
              con1=con1,con2=con2,
              data=retorno_portafolio))
  
}


