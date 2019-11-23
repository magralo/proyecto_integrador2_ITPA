## Librerias
library(tidyverse)
library(lubridate)
library(janitor)
library(TTR)
library(plotly)
##Parametros
TD=20
p_val=0.3


etfs=c(
  'MCHI US Equity', #china
  'EWH US Equity', #hong kong
  'EWY US Equity' #korea
)


etfs=c(
  'SPY US Equity', #US general
  'INDA US Equity',#India
  'IEV US Equity',#Francia
  'XLF US Equity',#Financial
  'MCHI US Equity'#China
)### Selecciono etfs a usar



## Funciones

get_pair_ret=function(raw,etfs){
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
    dplyr::select(date,etf1,etf2)%>%
    mutate(pair=paste0(etfs[1],'_',etfs[2]))
  
  pair_tecnical=pair%>%
    mutate(r1 = c(NA,ROC(etf1,n=1,type = 'discrete')[-1]),
           r2= c(NA,ROC(etf2,n=1,type = 'discrete')[-1]))%>%
    filter(!is.na(r1))%>%
    dplyr::select(pair,date,r1,r2)
  
  
  return(pair_tecnical)
}

game=function(v1,v2, pv=0.3){
  a=wilcox.test(v1,v2,alternative = "greater",paired = TRUE)
  if (a$p.value<pv){
    return(1)
  }
  a=wilcox.test(v2,v1,alternative = "greater",paired = TRUE)
  if (a$p.value<pv){
    return(3)
  }
  return(2)
}

game2=function(v1,v2, pv=0.3){
  
  if(median(v1)>median(v2)){
    return(1)
  }else{
    return(3)
  }
  
  return(2)
}


game3=function(v1,v2, pv=0.3){
  
  if(mean(v1)>mean(v2)){
    return(1)
  }else{
    return(3)
  }
  
  return(2)
}

get_strategy=function(df,dates,delta,game,pv=0.3){
  res=data.frame()
  
  for (d in dates){
    res=df%>%
      filter(date<=d)%>%
      arrange(date)%>%
      do(tail(.,delta))%>%### me coge los ultimo delta datos
      summarise(g=game(r1,r2,pv),date=max(date))%>%
      rbind(res)
    
  }
  
  return(res)
}

get_bt=function(raw,etfs,TD,p_val,game_fun){
  
  game=game_fun
  
  all_pairs=data.frame(combn(unique(etfs),2),stringsAsFactors = FALSE)
  
  all_pairs=lapply(all_pairs, c) ### Estas dos lineas me crea todas la parejas de los etfs
  
  all_tecs=lapply(all_pairs,get_pair_ret,raw=raw) #### traer dataframe con retornos por pareja
  
  pair_ret=do.call(rbind,all_tecs) ##hago append en un solo dataframe
  
  aux=pair_ret%>%
    group_by(pair)%>%
    summarise(d=min(date))%>%
    ungroup()%>%
    summarise(max(d))%>%
    pull()### ver fecha desde cuando tengo info de todos
  
  
  
  pair_ret=pair_ret%>%
    filter(date>"2010-01-01")%>%
    filter(date>=aux) ## filtro desde la fecha que quiero
  
  
  meses=pair_ret%>%
    mutate(m=week(date),y=year(date))%>%
    group_by(m,y)%>%
    summarise(date=max(date))%>%
    ungroup()%>%
    select(date)%>%
    arrange(date)%>%
    pull()%>%
    unique()
  
  meses=meses[-(1:3)]
  
  
  strategy=pair_ret%>%
    group_by(pair)%>%
    do(get_strategy(.,meses,TD,game_fun,pv=p_val))%>%
    ungroup() #### modficar
  
  portfolio=strategy%>%
    separate(pair,c('etf1','etf2'),'_')%>%
    mutate(vote1=ifelse(g==1,2,ifelse(g==3,0,1)),
           vote2=ifelse(g==3,2,ifelse(g==1,0,1)))%>%
    group_by(etf1,date)%>%
    summarise(votos=sum(vote1))%>%
    ungroup()%>%
    rename(etf=etf1)
  
  portfolio_tidy=strategy%>%
    separate(pair,c('etf1','etf2'),'_')%>%
    mutate(vote1=ifelse(g==1,2,ifelse(g==3,0,1)),
           vote2=ifelse(g==3,2,ifelse(g==1,0,1)))%>%
    group_by(etf2,date)%>%
    summarise(votos=sum(vote2))%>%
    ungroup()%>%
    rename(etf=etf2)%>%
    rbind(portfolio)%>%
    group_by(etf,date)%>%
    summarise(votos=sum(votos))%>%
    ungroup()%>%
    group_by(date)%>%
    mutate(w=votos/sum(votos))
  
  portfolio_wide=portfolio_tidy%>%
    select(date,etf,w)%>%
    spread(key = etf,value=w,fill=0) ## Check
  
  
  retornos_tidy_1m=raw%>%#el retorno del periodo siguiente
    dplyr::select(date,etf,tri)%>%
    filter(date%in%portfolio_tidy$date)%>%
    group_by(etf)%>%
    mutate(ret=c(ROC(tri,n=1,type = 'discrete')[-1],NA))%>%
    ungroup()%>%
    dplyr::select(date,etf,ret)
  
  
  retorno_portafolio=portfolio_tidy%>%
    inner_join(retornos_tidy_1m,by=c('etf','date'))%>%
    mutate(ret=ifelse(is.na(ret),0,ret))%>%
    group_by(date)%>%
    summarise(ret_stat=sum(w*ret),ret_ew=mean(ret))%>%
    ungroup()%>%
    mutate(stat=cumprod(1+ret_stat),ew=cumprod(1+ret_ew))  
  
  ret_per=retorno_portafolio%>%
    select(date,ret_stat)
  
  retorno_final=portfolio_tidy%>%
    inner_join(retornos_tidy_1m,by=c('etf','date'))%>%
    inner_join(ret_per,by=c('date'))%>%
    mutate(wf=w*(1+ret)/(1+ret_stat))%>%
    group_by(etf)%>%
    mutate(dw=delta_w(wf,w))%>%
    ungroup()%>%
    group_by(date)%>%
    summarise(ret_stat=sum(w*ret)-0.0001*sum(dw),ret_ew=mean(ret))%>%
    ungroup()%>%
    mutate(stat=cumprod(1+ret_stat),ew=cumprod(1+ret_ew)) 
  
  return(retorno_final)
}



## funcional
raw=read.csv('all_info.csv',stringsAsFactors = FALSE)%>%
  clean_names()%>%
  mutate(date=as.Date(date))%>%
  rename(tri=tot_return_index_net_dvds) ### Leo todo



wilcon=get_bt(raw,etfs,20,p_val=0.3,game)%>%
  select(date,stat,ew)%>%
  rename(Wilcox=stat)


mediana=get_bt(raw,etfs,20,p_val=0.3,game2)%>%
  select(date,stat,ew)%>%
  rename(Mediana=stat)

media=get_bt(raw,etfs,20,p_val=0.3,game3)%>%
  select(date,stat)%>%
  rename(Media=stat)


gather(wilcon,strategy,index,-date)%>%
  rbind(gather(mediana,strategy,index,-date))%>%
  rbind(gather(media,strategy,index,-date))%>%
  plot_ly(x=~date,y=~index,color=~strategy,mode='lines')%>%
  layout(title="Backtest de la estrategia")


wilconR=get_bt(raw,etfs,20,p_val=0.3,game)%>%
  select(date,ret_stat,ret_ew)%>%
  rename(Wilcox=ret_stat)


medianaR=get_bt(raw,etfs,20,p_val=0.3,game2)%>%
  select(date,ret_stat)%>%
  rename(Mediana=ret_stat)

mediaR=get_bt(raw,etfs,20,p_val=0.3,game3)%>%
  select(date,ret_stat)%>%
  rename(Media=ret_stat)

all=gather(wilconR,strategy,index,-date)%>%
  rbind(gather(medianaR,strategy,index,-date))%>%
  rbind(gather(mediaR,strategy,index,-date))

bm=all%>%
  filter(strategy=='ret_ew')%>%
  rename(bmi=index)%>%
  select(date,bmi)


all%>%
  filter(strategy!='ret_ew')%>%
  inner_join(bm,by='date')%>%
  mutate(alpha=index-bmi)%>%
  filter(!is.na(alpha))%>%
  group_by(strategy)%>%
  summarise(alpha_mean=mean(alpha),
            alpha_median=median(alpha),
            IR=alpha_mean/sd(alpha))%>%
  ggplot(aes(strategy,IR,fill=strategy))+geom_col()



