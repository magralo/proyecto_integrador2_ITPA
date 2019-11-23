## Librerias
library(tidyverse)
library(lubridate)
library(janitor)
library(TTR)
library(plotly)
##Parametros
TD=20
p_val=0.2

etfs=c(
  'SPY US Equity', #US general
  'INDA US Equity',#India
  'IEV US Equity',#Francia
  'XLF US Equity',#Financial
  'MCHI US Equity'#China
)### Selecciono etfs a usar



etfs=c(
  'XLI US Equity', #Industrial
  'XLB US Equity', #Material
  'XLF US Equity',#Financial
  'XLE US Equity'#Energi
)
etfs=c(
  'MCHI US Equity', #china
  'EWH US Equity', #hong kong
  'EWY US Equity' #korea
)

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
    mutate(r1 = c(ROC(etf1,n=1,type = 'discrete')[-1],NA),
           r2= c(ROC(etf2,n=1,type = 'discrete')[-1],NA))%>%
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

get_strategy=function(df,dates,delta,pv=0.3){
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



## funcional
raw=read.csv('all_info.csv',stringsAsFactors = FALSE)%>%
  clean_names()%>%
  mutate(date=as.Date(date))%>%
  rename(tri=tot_return_index_net_dvds) ### Leo todo







# etfs=raw%>%
#   group_by(etf)%>%
#   summarise(d=min(date))%>%
#   ungroup()%>%
#   filter(year(d)<2011)%>%
#   select(etf)%>%
#   pull()


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

#strategy=pair_ret%>%
#  mutate(m=month(date),y=year(date))%>%
#  group_by(pair,m,y)%>%
#  summarise(g=game(r1,r2,pv=p_val),date=max(date))%>%
#  ungroup() #### modficar

strategy=pair_ret%>%
  group_by(pair)%>%
  do(get_strategy(.,meses,TD,pv=p_val))%>%
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


portfolio_tidy%>%
  left_join(retornos_tidy_1m,by=c('etf','date'))%>%
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

retorno_portafolio%>%
  dplyr::select(date,stat,ew)%>%
  gather(strategy,index,-date)%>%
  plot_ly(x=~date,y=~index,color=~strategy,mode='lines')%>%
  layout(title="Backtest de la estrategia")

  
