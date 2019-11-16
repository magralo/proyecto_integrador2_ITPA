library(tidyverse)
library(TTR)
library(janitor)
library(plotly)
library(caret)
library(lubridate)
library(Rtsne)
library(pROC)
source('funciones.r')



if(stringi::stri_detect(getwd(),fixed='cluster')){
  setwd('..')
}

actualizar_portafolios=function(){
  raw=read.csv('all_info.csv',stringsAsFactors = FALSE)%>%
    clean_names()%>%
    mutate(date=as.Date(date))%>%
    rename(tri=tot_return_index_net_dvds)
  
  
  #### Cluster 1
  
  if(stringi::stri_detect(getwd(),fixed='cluster')){
    setwd('../cluster 1')
  }else{
    setwd('cluster 1')
  }
  
  
  etfs=c(
    'MCHI US Equity', #china
    'EWH US Equity', #hong kong
    'EWY US Equity' #korea
  )
  
  
  k=5
  
  all_pairs=data.frame(combn(unique(etfs),2),stringsAsFactors = FALSE)
  
  all_pairs=lapply(all_pairs, c)
  
  all_tecs=lapply(all_pairs,get_tec,raw=raw,k=k)
  
  pair_tecnical=do.call(rbind,all_tecs)
  
  
  stat=get_strategy(pair_tecnical,y=year(max(raw$date)),yot=5,met='regLogistic')
  
  
  portfolio1=get_portfolio(stat$prediction)%>%
    filter(date>'2019-01-01')
  
  ## Cluster 2
  
  if(stringi::stri_detect(getwd(),fixed='cluster')){
    setwd('../cluster 2')
  }else{
    setwd('cluster 2')
  }
  
  
  etfs=c(
    'EWQ US Equity', #Francia
    'EWG US Equity', #Alemania
    'EWU US Equity' #UK
  )
  
  
  k=5
  
  all_pairs=data.frame(combn(unique(etfs),2),stringsAsFactors = FALSE)
  
  all_pairs=lapply(all_pairs, c)
  
  all_tecs=lapply(all_pairs,get_tec,raw=raw,k=k)
  
  pair_tecnical=do.call(rbind,all_tecs)
  
  
  stat=get_strategy(pair_tecnical,y=year(max(raw$date)),yot=5,met='regLogistic')
  
  
  portfolio2=get_portfolio(stat$prediction)%>%
    filter(date>'2019-01-01')
  
  
  ## Cluster 3
  
  if(stringi::stri_detect(getwd(),fixed='cluster')){
    setwd('../cluster 3')
  }else{
    setwd('cluster 3')
  }
  
  
  etfs=c(
    'XLI US Equity', #Industrial
    'XLB US Equity', #Material
    'XLF US Equity',#Financial
    'XLE US Equity'#Energi
  )
  
  
  k=5
  
  all_pairs=data.frame(combn(unique(etfs),2),stringsAsFactors = FALSE)
  
  all_pairs=lapply(all_pairs, c)
  
  all_tecs=lapply(all_pairs,get_tec,raw=raw,k=k)
  
  pair_tecnical=do.call(rbind,all_tecs)
  
  
  stat=get_strategy(pair_tecnical,y=year(max(raw$date)),yot=5,met='regLogistic')
  
  
  portfolio3=get_portfolio(stat$prediction)%>%
    filter(date>'2019-01-01')
  
  ## Cluster 4
  
  if(stringi::stri_detect(getwd(),fixed='cluster')){
    setwd('../cluster 4')
  }else{
    setwd('cluster 4')
  }
  
  
  etfs=c(
    'INDA US Equity', #India
    'THD US Equity', #Tailandia
    'EWM US Equity'# Malasya
  )
  
  
  k=5
  
  all_pairs=data.frame(combn(unique(etfs),2),stringsAsFactors = FALSE)
  
  all_pairs=lapply(all_pairs, c)
  
  all_tecs=lapply(all_pairs,get_tec,raw=raw,k=k)
  
  pair_tecnical=do.call(rbind,all_tecs)
  
  
  stat=get_strategy(pair_tecnical,y=year(max(raw$date)),yot=5,met='regLogistic')
  
  
  portfolio4=get_portfolio(stat$prediction)%>%
    filter(date>'2019-01-01')
  
  ## Cluster 5
  
  if(stringi::stri_detect(getwd(),fixed='cluster')){
    setwd('../cluster 5')
  }else{
    setwd('cluster 5')
  }
  
  
  etfs=c(
    'SPY US Equity', #US general
    'XLK US Equity', #Tech
    'XLY US Equity'#Cons Disc
  )
  
  
  k=5
  
  all_pairs=data.frame(combn(unique(etfs),2),stringsAsFactors = FALSE)
  
  all_pairs=lapply(all_pairs, c)
  
  all_tecs=lapply(all_pairs,get_tec,raw=raw,k=k)
  
  pair_tecnical=do.call(rbind,all_tecs)
  
  
  stat=get_strategy(pair_tecnical,y=year(max(raw$date)),yot=5,met='regLogistic')
  
  
  portfolio5=get_portfolio(stat$prediction)%>%
    filter(date>'2019-01-01')
  
  if(stringi::stri_detect(getwd(),fixed='cluster')){
    setwd('..')
  }
  #### resumen
  
  portafolios=portfolio1%>%
    mutate(cluster=1)%>%
    rbind(mutate(portfolio2,cluster=2))%>%
    rbind(mutate(portfolio3,cluster=3))%>%
    rbind(mutate(portfolio4,cluster=4))%>%
    rbind(mutate(portfolio5,cluster=5))
  return(portafolios)

}

#write.csv(portafolios,'portafolios.csv')

