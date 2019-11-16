source('funciones_shiny.R')
function(input, output, session) {
readable_names=read.csv('info.csv',stringsAsFactors = FALSE)%>%
  rename(etf=Ticker)%>%
  select(etf,Name)%>%
  group_by(etf)%>%
  do(head(.,1))
rv=reactiveValues(
  ports=read.csv('portafolios.csv',stringsAsFactors = FALSE)%>%
    mutate(date=as.Date(date))
)  
  
observeEvent(input$rfs,
             showModal(modalDialog(
               title = "Atención",
               "No está configurada la licencia de bloomberg, se debe trabajar con datos locales.",
               easyClose = TRUE,
               footer = NULL
             ))
             )
  
  
observeEvent(input$create,{
               print('hey')
               rv$ports=actualizar_portafolios()%>%
                 filter(wday(date)==5)
               })

output$options=renderUI({
  fechas=rv$ports%>%
    select(date)%>%
    pull()%>%
    unique()%>%
    sort()%>%
    tail(5)
  
  selectInput('fecha',
              'Fechas',
              fechas,selected = max(fechas))
}
)

output$plot1=renderPlot({
  clus=switch(input$cluster, 
              'Asia Emergente'=1,
              'Europa'=2,
              'USA sectores'=3,
              'Asia Sub'=4,
              'USA core'=5)
  
  rv$ports%>%
    filter(cluster==clus,date<=input$fecha)%>%
    inner_join(readable_names,by='etf')%>%
    group_by(etf)%>%
    arrange(date)%>%
    do(tail(.,2))%>%
    mutate(Semana=factor(c('Anterior','Actual'),levels = c('Anterior','Actual')))%>%
    ungroup()%>%
    ggplot(aes(Name,w,fill=Semana))+
    geom_col(position = position_dodge2())+
    scale_y_continuous(labels = scales::percent)+
  coord_flip()+ 
    ggthemes::theme_hc()+
     scale_fill_manual(values=c("lightgrey", "blue"))+
    labs(title = 'Portafolio propuesto',
         x='ETF',y='')+
    theme(axis.text.y = element_text(size=12,face = 'bold'),
          axis.text.x = element_text(size=20,face = 'bold'),
          legend.text = element_text(size=20),
          legend.title = element_text(size=12),
          plot.title = element_text(size=30,face = 'bold'))
    
  
  
})
    
  
}