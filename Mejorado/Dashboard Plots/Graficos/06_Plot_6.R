library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(plotly) # Graficos interactivos
rm(list=ls())
#-----------#
# GRAFICO 3 #
#-----------#

# Evolución número de estudiantes
load('./Mejorado/Querys/010_Query.RData')

b <- `Resumen de estudiantes por programas final` #%>% 
  # Departamento==input$Departamento_estudiantes
  #filter(Departamento=='Finanzas')
#--------------------------------------------------
#--------------------------------------------------
per_sem_b <- paste0(b$Periodo,"-0",ifelse(b$Semestre==1,1,6),"-01") # Periodo y semestre
b$date <- per_sem_b

if(unique(b$Departamento)=="Postgrados"){
  fun_feature1<-function(x){
    nPalabras <- sapply(strsplit(x, " "), length)
    if(nPalabras>3){
      p1 <- word(x,1,3)
      if(nPalabras>6){
        p2 <- word(x,4,6)
        p3 <- word(x,7,nPalabras)
        p <- paste(p1,"<br>",p2,"<br>",p3)
      }else{p2 <- word(x,4,nPalabras)
      p <- paste(p1,"<br>",p2)}
    }else{p <- x}
    return(p)
  }
  b$Programa <- sapply(b$Programa,fun_feature1)
}

c <- b %>%
  group_by(Periodo,Semestre) %>% 
  summarise(TotalPS = sum(Total))# PS: Periodo-Semestre

d <- c %>% 
  merge(b,.,by=c('Periodo','Semestre'), all.x = TRUE)

b <- d %>% 
  mutate('%Global'=Total/TotalPS) %>% 
  filter(Departamento=='Finanzas')
plot_ly(b,  type = 'scatter', mode = 'lines+markers',marker = list(symbol = "x",size = 10)) %>%
  add_text(x = ~date, y = ~`%Global`,split = ~Programa,text=~paste0(round(`%Global`,3)*100,"%"),textposition = "top",showlegend=FALSE) %>% 
  add_trace(x = ~date, y = ~`%Global`,split = ~Programa) %>% 
  layout(
    xaxis = list(title="Evolucion de estudiantes",
                 ticktext = list("2016-02", "2017-01", "2017-02", "2018-01", "2018-02",
                                 "2019-01","2019-02","2020-01","2020-02","2021-01","2021-02"), 
                 tickvals = list("2016-06-01", "2017-01-01", "2017-06-01", "2018-01-01", "2018-06-01",
                                 "2019-01-01","2019-06-01","2020-01-01","2020-06-01","2021-01-01","2021-06-01"),
                 tickmode = "array"
    ),
    yaxis = list(title="Aporte total (%)"))


