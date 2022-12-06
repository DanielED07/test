library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(plotly) # Graficos interactivos
rm(list=ls())
#-----------#
# GRAFICO 8 #
#-----------#

# % E.I&R : Evolucion porcentaje de informes y propuestas reportados
load('./Mejorado/Querys/012_Query.RData')

#-------------#
# I: Informes #
#-------------#

k <- "programas"
u <- "I" # Informes
z <- get(as.name(paste("Resumen de trabajos de grado por",k,  "final"))) %>% 
  mutate_at(names(.)[-c(1,2)],as.numeric) %>% 
  select(c("Total","Departamento","Programa","Periodo","Semestre",names(.)[grepl(u,names(.) , fixed = TRUE)])) %>% 
  # Departamento==input$Departamento
  filter(Departamento=='Finanzas') %>% 
  mutate(TOTAL = rowSums(across(`(IFCM)`:`(IFR)`))) %>% 
  mutate(.,across(`(IFCM)`:`(IFR)`,~.x/TOTAL))

per_sem_I <- paste0(z$Periodo,"-0",ifelse(z$Semestre=='1',1,6),"-01") # Periodo y semestre, ticks para el plot
z$date <- per_sem_I
z$`(IFCM)` <- ifelse(is.na(z$`(IFCM)`),0,z$`(IFCM)`)
z$`(IFSM)` <- ifelse(is.na(z$`(IFSM)`),0,z$`(IFSM)`)
z$`(IFR)` <- ifelse(is.na(z$`(IFR)`),0,z$`(IFR)`)

x <- z %>% 
  select(-c(Departamento,Periodo,Semestre,Total,TOTAL)) %>%
  # Programa==input$Programa
  filter(Programa=='Ingenieria Financiera Y De Negocios') %>% 
  mutate(.,across(`(IFCM)`:`(IFR)`,~round(.x,4))) %>% 
  gather(key="Recomendacion", value = "Porcentaje",-c('date','Programa'))

plot_ly(x,  type = 'scatter', mode = 'lines+markers',marker = list(symbol = "x",size = 10)) %>%
  add_text(x = ~date, y = ~Porcentaje,split = ~Recomendacion,text=~paste0(round(Porcentaje,3)*100,"%"),textposition = "top",showlegend=FALSE) %>% 
  add_trace(x = ~date, y = ~Porcentaje,split = ~Recomendacion) %>% 
  layout(
    xaxis = list(
      ticktext = list("2016-02", "2017-01", "2017-02", "2018-01", "2018-02",
                      "2019-01","2019-02","2020-01","2020-02","2021-01","2021-02",
                      "2022-01","2022-02"), 
      tickvals = list("2016-06-01", "2017-01-01", "2017-06-01", "2018-01-01", "2018-06-01",
                      "2019-01-01","2019-06-01","2020-01-01","2020-06-01","2021-01-01","2021-06-01",
                      "2022-01-01","2022-06-01"),
      tickmode = "array"
    ),
    yaxis = list(title="%"))

#------------------------------------------------------------------
#---------------#
# P: Propuestas #
#---------------#

k <- "programas"
u <- "P" # Propuestas
zz <- get(as.name(paste("Resumen de trabajos de grado por",k,  "final"))) %>% 
  mutate_at(names(.)[-c(1,2)],as.numeric) %>% 
  select(c("Total","Departamento","Programa","Periodo","Semestre",names(.)[grepl(u,names(.) , fixed = TRUE)])) %>% 
  # Departamento==input$Departamento
  filter(Departamento=='Finanzas') %>% 
  mutate(TOTAL = rowSums(across(`(PCM)`:`(PR)`))) %>% 
  mutate(.,across(`(PCM)`:`(PR)`,~.x/TOTAL))

per_sem_P <- paste0(zz$Periodo,"-0",ifelse(zz$Semestre=='1',1,6),"-01") # Periodo y semestre, ticks para el plot
zz$date <- per_sem_P
zz$`(PCM)` <- ifelse(is.na(zz$`(PCM)`),0,zz$`(PCM)`)
zz$`(PSM)` <- ifelse(is.na(zz$`(PSM)`),0,zz$`(PSM)`)
zz$`(PR)` <- ifelse(is.na(zz$`(PR)`),0,zz$`(PR)`)

y <- zz %>% 
  select(-c(Departamento,Periodo,Semestre,Total,TOTAL)) %>%
  # Programa==input$Programa
  filter(Programa=='Ingenieria Financiera Y De Negocios') %>% 
  mutate(.,across(`(PCM)`:`(PR)`,~round(.x,4))) %>% 
  gather(key="Recomendacion", value = "Porcentaje",-c('date','Programa'))

plot_ly(y,  type = 'scatter', mode = 'lines+markers',marker = list(symbol = "x",size = 10)) %>%
  add_text(x = ~date, y = ~Porcentaje,split = ~Recomendacion,text=~paste0(round(Porcentaje,3)*100,"%"),textposition = "top",showlegend=FALSE) %>% 
  add_trace(x = ~date, y = ~Porcentaje,split = ~Recomendacion) %>% 
  layout(
    xaxis = list(
      ticktext = list("2016-02", "2017-01", "2017-02", "2018-01", "2018-02",
                      "2019-01","2019-02","2020-01","2020-02","2021-01","2021-02",
                      "2022-01","2022-02"), 
      tickvals = list("2016-06-01", "2017-01-01", "2017-06-01", "2018-01-01", "2018-06-01",
                      "2019-01-01","2019-06-01","2020-01-01","2020-06-01","2021-01-01","2021-06-01",
                      "2022-01-01","2022-06-01"),
      tickmode = "array"
    ),
    yaxis = list(title="%"))
