library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(plotly) # Graficos interactivos
rm(list=ls())
#-----------#
# GRAFICO 7 #
#-----------#

# % I%R : Porcentaje de informes y propuestas reportados
load('./Mejorado/Querys/012_Query.RData')

#-------------#
# I: Informes #
#-------------#
k <- "programas"
u <- "I" # Informes
p <- get(as.name(paste("Resumen de trabajos de grado por",k,  "final"))) %>% 
  mutate_at(names(.)[-c(1,2)],as.numeric) %>% 
  # Periodo==input$Periodo
  # Semestre==input$Semestre
  filter(Periodo=='2021' & Semestre=='1') %>%
  select(c("Total","Departamento","Programa",names(.)[grepl(u,names(.) , fixed = TRUE)])) %>% 
  # Departamento==input$Departamento
  filter(Departamento=='Finanzas') %>% 
  mutate(TOTAL = rowSums(across(`(IFCM)`:`(IFR)`))) %>% 
  mutate(.,across(`(IFCM)`:`(IFR)`,~.x/TOTAL)) %>% 
  replace(is.na(.),0) %>% 
  # Programa==input$Programa
  filter(Programa=='Contaduria Publica') %>%
  select(-c(Departamento,Programa,Total)) %>% 
  round(.,4)

#---------------#
# P: PROPUESTAS #
#---------------#

u <- "P" # Propuestas
pp <- get(as.name(paste("Resumen de trabajos de grado por",k,  "final"))) %>% 
  mutate_at(names(.)[-c(1,2)],as.numeric) %>% 
  # Periodo==input$Periodo
  # Semestre==input$Semestre
  filter(Periodo=='2021' & Semestre=='1') %>%
  select(c("Total","Departamento","Programa",names(.)[grepl(u,names(.) , fixed = TRUE)])) %>% 
  # Departamento==input$Departamento
  filter(Departamento=='Finanzas') %>% 
  mutate(TOTAL = rowSums(across(`(PCM)`:`(PR)`))) %>% 
  mutate(.,across(`(PCM)`:`(PR)`,~.x/TOTAL)) %>% 
  # Programa==input$Programa
  filter(Programa=='Contaduria Publica') %>%
  select(-c(Departamento,Programa,Total,Periodo)) %>% 
  round(.,4)

#---------#
# TITULOS #
#---------#

title_I <- paste("Informes Reportados:",p$Total)
title_P <- paste("Propuestas Reportadas:",pp$Total)

#  paste("I&R:",input$Departamento,input$Programa)
title_ <- paste("I&R:",'Finanzas','Ingenieria Financiera Y De Negocios') # I&R: Informes Y Resportes
title_ <- paste("<b>",title_,"</b>")

hijos_I <- gsub("[()]", "", names(p)[-4])
hijos_I <- paste0(hijos_I,": ",p[1,-4]*100,"%")

hijos_P <- gsub("[()]", "", names(pp)[-4])
hijos_P <- paste0(hijos_P,": ",pp[1,-4]*100,"%")

p <- 100*p
pp <- 100*pp


plot_ly(type = 'sunburst') %>%
  add_trace(labels = c(title_I,title_P,hijos_I,hijos_P),
            parents = c("","",rep(title_I,length(hijos_I)),rep(title_P,length(hijos_P))),
              values = c(100,100,as.numeric(p[,-4])-0.003333333,as.numeric(pp[,-4])),
            branchvalues = 'total') %>% 
  layout(title=title_)

