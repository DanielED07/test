library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(plotly) # Graficos interactivos
rm(list=ls())

# Titulos trabajos de grado
#------------#
# GRAFICO 11 #
#------------#

load('./Mejorado/Querys/013_Query.RData')

test <- `titulos_trabajos_segmentados`

z <-  test %>%
  filter(Departamento == 'Finanzas' , Programa == 'Ingenieria Financiera Y De Negocios') %>% 
  filter(Periodo == '2016', Semestre == '2' , Modalidad == 'Proyecto De Grado') %>% 
  select(c('Titulo','Total','Cedulas')) %>% 
  rename(Integrantes=Total) %>% 
  distinct(Titulo,.keep_all = TRUE)
  
