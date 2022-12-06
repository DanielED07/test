rm(list=ls())
library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(plotly) # Graficos interactivos

#-----------#
# GRAFICO 2 #
#-----------#

# Resumen de estudiantes por programas
load('./Mejorado/Querys/010_Query.RData')

test <- `Resumen de estudiantes por programas final`
nombres <- colnames(test)
#--------------------------------------------------
#--------------------------------------------------
test <- test %>% 
  mutate_at(nombres[-c(1:4)],as.numeric) %>% 
  # Periodo==as.numeric(input$Periodo)
  # Semestre==as.numeric(input$Semestre)
  filter(Periodo== '2016' & Semestre== '2') 
#--------------------------------------------------
#--------------------------------------------------
names(test)[6] <- "Porcentaje"
#test$Porcentaje <- round(100*test$Porcentaje,5)
t <- test %>% 
  group_by(Departamento) %>% 
  summarize(Total=sum(Total),Porcentaje=round(100*(sum(Total)/sum(test$Total)),5))
  

tt <- test %>% 
  arrange(Departamento) %>% 
  group_by(Departamento) %>% 
  mutate(n=Total) %>% 
  mutate(Total=sum(n)) %>% 
  mutate(Porcentaje=round(100*(Total/sum(test$Total))*(n/Total),5)) %>% 
  mutate(Porcentaje=ifelse(is.na(Porcentaje),0,Porcentaje))
#--------------------------------------------------
#--------------------------------------------------
plot_ly(
  type = 'sunburst'
)%>%
  add_trace(
    labels=c("Departamento",unique(t$Departamento)[1],tt$Programa[tt$Departamento==unique(t$Departamento)[1]],
             unique(t$Departamento)[2],tt$Programa[tt$Departamento==unique(t$Departamento)[2]],
             unique(t$Departamento)[3],tt$Programa[tt$Departamento==unique(t$Departamento)[3]],
             unique(t$Departamento)[4],tt$Programa[tt$Departamento==unique(t$Departamento)[4]]),
    
    parents=c("","Departamento",tt$Departamento[tt$Departamento==unique(t$Departamento)[1]],
              "Departamento",tt$Departamento[tt$Departamento==unique(t$Departamento)[2]],
              "Departamento",tt$Departamento[tt$Departamento==unique(t$Departamento)[3]],
              "Departamento",tt$Departamento[tt$Departamento==unique(t$Departamento)[4]]),
    
    values=c( 100,t$Porcentaje[1],tt$Porcentaje[tt$Departamento==unique(t$Departamento)[1]],
              t$Porcentaje[2],tt$Porcentaje[tt$Departamento==unique(t$Departamento)[2]],
              t$Porcentaje[3],tt$Porcentaje[tt$Departamento==unique(t$Departamento)[3]],
              t$Porcentaje[4],tt$Porcentaje[tt$Departamento==unique(t$Departamento)[4]]),
    
    branchvalues="total"
  ) %>% 
  layout(legend = list(
    orientation = "h",
    title="",
    xanchor=2
  ),
  title='Titulo'#titleP()
  )
  