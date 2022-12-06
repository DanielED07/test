library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(plotly) # Graficos interactivos
rm(list=ls())
#-----------#
# GRAFICO 9 #
#-----------#

# Resumen de docentes por tipo de contratación - Por Modalidad
#load('./Mejorado/Querys/03_Query.RData')
load('./Mejorado/Querys/04_Query.RData')

test1 <- `Resumen de docentes evaluadores por tipo de contratación - Por Modalidad final` %>% 
  select(-Total) %>% 
  gather(key="Contratacion_Asesor", value = "Total",-c('Modalidad','Periodo','Semestre')) %>% 
  filter(Periodo=='2016',Semestre=='2') %>% 
  select(-c(Periodo,Semestre))

test2 <- test1 %>% 
  group_by(Modalidad) %>% 
  summarise(Total=sum(Total)) %>% 
  mutate('%'=100*Total/sum(Total),2)
  

test3 <- test2 %>% 
  merge(test1,.,by='Modalidad',all.x = TRUE) %>% 
  rename(n=Total.x,Total=Total.y) %>% 
  mutate('%Local'=(n*`%`)/Total) %>% 
  replace(is.na(.),0) %>% 
  mutate(across(where(is.numeric), as.double)) %>% 
  arrange(Contratacion_Asesor)
  
  

# paste("Resumen de docentes",input$AsesEval,"por tipo de contratación - Por Modalidad")
titleA <- paste("Resumen de docentes",'asesores',"por tipo de contratación - Por Modalidad")


plot_ly(
  type = 'sunburst',
  maxdepth=4
)%>%
  add_trace(

    labels=c("Modalidad",
             unique(test3$Modalidad)[1],unique(test3$Contratacion_Asesor),
             unique(test3$Modalidad)[2],unique(test3$Contratacion_Asesor),
             unique(test3$Modalidad)[3],unique(test3$Contratacion_Asesor),
             unique(test3$Modalidad)[4],unique(test3$Contratacion_Asesor),
             unique(test3$Modalidad)[5],unique(test3$Contratacion_Asesor),
             unique(test3$Modalidad)[6],unique(test3$Contratacion_Asesor)),
    

    parents=c("",
              "Modalidad",rep(unique(test3$Modalidad)[1],6),
              "Modalidad",rep(unique(test3$Modalidad)[2],6),
              "Modalidad",rep(unique(test3$Modalidad)[3],6),
              "Modalidad",rep(unique(test3$Modalidad)[4],6),
              "Modalidad",rep(unique(test3$Modalidad)[5],6),
              "Modalidad",rep(unique(test3$Modalidad)[6],6)),
    

    values=c( 100.0,
              test2$`%`[1],test3[test3$Modalidad==test2$Modalidad[1],'%Local'],
              test2$`%`[2],test3[test3$Modalidad==test2$Modalidad[2],'%Local'],
              test2$`%`[3],test3[test3$Modalidad==test2$Modalidad[3],'%Local'],
              test2$`%`[4],test3[test3$Modalidad==test2$Modalidad[4],'%Local'],
              test2$`%`[5],test3[test3$Modalidad==test2$Modalidad[5],'%Local'],
              test2$`%`[6],test3[test3$Modalidad==test2$Modalidad[6],'%Local']),
    
    
    branchvalues="total") %>% 
  
  layout(legend = list(
    orientation = "h",
    title="",
    xanchor=2),
  title=titleA#titleA(),
  )
