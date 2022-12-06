library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(plotly) # Graficos interactivos
rm(list=ls())

# Resumen de docentes por tipo de contratación - Por Programa
#------------#
# GRAFICO 10 #
#------------#

load('./Mejorado/Querys/05_Query.RData')

# aseseval <- input$AsesEval
aseseval <- 'asesores'

data <- get(as.name(paste(
  "Resumen de docentes",aseseval,"por tipo de contratacion - Por Programa final")))

nombres <- colnames(data)

p <- data %>% 
  mutate_at(nombres[-c(1:4)],as.numeric)

k <- p %>% 
  # Periodo==as.numeric(input$Periodo)
  # Semestre==as.numeric(input$Semestre))
  # Departamento==input$Departamento
  filter(Periodo == '2016',Semestre == '2') %>% 
  mutate("TOTAL"=sum(Total)) %>%
  filter(Departamento == 'Finanzas') %>% 
  mutate(.,across(`Carrera`:`Asesor_Externo_Otro`, ~ 100*round(.x/TOTAL,4))) %>% 
  select(-c(Departamento,Total,TOTAL,Periodo,Semestre))

titleB <- 'Titulo'

plot_ly(type = 'scatterpolar') %>%
  add_trace(
    r = as.matrix(k[1, -1])[1, ],
    theta = colnames(as.data.frame(k))[-1],
    name = as.data.frame(k)$Programa[1],
    mode = 'dotlines'
  ) %>%
  add_trace(
    r = as.matrix(k[2, -1])[1, ],
    theta = colnames(as.data.frame(k))[-1],
    name = as.data.frame(k)$Programa[2],
    mode = 'dotlines'
  ) %>%
  layout(
    polar = list(radialaxis = list(
      visible = T,
      range = c(-2.5, round(max(
        apply(as.data.frame(k)[, -1], 2, max)
      ), 3) + 2.5)
    )),
    legend = list(
      orientation = "h",
      title = "",
      xanchor = 2
    ),
    # title = titleB()
    title = titleB
  )
