library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(plotly) # Graficos interactivos
rm(list=ls())
#-----------#
# GRAFICO 4 #
#-----------#

# Resumen de trabajos de grado por modalidad
load('./Mejorado/Querys/011_Query.RData')

l <- "modalidad"
data <- get(as.name(paste("Resumen de trabajos de grado por",l,  "final")))
nombres <- colnames(data)

p <- data %>% 
  mutate_at(nombres[-c(1:4)],as.numeric)

k <- p %>% 
  # Periodo==as.numeric(input$Periodo)
  # Semestre==as.numeric(input$Semestre))
  # Departamento==input$Departamento
  filter(Periodo == '2021',Semestre == '1') %>% 
  mutate("TOTAL"=sum(Total)) %>%
  filter(Departamento == 'Finanzas') %>% 
  mutate(.,across(`Proyecto De Grado`:`Intervencion Empresarial`, ~ 100*round(.x/TOTAL,4))) %>% 
  select(-c(Departamento,Total,TOTAL,Periodo,Semestre))

#------------------------------------------------
#------------------------------------------------
# Resumen de trabajos de grado por modalidad
load('./Mejorado/Querys/012_Query.RData')

ll <- "programas"
data_ <- get(as.name(paste("Resumen de trabajos de grado por",ll,  "final")))
nombres_ <- colnames(data_)

p_ <- data_ %>% 
  mutate_at(nombres_[-c(1:4)],as.numeric)

kk <- p_ %>% 
  # Periodo==as.numeric(input$Periodo)
  # Semestre==as.numeric(input$Semestre))
  # Departamento==input$Departamento
  filter(Periodo == '2016',Semestre == '2') %>% 
  mutate("TOTAL"=sum(Total)) %>%
  filter(Departamento == 'Finanzas') %>% 
  mutate(.,across(`(PCM)`:`(IFR)`, ~ 100*round(.x/TOTAL,4))) %>% 
  select(-c(Departamento,Total,TOTAL,Periodo,Semestre))

# titleB <- reactive({paste(input$Departamento,input$Periodo,"-",input$Semestre)})
titleB <- 'Titulo'

plot_ly(type = 'scatterpolar',mode = 'markers') %>%
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
  {if(length(as.data.frame(k)$Programa)>2)
    add_trace(.,
      r = as.matrix(k[3, -1])[1, ],
      theta = colnames(as.data.frame(k))[-1],
      name = as.data.frame(k)$Programa[3],
      mode = 'dotlines'
    )  else .
  
    } %>% 
  
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

