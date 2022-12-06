library(tidyverse) # Para operaciones,graficos, etc con DataSets
library(wordcloud2) # Graficos de nube interactivos
library(plotly) # Graficos interactivos
rm(list=ls())
#-----------#
# GRAFICO 5 #
#-----------#

# Palabras claves en trabajos
load('./Mejorado/Querys/014_Query.RData')
a <- get("tabla_palabras_")
test <- a %>% 
  # Periodo==input$Periodo
  # Semestre==input$Semestre
  # Programas==input$Palabras
  filter(Periodo=='2016',Semestre=='2',Programas=='Ingenieria Financiera Y De Negocios') %>% 
  select(Word,Freq)     

set.seed(123)
# data=ll()
wordcloud2(data=test, size = 0.7, shape = 'diamond') # looks good

#-----------#
# GRAFICO 6 #
#-----------#

# Palabras claves en trabajos
df1.1 <- test[test$Freq>=floor(summary(test$Freq)[4]+max(test$Freq)/3),]
plot_ly(
  labels = c("Palabras Clave", df1.1$Word),
  parents = c("", rep("Palabras Clave",length(df1.1$Word))),
  values = c(0, df1.1$Freq),
  type = 'sunburst'
  
)

