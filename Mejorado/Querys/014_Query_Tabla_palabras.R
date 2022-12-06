library(dplyr)
library(webr)
library(wordcloud2)
library(tm)
rm(list=ls())
load("./Mejorado/Querys/013_Query.RData")


# load("./RData/dataPalabras.RData")
# y <- tabla_palabras %>% 
#   filter(periodo=='2016')

zz <- titulos_trabajos_segmentados %>% 
  #filter(Departamento=='Finanzas') %>% 
  select(Titulo,Programa,Periodo,Semestre) %>% 
  mutate(Titulo=tolower(Titulo)) %>%
  distinct(Titulo, .keep_all= TRUE) 

tabla_palabras_ <- data.frame(Word=character(0),
                             Freq = numeric(0),
                             Programa=character(0),
                             Periodo=character(0),
                             Semestre=character(0))  

for (periodo in unique(zz$Periodo)){
  for(semestre in unique(zz[zz$Periodo==periodo,'Semestre'])){
    for (programa in unique(zz[(zz$Periodo==periodo & zz$Semestre==semestre),'Programa'])){
      data_temp <- zz %>% 
        filter(Programa==programa & Periodo==periodo & Semestre==semestre)
      
      text2 <-data_temp$Titulo
      docs2 <- Corpus(VectorSource(text2))
      docs2 <- docs2 %>%
        tm_map(removeNumbers) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace)
      docs2 <- tm_map(docs2, content_transformer(tolower))
      docs2 <- tm_map(docs2, removeWords, stopwords("spanish"))
      dtm2 <- TermDocumentMatrix(docs2)
      matrix2 <- as.matrix(dtm2)
      words2 <- sort(rowSums(matrix2),decreasing=TRUE)
      df2 <-data.frame(Word = names(words2),
                       Freq=words2,
                       Programas=programa,
                       Periodo=periodo,
                       Semestre=semestre)
      tabla_palabras_ <- rbind(tabla_palabras_,df2)
    }
  }
}

save("tabla_palabras_", file = "./Mejorado/Querys/014_Query.RData")

