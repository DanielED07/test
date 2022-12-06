library(readxl)
library(stringr)
library(tidyverse)
library(stringi)

rm(list=ls())
archivos <- dir("Actualizar")

data1 <- read_excel(paste0('Actualizar/',archivos[4]),sheet ='010222',skip = 2)
data2 <- read_excel(paste0('Actualizar/',archivos[4]),sheet ='170222',skip = 2)
data3 <- read_excel(paste0('Actualizar/',archivos[4]),sheet ='190422',skip = 2)
data4 <- read_excel(paste0('Actualizar/',archivos[4]),sheet ='030522',skip = 2)
data5 <- read_excel(paste0('Actualizar/',archivos[4]),sheet ='170522',skip = 2)

titulos_trabajos_segmentados_actualizar_ <- data.frame(Periodo=character(0), #
                                                      Semestre = character(0), #
                                                      Nombre_Estudiante = character(0),#
                                                      Cedula=numeric(0), #
                                                      Grupo_ID=numeric(0), #
                                                      Departamento=character(0), #
                                                      Programa=character(0), #
                                                      Modalidad=character(0),#
                                                      Titulo=character(0), #
                                                      Recomendacion_Convencion=character(0), #
                                                      Docente_Asesor=character(0), #
                                                      Docente_Evaluador=character(0), #
                                                      Contratacion_Asesor=character(0),
                                                      Contratacion_Evaluador=character(0))
data = data5

n <- names(data)[c(1,2,4,5,6,7,8,9,10)]
test <- data[,n]
names(test) <- c('Grupo_ID','Nombre_Estudiante','Cedula','Programa','Modalidad','Titulo','Docente_Asesor','Docente_Evaluador','Recomendacion_Convencion')

test$Recomendacion_Convencion <- sub("\\).*", ")", test$Recomendacion_Convencion)

idx <- c()
for (i in 1:nrow(test)) {
  esFullNa <- all(is.na(test[i,]))
  if(esFullNa){
    idx <- c(idx,i)
  }
}

test <- test[-c(idx),]
test <- data.frame(test)
for (i in 1:nrow(test)) {
  if(is.na(test[i,'Grupo_ID'])  ){
    print(i)
    test[i,c('Grupo_ID','Programa','Modalidad','Titulo','Docente_Asesor','Docente_Evaluador','Recomendacion_Convencion')] <- test[i-1,c('Grupo_ID','Programa','Modalidad','Titulo','Docente_Asesor','Docente_Evaluador','Recomendacion_Convencion')]
  }
}

# SOLO PARA DATA5
test$Programa[1] <- 'Contaduría Pública'
test$Programa[2] <- 'Contaduría Pública'
test$Programa[3] <- 'Contaduría Pública'
test$Programa[7] <- 'Ingeniería Financiera'

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
limpiar_columna <- function(columna){
  columna_1 <- stri_trans_general(str = columna, id = "Latin-ASCII") # Se remueven tildes
  columna_2 <- gsub("([.]+)", '', columna_1) # Se eliminan puntos
  columna_3 <- gsub("([0-9]+).*$", '', columna_2) # Se eliminan numeros
  columna_4 <- str_to_title(columna_3) # Primera letra en Mayuscula, el resto en minuscula
  
  return(columna_4)
}

test$Programa <- limpiar_columna(test$Programa)
test$Modalidad <- limpiar_columna(test$Modalidad)

for (i in 1:length(test$Programa)) {
  x <- test$Programa[i]
  z <- grepl(substr(x,1,5), c("Especializacion en Finanzas","Especializacion en formulacion y evaluacion de proyectos","Maestria En Gestion de la Innovacion Tecnologica, Cooperacion y Desarrollo Regional"), fixed = TRUE)
  y <- ifelse(z,"Postgrados",x)
  test$Programa[i] <- y
}

test$Programa <- ifelse(test$Programa == 'Ingenieria Financiera','Ingenieria Financiera Y De Negocios',test$Programa) # Se reemplazan los 'Ingenieria Financiera' por 'Ingenieria Financiera y de Negocios'

# Departamentos
departamentos <- test$Programa %>% 
  ifelse(.%in% c("Tecnologia En Analisis De Costos Y Presupuestos","Ingenieria Financiera Y De Negocios","Contaduria Publica"),"Finanzas",.) %>% 
  ifelse(.%in% c("Tecnologia En Gestion Administrativa","Administracion Tecnologica"),"Ciencias Administrativas",.) %>% 
  ifelse(.%in% c("Tecnologia En Calidad","Tecnologia En Produccion","Ingenieria En Produccion","Ingenieria En Calidad"),"Calidad y Produccion",.) 



"Especializacion en formulacion y evaluacion de proyectos"

test$Departamento <-  departamentos

test$Periodo <- "2022"
test$Semestre <- "1"

test$Modalidad <- 
  test$Modalidad %>% 
  ifelse(.%in%.[str_detect(.,"Inves")],"Procesos de Investigacion",.) %>% 
  ifelse(.%in%c("Monografia","Trabajo de grado","Empredimiento"),"Proyecto de grado",.) %>%
  ifelse(.%in%c("Sistematizacion Aporte Laboral","Sistematizacion de Aporte Laboral","Reconocimiento Laboral"),"Intervencion empresarial",.)

# # SOLO PARA DATA 2
# test <- data.frame(test)
# test[c(test$Titulo=='Modelo financiero de gestión y prevención de situaciones de riesgo económico a las PYMES por factores externos')[-6],'Docente_Asesor'] <-  'Juan Esteban Álvarez Hernández'
# SOLO PARA DATA 5
test <- data.frame(test)
test[c(test$Nombre_Estudiante=='Cristian Daniel Villa Lopez'),'Docente_Asesor'] <-  'Sebastian Aguirre Gómez'
test[c(test$Nombre_Estudiante=='Cristian Daniel Villa Lopez'),'Docente_Evaluador'] <-  'Sebastian Aguirre Gómez'
test[c(test$Nombre_Estudiante=='Emely Yuliana Ramírez Realpe'),'Docente_Asesor'] <-  'Sebastian Aguirre Gómez'
test[c(test$Nombre_Estudiante=='Emely Yuliana Ramírez Realpe'),'Docente_Evaluador'] <-  'Sebastian Aguirre Gómez'
test[c(test$Nombre_Estudiante=='Yesid Jabeydy Cuartas Barrientos'),'Docente_Evaluador'] <-  'Sin Asesor'
test[c(test$Nombre_Estudiante=='Manuela Vélez Serrato'),'Docente_Asesor'] <-  'Sebastian Aguirre Gómez'
test[c(test$Nombre_Estudiante=='Manuela Vélez Serrato'),'Docente_Evaluador'] <-  'Sebastian Aguirre Gómez'
test[c(test$Nombre_Estudiante=='Valeria Agudelo Velásquez'),'Docente_Asesor'] <-  'Sebastian Aguirre Gómez'
test[c(test$Nombre_Estudiante=='Valeria Agudelo Velásquez'),'Docente_Evaluador'] <-  'Sebastian Aguirre Gómez'

titulos_trabajos_segmentados_actualizar_ <- na.omit(rbind(titulos_trabajos_segmentados_actualizar_,test))
save("titulos_trabajos_segmentados_actualizar_", file = "./Mejorado/BD_Actualizar_2.RData")




