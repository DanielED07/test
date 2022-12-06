library(dplyr)
library(readxl)
library(stringr)
library(tidyverse)
library(stringi)
rm(list=ls())


archivos <- dir("Actualizar")
load("./Mejorado/BD_trabajos.RData")

data1 <- read_excel(paste0('Actualizar/',archivos[1]),sheet ='fecha',skip = 2)
data2 <- read_excel(paste0('Actualizar/',archivos[2]),sheet ='030220',skip = 2)
data3 <- read_excel(paste0('Actualizar/',archivos[3]),sheet ='fecha',skip = 2)

titulos_trabajos_segmentados_actualizar <- data.frame(Periodo=character(0), #
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
data = data3

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

#x <- c()
for (i in 1:nrow(test)) {
  if(is.na(test[i,'Grupo_ID'])){
    print(i)
    #x <- c(x,i)
    test[i,c('Grupo_ID','Programa','Modalidad','Titulo','Docente_Asesor','Docente_Evaluador','Recomendacion_Convencion')] <- test[i-1,c('Grupo_ID','Programa','Modalidad','Titulo','Docente_Asesor','Docente_Evaluador','Recomendacion_Convencion')]
  }
}

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

titulos_trabajos_segmentados_actualizar <- rbind(titulos_trabajos_segmentados_actualizar,test)
save("titulos_trabajos_segmentados_actualizar", file = "./Mejorado/BD_Actualizar_1.RData")
