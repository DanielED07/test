library(readxl)
library(stringr)
library(tidyverse)
library(stringi)

rm(list=ls())
archivos <- dir("./Mejorado/Actualizar 2022_1-2022_2 - ll")
load("./Mejorado/BD_trabajos.RData")

#---------#
# 2022- 1 #
#---------#
data1 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 3',skip = 2)
data2 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 4',skip = 2)
data3 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 5',skip = 2)
data4 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 6',skip = 2)
data5 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 7',skip = 2)
data6 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 8',skip = 2)
data7 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 9',skip = 2)

#---------#
# 2022- 2 #
#---------#
data8 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 14',skip = 2)
data9 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 15',skip = 2)
data10 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 17',skip = 2)
data11 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[4]),sheet ='PLANTILLA 22',skip = 2)


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
data = data11

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

# # SOLO PARA DATA4
# test <- test[-4,]

# # SOLO PARA DATA4
# test <- test[-3,]

# # SOLO PARA DATA7
# test <- test[-32,]

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
test$Semestre <- "2"

test$Modalidad <- 
  test$Modalidad %>% 
  ifelse(.%in%.[str_detect(.,"Inves")],"Procesos de Investigacion",.) %>% 
  ifelse(.%in%c("Monografia","Trabajo de grado","Empredimiento"),"Proyecto de grado",.) %>%
  ifelse(.%in%c("Sistematizacion Aporte Laboral","Sistematizacion de Aporte Laboral","Reconocimiento Laboral"),"Intervencion empresarial",.)

# # SOLO PARA DATA4
# test[3,'Docente_Evaluador'] <- 'Erica Janeth Agudelo Ceballos'

# # SOLO PARA DATA5
# test[c(2,3),'Docente_Asesor'] <- 'Lucia Palacios Moya'

# # SOLO PARA DATA6
# test[c(1:5),'Docente_Asesor'] <- 'Lucia Palacios Moya'
# test[c(6:9),'Docente_Asesor'] <- 'Ricardo Alonso Colmenares'

# # SOLO PARA DATA7
# test[c(15,16),'Docente_Asesor'] <- 'Ricardo Alonso Colmenares'
# test[c(25:27,30:31),'Docente_Asesor'] <- 'Lucia Palacios Moya'

# # SOLO PARA DATA11
# test[c(7:9),'Docente_Evaluador'] <- 'Rafael Jaime Carmona'

titulos_trabajos_segmentados_actualizar <- rbind(titulos_trabajos_segmentados_actualizar,test)

titulos_trabajos_segmentados_actualizar_2 <- titulos_trabajos_segmentados_actualizar

save("titulos_trabajos_segmentados_actualizar_2", file = "./Mejorado/Actualizar 2022_1-2022_2 - ll/BD_Actualizar_2_2022-1-2.RData")
