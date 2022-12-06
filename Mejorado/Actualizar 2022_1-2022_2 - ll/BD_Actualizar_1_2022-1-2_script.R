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
data1 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 1',skip = 2)
data2 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 3',skip = 2)
data3 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 5',skip = 2)
data4 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 6',skip = 2)
data5 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 7',skip = 2)
data6 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 8',skip = 2)
data7 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 9',skip = 2)
data8 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 10',skip = 2)
data9 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 11',skip = 2)

#---------#
# 2022- 2 #
#---------#
data10 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 12 inicio sdo semestr',skip = 2)
data11 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 13',skip = 2)
data12 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 14',skip = 2)
data13 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 15',skip = 2)
data14 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 16',skip = 2)
data15 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 17',skip = 2)
data16 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 18',skip = 2)
data17 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 20',skip = 2)
data18 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 21',skip = 2)
data19 <- read_excel(paste0('./Mejorado/Actualizar 2022_1-2022_2 - ll/',archivos[1]),sheet ='Plantilla 22',skip = 2)


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
data = data19

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
# # SOLO PARA DATA3
# test <- test[-2,]

# # SOLO PARA DATA5
# test <- test[-c(2,4,6),]

# # SOLO PARA DATA7
# test <- test[-c(2,4,6,9,12,14,16,20,22,24,26,30),]

# # SOLO PARA DATA17
# test <- test[-c(5,8),]

# # SOLO PARA DATA18
# test <- test[-c(2),]

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

# # SOLO PARA DATA1
# idx_asesor_na <- which(is.na(test$Docente_Asesor)==TRUE)
# test$Docente_Asesor[idx_asesor_na] <- 'Lucia Palacios Moya'

# # SOLO PARA DATA3
# test[1,'Docente_Asesor'] <- 'Angela Maria Builes Grisales'
# test[1,'Docente_Evaluador'] <- 'Angela Maria Builes Grisales'

# # SOLO PARA DATA4
# idx_asesor_na <- which(test$Docente_Asesor=='Sin Asesor')
# test$Docente_Asesor[idx_asesor_na] <- 'Lucia Palacios Moya'

# # SOLO PARA DATA5
# test[c(1:3),'Docente_Asesor'] <- 'Porfirio De Jesus Cardona Restrepo'
# test[c(1:3),'Docente_Evaluador'] <- 'Porfirio De Jesus Cardona Restrepo'
# test[9,'Modalidad'] <- 'Diplomado'

# # SOLO PARA DATA6
# test[c(8:10),'Docente_Asesor'] <- 'Lucia Palacios Moya'

# # SOLO PARA DATA7
# test[1,'Docente_Asesor'] <- 'Jose Leonardo Ramirez Echavarria'
# test[1,'Docente_Evaluador'] <- 'Jose Leonardo Ramirez Echavarria'
# test[3,'Docente_Asesor'] <- 'Porfirio De Jesus Cardona Restrepo'
# test[3,'Docente_Evaluador'] <- 'Porfirio De Jesus Cardona Restrepo'
# test[18,'Docente_Asesor'] <- 'Estela Herrera Cardenas'
# test[18,'Docente_Evaluador'] <- 'Estela Herrera Cardenas'
# test[c(12,13),'Docente_Asesor'] <- 'Gentil Eduardo Cordoba Rivera'
# test[c(12,13),'Docente_Evaluador'] <- 'Gentil Eduardo Cordoba Rivera'
# test[c(2,7,8),'Docente_Asesor'] <- 'Claudia Estela Herrera Cardenas'
# test[c(2,7,8),'Docente_Evaluador'] <- 'Claudia Estela Herrera Cardenas'
# test[c(5,9,14,15),'Docente_Asesor'] <- 'Juan Camilo Vasquez Acosta'
# test[c(5,9,14,15),'Docente_Evaluador'] <- 'Juan Camilo Vasquez Acosta'
# # Diplomado
# test[c(31,32),'Modalidad'] <- 'Diplomado'

# # SOLO PARA DATA9
# test[5,'Modalidad'] <- 'Diplomado'

# # SOLO PARA DATA10
# test[c(1:6),'Modalidad'] <- 'Diplomado'
# test[c(9,10),'Docente_Asesor'] <- 'Lucia Palacios Moya'

# # SOLO PARA DATA11
# test[c(1:48,52,55:57),'Modalidad'] <- 'Diplomado'
# idx_AE <- which(test$Docente_Asesor=='Asesor Externo') # AE: Asesor Externo
# idx_EE <- which(test$Docente_Evaluador=='Asesor Externo') # EE: Evaluador Externo
# test$Docente_Asesor[idx_AE] <- 'Asesor_Externo_Otro'
# test$Docente_Evaluador[idx_EE] <- 'Evaluador_Externo_Otro'

# # SOLO PARA DATA12
# test[c(1,2),'Modalidad'] <- 'Diplomado'
# idx_AE <- which(test$Docente_Asesor=='Asesor Externo') # AE: Asesor Externo
# idx_EE <- which(test$Docente_Evaluador=='Asesor Externo') # EE: Evaluador Externo
# test$Docente_Asesor[idx_AE] <- 'Asesor_Externo_Otro'
# test$Docente_Evaluador[idx_EE] <- 'Evaluador_Externo_Otro'

# # SOLO PARA DATA13
# test[1,'Modalidad'] <- 'Diplomado'
# idx_AE <- which(test$Docente_Asesor=='Asesor Externo') # AE: Asesor Externo
# idx_EE <- which(test$Docente_Evaluador=='Asesor Externo') # EE: Evaluador Externo
# test$Docente_Asesor[idx_AE] <- 'Asesor_Externo_Otro'
# test$Docente_Evaluador[idx_EE] <- 'Evaluador_Externo_Otro'

# # SOLO PARA DATA14
# test$Modalidad <- 'Diplomado'
# idx_AE <- which(test$Docente_Asesor=='Asesor Externo') # AE: Asesor Externo
# idx_EE <- which(test$Docente_Evaluador=='Asesor Externo') # EE: Evaluador Externo
# test$Docente_Asesor[idx_AE] <- 'Asesor_Externo_Otro'
# test$Docente_Evaluador[idx_EE] <- 'Evaluador_Externo_Otro'

# # SOLO PARA DATA15
# test$Docente_Asesor <- 'Sin Asesor'
# test$Docente_Evaluador <- 'Sin Asesor'

# # SOLO PARA DATA16
# test[c(1:3),'Modalidad'] <- 'Diplomado'
# test[c(1:3),'Docente_Asesor'] <- 'Asesor_Externo_Otro'
# test[c(1:3),'Docente_Evaluador'] <- 'Evaluador_Externo_Otro'

# # SOLO PARA DATA17
# test[c(3:6),'Docente_Asesor'] <- 'Yanneth Lopez Andrade'

# # SOLO PARA DATA18
# test[1,'Docente_Asesor'] <- 'Asesor_Externo_Otro'
# test[1,'Docente_Evaluador'] <- 'Evaluador_Externo_Otro'

# # SOLO PARA DATA19
# test <- test[-2,]
# test[1,'Docente_Asesor'] <- 'Edison Montoya Velez'
# test[1,'Docente_Evaluador'] <- 'Edison Montoya Velez'


titulos_trabajos_segmentados_actualizar_1 <- rbind(titulos_trabajos_segmentados_actualizar,test)
save("titulos_trabajos_segmentados_actualizar_1", file = "./Mejorado/Actualizar 2022_1-2022_2 - ll/BD_Actualizar_1_2022-1-2.RData")









