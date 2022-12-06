library(stringr)
library(tidyverse)
library(stringi)
rm(list=ls())
load("./Mejorado/BD_trabajos.RData")

limpiar_columna <- function(columna){
  columna_1 <- stri_trans_general(str = columna, id = "Latin-ASCII") # Se remueven tildes
  columna_2 <- gsub("([.]+)", '', columna_1) # Se eliminan puntos
  columna_3 <- gsub("([0-9]+).*$", '', columna_2) # Se eliminan numeros
  columna_4 <- str_to_title(columna_3) # Primera letra en Mayuscula, el resto en minuscula
  columna_5 <- gsub("  ", ' ', columna_4) # Se eliminan espacios adicionales
  
  return(columna_5)
}

#x <- unique(BD_trabajos$Programa)
#y <- limpiar_columna(x)
BD_trabajos$Programa <- limpiar_columna(BD_trabajos$Programa)
BD_trabajos$Modalidad <- limpiar_columna(BD_trabajos$Modalidad)
BD_trabajos$Departamento <- limpiar_columna(BD_trabajos$Departamento)

BD_trabajos$Programa <- BD_trabajos$Programa %>% 
  ifelse(.%in%.[str_detect(.,"Especializacion En For")],"Especializacion En Formulacion Y Evaluacion De Proyectos",.) %>% 
  ifelse(.%in%.[str_detect(.,"Especializacion En Fin")],"Especializacion En Finanzas",.)

BD_trabajos$Programa <- limpiar_columna(BD_trabajos$Programa)
#zz <- BD_trabajos[BD_trabajos$Programa %in% c("Especializacion En Formulacion Y Evaluacion De Proyectos","Especializacion En Finanzas"),]

#test <- data.frame('y'=y, 'x'='')
for (i in 1:length(BD_trabajos$Programa)) {
  x <- BD_trabajos$Programa[i]
  xx <- BD_trabajos$Departamento[i]
  #z <- grepl(substr(x,1,5), c("Especializacion En Finanzas","Especializacion En Formulacion Y Evaluacion De Proyectos","Maestria En Gestion De La Innovacion Tecnologica, Cooperacion y Desarrollo Regional"), fixed = TRUE)
  z <- x %in% c("Especializacion En Finanzas","Especializacion En Formulacion Y Evaluacion De Proyectos","Maestria En Gestion De La Innovacion Tecnologica, Cooperacion y Desarrollo Regional")
  y <- ifelse(z,"Postgrados",xx)
  BD_trabajos$Departamento[i] <- y
}


idx_comillas <- which(BD_trabajos$Departamento=="")
BD_trabajos$Departamento[idx_comillas] <- 'Postgrados'

save("BD_trabajos", file = "./Mejorado/BD_trabajos.RData")

#-------------------------------
#-------------------------------

zz <- `Resumen de estudiantes por programas final` %>% #BD_trabajos[BD_trabajos$Departamento=="Postgrados",] %>% 
  filter(Departamento=="Postgrados",Periodo=='2017',Semestre=='1') #%>% 
  #distinct(Grupo_ID,.keep_all = TRUE)

zz <- BD_trabajos %>% 
  filter(Departamento=="Postgrados",Periodo=='2017',Semestre=='1')

data_programas_departamentos <- data.frame('Programa'=c('Especializacion En Finanzas','Especializacion En Formulacion Y Evaluacion De Proyectos','Maestria En Gestion De La Innovacion Tecnologica, Coop Y Desarrollo Regional'),# 11: Numero total de totalizadores
           'Departamento'=rep('Postgrados',3),
           'Periodo'=rep('2017',length(3)),
           'Semestre' = rep('1',length(3)))
# data_programas_departamentos_test <- data_programas_departamentos %>% 
#   filter(Periodo=='2016')

kk <- merge(data_programas_departamentos, z, by = c('Departamento','Programa','Periodo','Semestre'), all.x = TRUE) %>% 
  replace(is.na(.), 0) # Se remplazan NaN por 0
