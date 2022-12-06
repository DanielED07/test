library(dplyr)
rm(list=ls())

load("./Mejorado/Actualizar 2022_1-2022_2 - ll/BD_Actualizar_1_2022-1-2.RData")
load("./Mejorado/Actualizar 2022_1-2022_2 - ll/BD_Actualizar_2_2022-1-2.RData")
load("./Mejorado/BD_trabajos.RData")

datos_actualizar <- rbind(titulos_trabajos_segmentados_actualizar_1,
                          titulos_trabajos_segmentados_actualizar_2)

# Para dar codigo unico a los grupos
datos_actualizar$Grupo_ID <- as.numeric(datos_actualizar$Grupo_ID)


for (i in 1:nrow(datos_actualizar)) { # Se itera sobre todas  las observaciones
  print(i)
  if (i!=1){
    grupo_ID_actual <- datos_actualizar[i,'Grupo_ID']
    print(c(as.character(grupo_ID_anterior),as.character(grupo_ID_actual)))
    if(grupo_ID_anterior==grupo_ID_actual){
      grupo_ID_anterior <- datos_actualizar[i-1,'Grupo_ID']
      datos_actualizar[i,'Grupo_ID'] <- as.numeric(grupo_ID_anterior)
      grupo_ID_anterior <- grupo_ID_actual
    }else{
      grupo_ID_anterior <- datos_actualizar[i-1,'Grupo_ID']
      datos_actualizar[i,'Grupo_ID'] <- as.numeric(grupo_ID_anterior)+1
      grupo_ID_anterior <- grupo_ID_actual
    }
    
  }else{
    grupo_ID_anterior <- datos_actualizar[i,'Grupo_ID']
    datos_actualizar[i,'Grupo_ID'] <- as.numeric(grupo_ID_anterior)
  }
  
}

zz <-  merge(x = datos_actualizar, y = BD_trabajos[(BD_trabajos$Periodo=='2021' & BD_trabajos$Semestre=='2'),c('Docente_Asesor','Contratacion_Asesor','Modalidad','Programa')], by = c('Docente_Asesor','Modalidad','Programa'), all.x = TRUE) %>% 
  distinct(Nombre_Estudiante,Titulo,Recomendacion_Convencion,Grupo_ID, .keep_all= TRUE) 

zz$Contratacion_Asesor[is.na(zz$Contratacion_Asesor)] <- 'Asesor_Externo_Otro'

zzz <- merge(zz, BD_trabajos[(BD_trabajos$Periodo=='2021' & BD_trabajos$Semestre=='2'),c('Docente_Evaluador','Contratacion_Evaluador','Modalidad','Programa')], by = c('Docente_Evaluador','Modalidad','Programa'), all.x = TRUE) %>% 
  distinct(Nombre_Estudiante,Titulo,Recomendacion_Convencion,Grupo_ID, .keep_all= TRUE)

zzz$Contratacion_Evaluador[is.na(zzz$Contratacion_Evaluador)] <- 'Evaluador_Externo_Otro'

zzz <- zzz %>%
  select(Periodo,Semestre,Nombre_Estudiante,Cedula,
         Grupo_ID,Departamento,Programa,Modalidad,
         Titulo,Recomendacion_Convencion,Docente_Asesor,Docente_Evaluador,
         Contratacion_Asesor,Contratacion_Evaluador)

limpiar_categoria_estudiante <- function(columna){
  columna_1 <- stri_trans_general(str = columna, id = "Latin-ASCII") # Se remueven tildes
  columna_2 <- gsub("([.]+)", '', columna_1) # Se eliminan puntos
  columna_3 <- gsub("([0-9]+).*$", '', columna_2) # Se eliminan numeros
  columna_4 <- str_to_title(columna_3) # Primera letra en Mayuscula, el resto en minuscula
  
  return(columna_4)
}

zzz$Nombre_Estudiante <- limpiar_categoria_estudiante(zzz$Nombre_Estudiante)
zzz$Docente_Asesor <- limpiar_categoria_estudiante(zzz$Docente_Asesor)
zzz$Docente_Evaluador <- limpiar_categoria_estudiante(zzz$Docente_Evaluador)

BD_trabajos <- rbind(BD_trabajos,zzz)
save("BD_trabajos", file = "./Mejorado/BD_trabajos.RData")
