rm(list=ls())

load("./Mejorado/BD_trabajos.RData")
load("./Mejorado/BD_Actualizar_1.RData")
load("./Mejorado/BD_Actualizar_2.RData")

datos_actualizar <- rbind(titulos_trabajos_segmentados_actualizar,titulos_trabajos_segmentados_actualizar_)

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
  distinct(Nombre_Estudiante, .keep_all= TRUE)

zz$Contratacion_Asesor[is.na(zz$Contratacion_Asesor)] <- 'Asesor_Externo_Otro'
zzz <-  merge(zz, BD_trabajos[(BD_trabajos$Periodo=='2021' & BD_trabajos$Semestre=='2'),c('Docente_Evaluador','Contratacion_Evaluador','Modalidad','Programa')], by = c('Docente_Evaluador','Modalidad','Programa'), all.x = TRUE) %>% 
  distinct(Nombre_Estudiante, .keep_all= TRUE)
zzz$Contratacion_Evaluador[is.na(zzz$Contratacion_Evaluador)] <- 'Evaluador_Externo_Otro'
  

idx_duplicados <- which(duplicated(datos_actualizar$Nombre_Estudiante))
nombres <- datos_actualizar$Nombre_Estudiante[idx_duplicados]

data_temp1 <- datos_actualizar[datos_actualizar$Nombre_Estudiante==nombres[1],][2,]
data_temp1$Contratacion_Asesor <- zzz[zzz$Nombre_Estudiante==nombres[1],'Contratacion_Asesor']
data_temp1$Contratacion_Evaluador <- zzz[zzz$Nombre_Estudiante==nombres[1],'Contratacion_Evaluador']

data_temp2 <- datos_actualizar[datos_actualizar$Nombre_Estudiante==nombres[2],][1,]
data_temp2$Contratacion_Asesor <- zzz[zzz$Nombre_Estudiante==nombres[2],'Contratacion_Asesor']
data_temp2$Contratacion_Evaluador <- zzz[zzz$Nombre_Estudiante==nombres[2],'Contratacion_Evaluador']

zzz <- rbind(zzz,data_temp1)
zzz <- rbind(zzz,data_temp2)
zzz <- zzz %>%
  select(Periodo,Semestre,Nombre_Estudiante,Cedula,
         Grupo_ID,Departamento,Programa,Modalidad,
         Titulo,Recomendacion_Convencion,Docente_Asesor,Docente_Evaluador,
         Contratacion_Asesor,Contratacion_Evaluador)

BD_trabajos <- rbind(BD_trabajos,zzz)
save("BD_trabajos", file = "./Mejorado/BD_trabajos.RData")
