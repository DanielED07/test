load("./Mejorado/BD_trabajos.RData")


datos_actualizar <- BD_trabajos

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

BD_trabajos <- datos_actualizar
save("BD_trabajos", file = "./Mejorado/BD_trabajos.RData")
