library(readxl)
library(stringr)
library(tidyverse)
library(stringi)
library(readr)

rm(list=ls())

BD_trabajos <- data.frame(Periodo=character(0),
                              Semestre = character(0),
                              Cedula=numeric(0),
                              Grupo_ID=numeric(0),
                              Departamento=character(0),
                              Programa=character(0),
                              Modalidad=character(0),
                              Titulo=character(0),
                              Recomendacion_Convencion=character(0),
                              Docente_Asesor=character(0),
                              Docente_Evaluador=character(0),
                              Contratacion_Asesor=character(0),
                              Contratacion_Evaluador=character(0))

cont <- 0
archivos <- dir("./datos/")
for(file in archivos){
  cont <- cont+1
  
  file <- archivos[cont]
  
  numero <- as.character(parse_number(file,locale = locale(grouping_mark = " "))) # Numero que sale en el nombre del archivo del totalizador
  semestre <- substr(numero,nchar(numero),nchar(numero)) # semestre evaluado
  year <- substr(numero,1,nchar(numero)-1) # año evaluado
  
  ruta <- paste0('./datos/',file) # ruta para leer archivo totalizador del año y semestre evaluado
  hoja <- paste0(year,semestre) # Nombre de la hoja del archivo totalizador donde estan todos los dartos
  datos_raw <- read_excel(ruta,sheet = hoja) # lectura del archivo totalizador

  datos_raw$Periodo <- year # Se adjunta el año evaluado
  datos_raw$Semestre <- semestre # Se adjunta el semestre evaluado
  
  # Se procesa el nombre de las columnas
  estandarizacion_nombres_columnas <- function(nombres_columnas){
    nombres_columnas_1 <- stri_trans_general(str = nombres_columnas, id = "Latin-ASCII") # Se remueven tildes
    nombres_columnas_2 <- gsub("([' ']+)", '', nombres_columnas_1) # Se eliminan espacios
    nombres_columnas_3 <- gsub("([.]+)", '', nombres_columnas_2) # Se eliminan puntos
    nombres_columnas_4 <- gsub("([0-9]+).*$", '', nombres_columnas_3) # Se eliminan numeros
    nombres_columnas_5 <- str_to_title(nombres_columnas_4) # Primera letra en Mayuscula, el resto en minuscula
    
    return(nombres_columnas_5)
  }
  
  columnas_raw <- colnames(datos_raw) # Nombres columnas crudas
  columnas_std <- estandarizacion_nombres_columnas(nombres_columnas = columnas_raw) # std: Estandar (Standard)
  colnames(datos_raw) <- columnas_std # Nombres estandarizados columnas
  
  # Columnas necesarias:
  # Periodo := Periodo
  # Semestre := Semestre
  # Estudiante := Nombre_Estudiante
  # Cedula := Cedula
  # Tnumero := Grupo_ID
  # Departamento // Se define más tarde con el programa
  # Programa := Programa
  # Modalidad := Modalidad
  # Titulodeltrabajo := Titulo
  # Recomendacion :=  Recomendacion_Convencion
  # Auxiliar3 := Doncente_Asesor
  # Auxiliar4 := Doncente_Evaluador
  # Contratacion_Asesor // // Se define más tarde con: Oasesor,Catasesor,Casesor,Espasesor,(Especial)
  # Contratacion_Evaluador // // Se define más tarde con: Oevaluador,Catevaluador,Cevaluador,Espevaluaodr
  
  idx_Auxiliar <- which(colnames(datos_raw)=='Auxiliar') # Indices donde aparece Auxiliar
  colnames(datos_raw)[idx_Auxiliar[3]] <- 'Auxiliar3' # Para identificar el Auxiliar que lleva el nombre del asesor
  colnames(datos_raw)[idx_Auxiliar[4]] <- 'Auxiliar4' # Para identificar el Auxiliar que lleva el nombre del evaluador
  
  if ("Especial" %in% colnames(datos_raw)){ # Si existe la columna 'Especial' en el totalizador se considera, en caso contrario no se considera
    columnas_necesarias <- c('Periodo','Semestre','Estudiante','Cedula','Tnumero',
                               'Programa','Modalidad','Titulodeltrabajo','Recomendacion',
                               'Auxiliar3','Auxiliar4','Oasesor','Catasesor','Casesor','Espasesor',
                               'Especial','Oevaluador','Catevaluador','Cevaluador','Espevaluaodr') # Columnas necesarias datos interes
    columnas_asesor <- c('Oasesor','Catasesor','Casesor','Espasesor','Especial') # Columnas necesarias datos asesores
  }else{
    columnas_necesarias <- c('Periodo','Semestre','Estudiante','Cedula','Tnumero',
                             'Programa','Modalidad','Titulodeltrabajo','Recomendacion',
                             'Auxiliar3','Auxiliar4','Oasesor','Catasesor','Casesor','Espasesor',
                             'Oevaluador','Catevaluador','Cevaluador','Espevaluaodr') # Columnas necesarias datos interes
    columnas_asesor <- c('Oasesor','Catasesor','Casesor','Espasesor') # Columnas necesarias datos asesores
  }
  columnas_evaluador <- c('Oevaluador','Catevaluador','Cevaluador','Espevaluaodr') # Columnas necesarias datos evaluadores
  
  datos_interes <- datos_raw[,columnas_necesarias] # Datos de interes
  print('LLEGA ANTES DE LA LIMPIEZA DATOS FALTANTES')
  # Limpieza datos faltantes
  idx_filas_vacias <- c() # filas que estan vacias
  for (i in 1:nrow(datos_interes)) { # Se itera sobre todas  las observaciones
    esFullNa <- all(is.na(datos_interes[i,c('Estudiante','Cedula','Tnumero','Programa','Modalidad','Titulodeltrabajo','Recomendacion')])) # ¿La fila 'i' esta completamente vacia en las columnas especificadas?
    if(esFullNa){ # Si La fila 'i' esta completamente vacia
      idx_filas_vacias <- c(idx_filas_vacias,i) # Se agrega a los indices de filas que estan vacias
    }
  }
  
  datos_interes_noNA <- datos_interes[-c(idx_filas_vacias),] # Datos de interes sin filas vacias
  
  idx_fila_errores <- c() # filas que estan vacias por culpa de un error en el archivo por combinar tablas
  for (i in 1:nrow(datos_interes_noNA)) { # Se itera sobre todas  las observaciones
    if(is.na(datos_interes_noNA[i,'Estudiante'])){ # Si el nombre del estudiante esta vacio, es porque se genero un error
      print(i)
      idx_fila_errores <- c(idx_fila_errores,i) # Se agrega s a los indices de filas vacias por culpa del error
      datos_interes_noNA[i+1,c('Tnumero','Programa','Modalidad','Titulodeltrabajo','Recomendacion')] <- datos_interes_noNA[i,c('Tnumero','Programa','Modalidad','Titulodeltrabajo','Recomendacion')] # Los datos de la fila con nombre vacio corresponden al siguiente dato
    }
  }
  
  if (length(idx_fila_errores)!=0){ # Si hay errores
    datos_interes_noErrores <-  datos_interes_noNA[-c(idx_fila_errores),] # Se eliminan errores
  }else{
    datos_interes_noErrores <-  datos_interes_noNA # No existen errores
  }
  
  print('LLEGA ANTES DE LA IMPUTACION DATOS FALTANTES')
  # Imputacion datos faltantes
  # Si existen datos faltantes es porque los datos de la fila anterior corresponderan a la de la fila actual
  
  for (i in 1:nrow(datos_interes_noErrores)){
    estudiante <- datos_interes_noErrores[i,c('Tnumero','Programa','Modalidad','Titulodeltrabajo','Recomendacion')]
    columnas_inputar <- c('Tnumero','Programa','Modalidad','Titulodeltrabajo','Recomendacion',
                          columnas_asesor,columnas_evaluador)
    esNA <- ifelse(sum(is.na(estudiante))!=0,TRUE,FALSE)
    if (esNA){
      datos_interes_noErrores[i,columnas_inputar] <- datos_interes_noErrores[i-1,columnas_inputar]
    }
  }
  
  print('LLEGA ANTES DE VERIFICAR SI NO HAY INFO DOCENTES COMPLETO')
  #--------------------------
  #--------------------------
  # Se verifica si no hay informacion diligenciada de los docentes por completo (ni asesor ni evaluador)
  noData_catDocente <- which(apply(datos_interes_noErrores, 1, function(x){all(is.na(x[12:20]))})==TRUE)
  if(length(noData_catDocente)!=0){
    
    temp_data <-datos_interes_noErrores[noData_catDocente,] # data temporal a inputar
    temp_grupos <- unique(temp_data$Tnumero)
    for (grupo in temp_grupos) {
      idx_est <- which(temp_data$Tnumero==grupo) # Cuales pertenecen al mismo grupo (por si hay más de un grupo)
      temp_estudiante <- unlist(temp_data[idx_est,'Estudiante'])[1] # Estudiante de referencia del mismo grupo
      temp_data_input <- datos_interes_noErrores[datos_interes_noErrores$Estudiante==temp_estudiante,] # Todos los datos con el estudiante de referencia
      data_input_idx <- which(apply(temp_data_input, 1, function(x){!all(is.na(x[12:20]))})==TRUE) # Indice de fila que tenga infor de los docentes
      col_input <- c("Oasesor","Catasesor","Casesor","Espasesor","Especial","Oevaluador","Catevaluador","Cevaluador","Espevaluaodr") # columnas a imputar
      if ("Especial"%in%colnames(temp_data_input)){
        col_input <-col_input
      }else{
        col_input <-col_input[-c(5)]
      }
      if(length(data_input_idx)==0){
        next
      }
    
      data_input <- unlist(temp_data_input[data_input_idx,col_input]) # Datos a inputar
      datos_interes_noErrores[noData_catDocente[idx_est],col_input] <- matrix(data=rep(data_input,length(idx_est)),ncol = 9,nrow = length(idx_est),byrow = TRUE) # Se inputan los datos
      
      
      }
  }
  print('PASO')
  #--------------------------
  #--------------------------
  cat_asesor <- datos_interes_noErrores[,columnas_asesor] # Categorias asesor
  cat_evaluador <- datos_interes_noErrores[,columnas_evaluador] # Categorias evaluador
  categoria_docente <- function(fila){ # Funcion para determinar la categoria del docente
    fullNA <- all(is.na(fila))# La fila esta llena de NaNs, es decir no hay asesor
    if(fullNA){
      return('Sin Asesor') # Se devuleve 'Sin Asesor' (En caso que el docente sea asesor)
    }else{
      idx_cat_asesor <- which(!is.na(fila))# Indice de la categoria del asesor
      return(fila[idx_cat_asesor]) # Se devuleve la categoria del docente
    }
  }
  
  limpiar_categoria_docente <- function(columna){
    columna_1 <- stri_trans_general(str = columna, id = "Latin-ASCII") # Se remueven tildes
    columna_2 <- gsub("([.]+)", '', columna_1) # Se eliminan puntos
    columna_3 <- gsub("([0-9]+).*$", '', columna_2) # Se eliminan numeros
    columna_4 <- str_to_title(columna_3) # Primera letra en Mayuscula, el resto en minuscula
    
    return(columna_4)
  }
  
  cat_asesor <- as.data.frame(apply(cat_asesor, 2, limpiar_categoria_docente)) # Se limpian las categorias de los asesores
  cat_evaluador <- as.data.frame(apply(cat_evaluador, 2, limpiar_categoria_docente)) # Se limpian las categorias de los evaluadores
  
  datos_interes_noErrores$Contratacion_Asesor <- apply(cat_asesor, 1, categoria_docente) # Tipo de contratacion docentes asesores
  datos_interes_noErrores$Contratacion_Evaluador <- apply(cat_evaluador, 1, categoria_docente) # Tipo de contratacion docentes evaluadores
  
  datos_interes_noErrores$Recomendacion_Convencion <- sub("\\ .*", "",datos_interes_noErrores$Recomendacion) # Recomendacion segun convencion
  
  datos_interes_noErrores$Programa <- limpiar_categoria_docente(datos_interes_noErrores$Programa) # Se limpian las categorias de los programas
  # Se determina el departamento
  datos_interes_noErrores$Departamento <- ifelse(datos_interes_noErrores$Programa %in% c('Tecnologia En Analisis De Costos Y Presupuestos','Ingenieria Financiera Y De Negocios'),'Finanzas',
                                                 ifelse(datos_interes_noErrores$Programa %in% c('Tecnologia En Gestion Administrativa','Administracion Tecnologica'),'Ciencias Administrativas',
                                                        ifelse(datos_interes_noErrores$Programa %in% c('Tecnologia En Calidad','Tecnologia En Produccion','Ingenieria En Produccion'),'Calidad Y Produccion',
                                                               ifelse(datos_interes_noErrores$Programa %in% c('Especializacion En Finanzas','Especializacion En Formulacion Y Evaluacion De Proyectos','Maestria En Gestion De La Innovacion Tecnologica,Cooperacion Y Desarrollo Regional'),'Postgrados',''))))
  
  
  columnas_definitivas <- c('Periodo','Semestre','Estudiante','Cedula','Tnumero',
                            'Departamento','Programa','Modalidad','Titulodeltrabajo',
                            'Recomendacion_Convencion','Auxiliar3','Auxiliar4',
                            'Contratacion_Asesor','Contratacion_Evaluador') # Columnas definitivas
  datos_definitivos <- datos_interes_noErrores[,columnas_definitivas] # Datos definitivos
  # Se renombra las columnas para una mejor comprension de los datos
  colnames(datos_definitivos) <- c('Periodo','Semestre','Nombre_Estudiante','Cedula',
                                   'Grupo_ID','Departamento','Programa','Modalidad',
                                   'Titulo','Recomendacion_Convencion','Docente_Asesor',
                                   'Docente_Evaluador','Contratacion_Asesor','Contratacion_Evaluador')
  
  # Para dar codigo unico a los grupos
  datos_definitivos$Grupo_ID <- as.numeric(datos_definitivos$Grupo_ID)
  
  
  for (i in 1:nrow(datos_definitivos)) { # Se itera sobre todas  las observaciones
    print(i)
    if (i!=1){
      grupo_ID_actual <- datos_definitivos[i,'Grupo_ID']
      print(c(as.character(grupo_ID_anterior),as.character(grupo_ID_actual)))
      if(grupo_ID_anterior==grupo_ID_actual){
        grupo_ID_anterior <- datos_definitivos[i-1,'Grupo_ID']
        datos_definitivos[i,'Grupo_ID'] <- as.numeric(grupo_ID_anterior)
        grupo_ID_anterior <- grupo_ID_actual
      }else{
        grupo_ID_anterior <- datos_definitivos[i-1,'Grupo_ID']
        datos_definitivos[i,'Grupo_ID'] <- as.numeric(grupo_ID_anterior)+1
        grupo_ID_anterior <- grupo_ID_actual
      }
      
    }else{
      grupo_ID_anterior <- datos_definitivos[i,'Grupo_ID']
      datos_definitivos[i,'Grupo_ID'] <- as.numeric(grupo_ID_anterior)
    }
     
  }
  #----------------------------------------------#
  # HASTA AQUI SE LOGRO LO QUE SE TENIA PLANEADO #
  # AHORA HA QUE VERIFICAR CONSTRULLENDO ALGUNA  #
  # TABLA                                        #
  #----------------------------------------------#
  #save("datos_definitivos", file = "./Mejorado/datos_definitivos.RData")
  #load("./Mejorado/datos_definitivos.RData")
  
  # Se cambian las modalidades
  datos_definitivos$Modalidad <- limpiar_categoria_docente(datos_definitivos$Modalidad)
  print(paste('Hasta totalizador:',cont))
  datos_definitivos$Modalidad <- datos_definitivos$Modalidad %>% 
    ifelse(.%in%.[str_detect(.,"Inves")],"Procesos De Investigacion",.) %>% 
    ifelse(.=="Monografia","Proyecto De Grado",.) %>%
    ifelse(.=="Sistematizacion De Aporte Laboral","Intervencion Empresarial",.)
  
  
  # Se cambia Contratacion_Asesor 'Especial' por 'Asesor_Externo_Otro'
  datos_definitivos$Contratacion_Asesor <- ifelse(datos_definitivos$Contratacion_Asesor %in% c("Catedra","Ocasional","Sin Asesor","Carrera","Especializacion"),datos_definitivos$Contratacion_Asesor,'Asesor_Externo_Otro')
  # Se cambia 'Sin Asesor' por 'Evaluador_Externo_Otro'
  datos_definitivos$Contratacion_Evaluador <- ifelse(datos_definitivos$Contratacion_Evaluador=='Sin Asesor','Evaluador_Externo_Otro',datos_definitivos$Contratacion_Evaluador)
  
  BD_trabajos <- rbind(BD_trabajos,datos_definitivos)
  
}

BD_trabajos$Nombre_Estudiante <- limpiar_categoria_docente(BD_trabajos$Nombre_Estudiante)
BD_trabajos$Docente_Asesor <- limpiar_categoria_docente(BD_trabajos$Docente_Asesor)
BD_trabajos$Docente_Evaluador <- limpiar_categoria_docente(BD_trabajos$Docente_Evaluador)

save("BD_trabajos", file = "./Mejorado/BD_trabajos.RData")




