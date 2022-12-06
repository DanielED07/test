library(dplyr)
rm(list=ls())
load("./Mejorado/BD_trabajos.RData")

n_estudiantes_por_grupo <- BD_trabajos %>% 
  group_by(Grupo_ID,Programa,Modalidad,Titulo,Departamento,Periodo,Semestre) %>% 
  summarise(Total = n()) %>% 
  select(Grupo_ID,Total,Programa,Modalidad,Titulo,Departamento,Periodo,Semestre)

cedulas_por_grupo <- c()
# temp : Temporal (Para las variables)
for (grupo in unique(BD_trabajos$Grupo_ID)){
  
  data_tem <- sapply(BD_trabajos[BD_trabajos$Grupo_ID==grupo,'Cedula'], paste, collapse=", ")
  cedulas_temp <- as.character(data_tem)
  cedulas_por_grupo <- c(cedulas_por_grupo,cedulas_temp)
}
  

grupos_y_cedulas <- data.frame('Grupo_ID'=unique(BD_trabajos$Grupo_ID),'Cedulas'=cedulas_por_grupo)

titulos_trabajos_segmentados  <- n_estudiantes_por_grupo %>% 
  merge(., grupos_y_cedulas, by = c('Grupo_ID'), all.x = TRUE) %>% 
  select(Grupo_ID,Total,Cedulas,Programa,Modalidad,Titulo,Departamento,Periodo,Semestre)

save("titulos_trabajos_segmentados", file = "./Mejorado/Querys/013_Query.RData")
titulos_trabajos_segmentados

# load("./RData/data0120.RData")
# titulos_trabajos_segmentados %>% 
#   filter(Periodo=='2016')
