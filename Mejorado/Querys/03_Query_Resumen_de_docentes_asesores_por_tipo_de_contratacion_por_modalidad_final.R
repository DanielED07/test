library(dplyr)
rm(list=ls())

# "Catedra"         "Ocasional"       "Sin Asesor"      "Carrera"         "Especializacion"

load("./Mejorado/BD_trabajos.RData")

carrera_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Carrera') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Carrera' = n())

ocasional_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Ocasional')%>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Ocasional' = n())

catedra_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Catedra') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Catedra' = n())

sin_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Sin Asesor') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Sin Asesor' = n())

especializacion_asesor <- BD_trabajos %>% 
  filter(Contratacion_Asesor=='Especializacion') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Especializacion' = n())

otro_asesor <- BD_trabajos %>% 
  filter(!(Contratacion_Asesor%in%c("Catedra","Ocasional","Sin Asesor","Carrera","Especializacion"))) %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Asesor,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Asesor_Externo_Otro' = n())



data_mod_asesores <- function(nombre_tipo,data_cruze){
  
  all_modalidades <- c('Proyecto De Grado','Practica Profesional','Procesos De Investigacion','Intervencion Empresarial','Cursos De Posgrado','Diplomado') # Todos las modalidades
  
  data <- data.frame('Modalidad'=rep(all_modalidades,13), # 11: Numero total de totalizadores
                     'Periodo'=c(rep('2016',length(all_modalidades)),
                                 rep('2017',length(all_modalidades)),
                                 rep('2017',length(all_modalidades)),
                                 rep('2018',length(all_modalidades)),
                                 rep('2018',length(all_modalidades)),
                                 rep('2019',length(all_modalidades)),
                                 rep('2019',length(all_modalidades)),
                                 rep('2020',length(all_modalidades)),
                                 rep('2020',length(all_modalidades)),
                                 rep('2021',length(all_modalidades)),
                                 rep('2021',length(all_modalidades)),
                                 rep('2022',length(all_modalidades)),
                                 rep('2022',length(all_modalidades))),
                     'Semestre' = c(rep('2',length(all_modalidades)),
                                    rep('1',length(all_modalidades)),
                                    rep('2',length(all_modalidades)),
                                    rep('1',length(all_modalidades)),
                                    rep('2',length(all_modalidades)),
                                    rep('1',length(all_modalidades)),
                                    rep('2',length(all_modalidades)),
                                    rep('1',length(all_modalidades)),
                                    rep('2',length(all_modalidades)),
                                    rep('1',length(all_modalidades)),
                                    rep('2',length(all_modalidades)),
                                    rep('1',length(all_modalidades)),
                                    rep('2',length(all_modalidades))),
                     Tipo = rep(0,length(rep(all_modalidades,13)))
  ) 
  
  data_cruzada <- merge(data, data_cruze, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>% 
    replace(is.na(.), 0) %>%  # Se remplazan NaN por 0
    select(c('Modalidad','Periodo','Semestre',nombre_tipo))
  
  return(data_cruzada)
}

#test <- data_mod_asesores(nombre_tipo = 'Ocasional',data_cruze = ocasional_asesor)

# data_nombres_modalidades_test <- data_nombres_modalidades %>% 
#   filter(Periodo=='2016')

carrera_asesor<- data_mod_asesores(nombre_tipo = 'Carrera',data_cruze = carrera_asesor)

ocasional_asesor<- data_mod_asesores(nombre_tipo = 'Ocasional',data_cruze = ocasional_asesor)

catedra_asesor<- data_mod_asesores(nombre_tipo = 'Catedra',data_cruze = catedra_asesor)

sin_asesor<- data_mod_asesores(nombre_tipo = 'Sin Asesor',data_cruze = sin_asesor)

especializacion_asesor<-data_mod_asesores(nombre_tipo = 'Especializacion',data_cruze = especializacion_asesor)

otro_asesor<- data_mod_asesores(nombre_tipo = 'Asesor_Externo_Otro',data_cruze = otro_asesor)

`Resumen de docentes asesores por tipo de contratacion - Por Modalidad final` <-  merge(carrera_asesor, ocasional_asesor, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., catedra_asesor, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., sin_asesor, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., especializacion_asesor, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., otro_asesor, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>%
  mutate(Total = rowSums(across(c("Catedra","Ocasional","Sin Asesor","Carrera","Especializacion",'Asesor_Externo_Otro'))))

save("Resumen de docentes asesores por tipo de contratacion - Por Modalidad final", file = "./Mejorado/Querys/03_Query.RData")
`Resumen de docentes asesores por tipo de contratacion - Por Modalidad final`


# load("./RData/data03.RData")
# `Resumen de docentes asesores por tipo de contratación - Por Modalidad final` %>% 
#   filter(Periodo=='2016')
