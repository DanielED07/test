library(dplyr)
rm(list=ls())

# "Catedra"         "Ocasional"       "Sin evaluador"      "Carrera"         "Especializacion"

load("./Mejorado/BD_trabajos.RData")

carrera_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Carrera') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Evaluador,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Carrera' = n())

ocasional_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Ocasional') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Evaluador,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Ocasional' = n())

catedra_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Catedra') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Evaluador,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Catedra' = n())


especializacion_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Especializacion') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Evaluador,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Especializacion' = n())

otro_evaluador <- BD_trabajos %>% 
  filter(Contratacion_Evaluador=='Evaluador_Externo_Otro') %>%
  distinct(Grupo_ID, .keep_all= TRUE) %>% # Se cuenta por grupos
  select(Docente_Evaluador,Modalidad,Periodo,Semestre) %>% 
  group_by(Modalidad,Periodo,Semestre) %>% 
  summarise('Evaluador_Externo_Otro' = n())

data_mod_evaluadores <- function(nombre_tipo,data_cruze){
  
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

carrera_evaluador<- data_mod_evaluadores(nombre_tipo = 'Carrera',data_cruze = carrera_evaluador)

ocasional_evaluador<- data_mod_evaluadores(nombre_tipo = 'Ocasional',data_cruze = ocasional_evaluador)

catedra_evaluador<- data_mod_evaluadores(nombre_tipo = 'Catedra',data_cruze = catedra_evaluador)

especializacion_evaluador<-data_mod_evaluadores(nombre_tipo = 'Especializacion',data_cruze = especializacion_evaluador)

otro_evaluador<- data_mod_evaluadores(nombre_tipo = 'Evaluador_Externo_Otro',data_cruze = otro_evaluador)

`Resumen de docentes evaluadores por tipo de contratacion - Por Modalidad final` <- merge(carrera_evaluador, ocasional_evaluador, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., catedra_evaluador, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., especializacion_evaluador, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>%
  merge(., otro_evaluador, by = c('Modalidad','Periodo','Semestre'), all.x = TRUE) %>%
  mutate(Total = rowSums(across(c("Catedra","Ocasional","Carrera","Especializacion",'Evaluador_Externo_Otro'))))

save("Resumen de docentes evaluadores por tipo de contratacion - Por Modalidad final", file = "./Mejorado/Querys/04_Query.RData")
`Resumen de docentes evaluadores por tipo de contratacion - Por Modalidad final`


# load("./RData/data04.RData")
# `Resumen de docentes evaluadores por tipo de contratación - Por Modalidad final` %>% 
#   filter(Periodo=='2016')
